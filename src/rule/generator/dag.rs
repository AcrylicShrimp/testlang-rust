extern crate num_cpus;
extern crate scoped_threadpool;

use super::super::parser::RuleItemType;
use super::generator::Generator;

use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::convert::TryInto;
use std::sync::mpsc::channel;
use std::sync::Arc;
use std::vec::Vec;

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ARule {
	pub rule_index: usize,
	pub seen_index: usize,
	pub lookahead: String,
}

impl ARule {
	pub fn new(rule_index: usize, lookahead: String) -> ARule {
		ARule {
			rule_index: rule_index,
			seen_index: 0,
			lookahead: lookahead,
		}
	}
}

pub type State = BTreeSet<ARule>;
pub type StateHandle = Arc<State>;
pub type StateTransitionSource = (usize, String);

#[derive(Debug)]
pub struct StateGraph {
	pub state_vec: Vec<StateHandle>,
	pub state_map: HashMap<StateHandle, usize>,
	pub state_transition_map: HashMap<StateTransitionSource, usize>,
}

impl StateGraph {
	pub fn new() -> StateGraph {
		StateGraph {
			state_vec: Vec::new(),
			state_map: HashMap::new(),
			state_transition_map: HashMap::new(),
		}
	}

	pub fn add_state(&mut self, state_handle: StateHandle) -> usize {
		match self.state_map.entry(state_handle.clone()) {
			Entry::Occupied(index) => *index.get(),
			Entry::Vacant(index) => {
				let state_index = self.state_vec.len();

				index.insert(state_index);
				self.state_vec.push(state_handle);

				state_index
			}
		}
	}
}

impl Generator {
	fn closure(&self, state: &mut State) {
		loop {
			let size = state.len();
			let mut rule_vec = Vec::new();

			for rule in state.iter() {
				let src_rule = &self.rule_table.rule_vec[rule.rule_index];

				if src_rule.item_vec.len() <= rule.seen_index {
					continue;
				}

				let marker_item = &src_rule.item_vec[rule.seen_index];

				if marker_item.item_type != RuleItemType::Id {
					continue;
				}

				let mut lookahead_set = HashSet::new();

				if rule.seen_index + 1 < src_rule.item_vec.len() {
					let lookahead_rule_item = &src_rule.item_vec[rule.seen_index + 1];

					if lookahead_rule_item.item_type == RuleItemType::Literal {
						lookahead_set.insert(lookahead_rule_item.item_content.clone());
					} else {
						let src_first_set = &self.first_set[&lookahead_rule_item.item_content];

						if src_first_set.is_empty() {
							lookahead_set.insert("".to_string());
						} else {
							for first in src_first_set {
								lookahead_set.insert(first.clone());
							}
						}
					}
				} else {
					lookahead_set.insert("".to_string());
				}

				if lookahead_set.len() == 1 && lookahead_set.contains("") {
					lookahead_set = HashSet::new();
					lookahead_set.insert(rule.lookahead.clone());
				}

				for rule_index in &self.rule_table.rule_map[&marker_item.item_content] {
					for lookahead in lookahead_set.iter() {
						rule_vec.push(ARule::new(*rule_index, lookahead.clone()));
					}
				}
			}

			for rule in rule_vec {
				state.insert(rule);
			}

			if size == state.len() {
				break;
			}
		}
	}

	fn closure_rule(&self, rule: ARule) -> State {
		let mut state = State::new();
		state.insert(rule);

		self.closure(&mut state);

		state
	}

	fn goto(&self, state: &State, label: &String) -> Option<StateHandle> {
		let mut result_state = State::new();

		for rule in state.iter() {
			let src_rule = &self.rule_table.rule_vec[rule.rule_index];

			if src_rule.item_vec.len() <= rule.seen_index {
				continue;
			}

			if src_rule.item_vec[rule.seen_index].item_content != *label {
				continue;
			}

			result_state.insert(ARule {
				rule_index: rule.rule_index,
				seen_index: rule.seen_index + 1,
				lookahead: rule.lookahead.clone(),
			});
		}

		if result_state.is_empty() {
			None
		} else {
			self.closure(&mut result_state);
			Some(StateHandle::new(result_state))
		}
	}

	pub fn generate_dag(&mut self) {
		if !self.rule_table.rule_map.contains_key("__root") {
			panic!("no root nonterminal found");
		}

		if self.rule_table.rule_map["__root"].len() != 1 {
			panic!("multiple root nonterminal found");
		}

		let mut state_index_vec: Vec<usize> =
			vec![self
				.state_graph
				.add_state(StateHandle::new(self.closure_rule(ARule::new(
					self.rule_table.rule_map["__root"][0],
					"".to_string(),
				))))];

		let mut thread_pool = scoped_threadpool::Pool::new(num_cpus::get().try_into().unwrap());
		let (sender, receiver) = channel::<Vec<(usize, String, StateHandle, Option<usize>)>>();

		while !state_index_vec.is_empty() {
			thread_pool.scoped(|scoped| {
				for index in 0..state_index_vec.len() {
					let this = &*self;
					let thread_sender = sender.clone();
					let state_index = state_index_vec[index];

					scoped.execute(move || {
						match thread_sender.send(
							this.state_graph.state_vec[state_index]
								.iter()
								.filter(|rule| {
									rule.seen_index
										< this.rule_table.rule_vec[rule.rule_index].item_vec.len()
								})
								.map(|rule| {
									let state = this.goto(
										&this.state_graph.state_vec[state_index],
										&this.rule_table.rule_vec[rule.rule_index].item_vec
											[rule.seen_index]
											.item_content,
									);
									let previous_state_index = if state.is_some() {
										match this
											.state_graph
											.state_map
											.get(state.as_ref().unwrap())
										{
											Some(state_index) => Some(*state_index),
											None => None,
										}
									} else {
										None
									};

									(
										state_index,
										this.rule_table.rule_vec[rule.rule_index].item_vec
											[rule.seen_index]
											.item_content
											.clone(),
										state,
										previous_state_index,
									)
								})
								.filter(|transition| transition.2.is_some())
								.map(|transition| {
									(
										transition.0,
										transition.1,
										transition.2.unwrap(),
										transition.3,
									)
								})
								.collect(),
						) {
							Ok(_) => (),
							Err(_) => panic!(),
						};
					});
				}
			});

			state_index_vec = receiver
				.iter()
				.take(state_index_vec.len())
				.flatten()
				.map(|transition| {
					let state_index = match transition.3 {
						Some(state_index) => state_index,
						None => self.state_graph.add_state(transition.2),
					};

					self.state_graph
						.state_transition_map
						.insert((transition.0, transition.1), state_index);

					match transition.3 {
						Some(_) => None,
						None => Some(state_index),
					}
				})
				.flatten()
				.unique()
				.collect();
		}
	}
}
