extern crate serde;

use super::super::parser::RuleItemType;
use super::generator::Generator;

use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use std::vec::Vec;

#[derive(Serialize, Deserialize, PartialEq)]
pub enum ActionType {
	Shift,
	Reduce,
	Goto,
	Accept,
}

#[derive(Serialize, Deserialize, PartialEq)]
pub struct Action {
	pub action_type: ActionType,
	pub next_state: usize,
	pub reduce_count: usize,
	pub reduce_rule_name: String,
}

#[derive(Serialize, Deserialize)]
pub struct ActionMap {
	pub id: HashMap<String, Action>,
	pub literal: HashMap<String, Action>,
}

impl ActionMap {
	pub fn new() -> ActionMap {
		ActionMap {
			id: HashMap::new(),
			literal: HashMap::new(),
		}
	}
}

pub type ActionTable = Vec<ActionMap>;

impl Generator {
	pub fn generate_action_table(&mut self) {
		for _ in 0..self.state_graph.state_vec.len() {
			self.action_table.push(ActionMap::new());
		}

		for state_index in 0..self.state_graph.state_vec.len() {
			for rule in self.state_graph.state_vec[state_index].iter() {
				let src_rule = &self.rule_table.rule_vec[rule.rule_index];

				if rule.seen_index < src_rule.item_vec.len()
					&& src_rule.item_vec[rule.seen_index].item_type == RuleItemType::Literal
				{
					// Shift
					let table = &mut self.action_table[state_index].literal;
					let item = src_rule.item_vec[rule.seen_index].item_content.clone();
					let action = Action {
						action_type: ActionType::Shift,
						next_state: *self
							.state_graph
							.state_transition_map
							.get(&(
								state_index,
								src_rule.item_vec[rule.seen_index].item_content.clone(),
							))
							.unwrap(),
						reduce_count: 0,
						reduce_rule_name: "".to_string(),
					};

					if table.contains_key(&item) {
						let old_action = table.get(&item).unwrap();

						if action != *old_action {
							panic!("ambiguous syntax; shift conflict. {}", src_rule.name);
						}
					} else {
						table.insert(item, action);
					}
				} else if rule.seen_index < src_rule.item_vec.len()
					&& src_rule.item_vec[rule.seen_index].item_type == RuleItemType::Id
				{
					// Goto
					let table = &mut self.action_table[state_index].id;
					let item = src_rule.item_vec[rule.seen_index].item_content.clone();
					let action = Action {
						action_type: ActionType::Goto,
						next_state: *self
							.state_graph
							.state_transition_map
							.get(&(
								state_index,
								src_rule.item_vec[rule.seen_index].item_content.clone(),
							))
							.unwrap(),
						reduce_count: 0,
						reduce_rule_name: "".to_string(),
					};

					if table.contains_key(&item) {
						let old_action = table.get(&item).unwrap();

						if action != *old_action {
							panic!("ambiguous syntax; goto conflict. {}", src_rule.name);
						}
					} else {
						table.insert(item, action);
					}
				} else if rule.lookahead == "" && src_rule.name == "__root" {
					//Accept
					let table = &mut self.action_table[state_index].literal;
					let item = "";
					let action = Action {
						action_type: ActionType::Accept,
						next_state: 0,
						reduce_count: 0,
						reduce_rule_name: "".to_string(),
					};

					if table.contains_key(item) {
						let old_action = table.get(item).unwrap();

						if action != *old_action {
							panic!("ambiguous syntax; accept conflict. {}", src_rule.name);
						}
					} else {
						table.insert(item.to_owned(), action);
					}
				} else {
					// Reduce
					let table = &mut self.action_table[state_index].literal;
					let item = rule.lookahead.clone();
					let action = Action {
						action_type: ActionType::Reduce,
						next_state: 0,
						reduce_count: src_rule.item_vec.len(),
						reduce_rule_name: src_rule.name.clone(),
					};

					if table.contains_key(&item) {
						let old_action = table.get(&item).unwrap();

						if action != *old_action {
							panic!("ambiguous syntax; reduce conflict. {}", src_rule.name);
						}
					} else {
						table.insert(item, action);
					}
				}
			}
		}
	}
}
