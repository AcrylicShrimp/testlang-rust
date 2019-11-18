use std::collections::HashMap;
use std::vec::Vec;

use super::lexer::Lexer;
use super::lexer::Type;

#[derive(Debug, PartialEq)]
pub enum RuleItemType {
	Id,
	Literal,
}

#[derive(Debug)]
pub struct RuleItem {
	pub item_type: RuleItemType,
	pub item_content: String,
}

struct PartialRule {
	pub name: String,
	pub item_vec: Vec<RuleItem>,
}

#[derive(Debug)]
pub struct Rule {
	pub index: usize,
	pub name: String,
	pub item_vec: Vec<RuleItem>,
}

impl Rule {
	pub fn first_item(&self) -> Option<&RuleItem> {
		self.item_vec.first()
	}
}

#[derive(Debug)]
pub struct RuleTable {
	pub rule_vec: Vec<Rule>,
	pub rule_map: HashMap<String, Vec<usize>>,
}

impl RuleTable {
	pub fn new() -> RuleTable {
		RuleTable {
			rule_vec: Vec::new(),
			rule_map: HashMap::new(),
		}
	}

	fn add_rule(&mut self, partial_rule: PartialRule) {
		if !self.rule_map.contains_key(&partial_rule.name) {
			self.rule_map.insert(partial_rule.name.clone(), Vec::new());
		}

		let rule_index = self.rule_vec.len();
		self.rule_map
			.get_mut(&partial_rule.name)
			.unwrap()
			.push(rule_index);
		self.rule_vec.push(Rule {
			index: rule_index,
			name: partial_rule.name,
			item_vec: partial_rule.item_vec,
		});
	}
}

#[derive(Debug)]
pub struct Parser {
	lexer: Lexer,
}

impl Parser {
	pub fn new(content: String) -> Parser {
		Parser {
			lexer: Lexer::new(content),
		}
	}

	pub fn parse(&mut self) -> RuleTable {
		let mut rule_table = RuleTable::new();

		loop {
			match self.next() {
				Some(partial_rule) => {
					rule_table.add_rule(partial_rule);
				}
				_ => {
					break;
				}
			};
		}

		rule_table
	}

	fn next(&mut self) -> Option<PartialRule> {
		let name = self.lexer.next();

		if name.token_type == Type::EoF {
			return None;
		}
		if name.token_type != Type::Literal {
			panic!(
				"literal expected, got [{:?}: {}]",
				name.token_type, name.token_content
			);
		}

		let colon = self.lexer.next();

		if colon.token_type != Type::Colon {
			panic!(
				"colon expected, got [{:?}: {}]",
				colon.token_type, colon.token_content
			);
		}

		let mut partial_rule = PartialRule {
			name: name.token_content,
			item_vec: Vec::new(),
		};

		let first_rule_item = self.lexer.next();

		if first_rule_item.token_type != Type::Id && first_rule_item.token_type != Type::Literal {
			panic!(
				"id or literal expected, got [{:?}: {}]",
				first_rule_item.token_type, first_rule_item.token_content
			);
		}

		partial_rule.item_vec.push(RuleItem {
			item_type: if first_rule_item.token_type == Type::Id {
				RuleItemType::Id
			} else {
				RuleItemType::Literal
			},
			item_content: first_rule_item.token_content,
		});

		loop {
			let rule_item = self.lexer.next();

			if rule_item.token_type != Type::Id
				&& rule_item.token_type != Type::Literal
				&& rule_item.token_type != Type::Semicolon
			{
				panic!(
					"id or literal or semicolon expected, got [{:?}: {}]",
					rule_item.token_type, rule_item.token_content
				);
			}

			if rule_item.token_type == Type::Semicolon {
				break;
			}

			partial_rule.item_vec.push(RuleItem {
				item_type: if rule_item.token_type == Type::Id {
					RuleItemType::Id
				} else {
					RuleItemType::Literal
				},
				item_content: rule_item.token_content,
			});
		}

		Some(partial_rule)
	}
}
