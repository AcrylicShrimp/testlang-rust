use super::super::parser::RuleItem;
use super::super::parser::RuleItemType;
use super::generator::Generator;

use std::collections::HashMap;
use std::collections::HashSet;

pub type FirstSet = HashMap<String, HashSet<String>>;

impl Generator {
	pub fn generate_first(&mut self) {
		for (rule_name, _) in &self.rule_table.rule_map {
			self.first_set.insert(rule_name.clone(), HashSet::new());
		}

		loop {
			let mut updated = false;
			for rule in &self.rule_table.rule_vec {
				match rule.first_item() {
					Some(rule_item) => {
						updated = updated
							|| Generator::update_set(&mut self.first_set, &rule.name, rule_item);
					}
					_ => (),
				}
			}

			if !updated {
				break;
			}
		}
	}

	fn update_set(first_set: &mut FirstSet, rule_name: &String, rule_item: &RuleItem) -> bool {
		let mut first_item_vec: Vec<String> = Vec::new();

		if rule_item.item_type == RuleItemType::Literal {
			first_item_vec.push(rule_item.item_content.clone());
		} else {
			for src_first_set_item in first_set.get(&rule_item.item_content).unwrap().iter() {
				first_item_vec.push(src_first_set_item.clone());
			}
		}

		let first = first_set.get_mut(rule_name).unwrap();
		let first_item_count = first.len();

		for first_item in first_item_vec {
			first.insert(first_item);
		}

		first_item_count != first.len()
	}
}
