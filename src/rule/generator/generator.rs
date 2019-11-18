use super::super::parser::RuleTable;
use super::action_table::ActionTable;
use super::dag::StateGraph;
use super::first::FirstSet;

pub struct Generator {
	pub rule_table: RuleTable,
	pub first_set: FirstSet,
	pub state_graph: StateGraph,
	pub action_table: ActionTable,
}

impl Generator {
	pub fn new(rule_table: RuleTable) -> Generator {
		Generator {
			rule_table: rule_table,
			first_set: FirstSet::new(),
			state_graph: StateGraph::new(),
			action_table: ActionTable::new(),
		}
	}
}
