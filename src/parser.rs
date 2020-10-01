use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenType;
use crate::rule::generator::action_table::ActionTable;
use crate::rule::generator::action_table::ActionType;

use std::vec::Vec;

#[derive(Debug)]
pub struct AST {
	pub name: String,
	pub is_terminal: bool,
	pub child: Option<Token>,
	pub children: Vec<AST>,
}

impl AST {
	pub fn new(name: String, is_ternimal: bool) -> AST {
		AST {
			name: name,
			is_terminal: is_ternimal,
			child: None,
			children: Vec::new(),
		}
	}
}

enum StackItem {
	AST(AST),
	State(usize),
}

pub struct Parser {
	action_table: ActionTable,
}

impl Parser {
	pub fn new(action_table: ActionTable) -> Parser {
		Parser {
			action_table: action_table,
		}
	}

	pub fn parse(&self, content: String) -> Result<AST, String> {
		let mut state = 0;
		let mut stack: Vec<StackItem> = Vec::new();

		stack.push(StackItem::State(state));

		fn pop_ast(stack: &mut Vec<StackItem>) -> AST {
			match stack.pop() {
				Some(stack_item) => match stack_item {
					StackItem::AST(ast) => {
						return ast;
					}
					StackItem::State(_) => {
						panic!("unexpected stack item; AST expected, got State.");
					}
				},
				None => {
					panic!("empty stack");
				}
			}
		}
		fn pop_state(stack: &mut Vec<StackItem>) -> usize {
			match stack.pop() {
				Some(stack_item) => match stack_item {
					StackItem::AST(_) => {
						panic!("unexpected stack item; State expected, got AST.");
					}
					StackItem::State(state) => {
						return state;
					}
				},
				None => {
					panic!("empty stack");
				}
			}
		}
		fn peek_state(stack: &Vec<StackItem>) -> usize {
			match stack.last() {
				Some(stack_item) => match stack_item {
					StackItem::AST(_) => {
						panic!("unexpected stack item; State expected, got AST.");
					}
					StackItem::State(state) => {
						return *state;
					}
				},
				None => {
					panic!("empty stack");
				}
			}
		}

		let mut lexer = Lexer::new(content);
		let mut next_token = || loop {
			let token = lexer.next();

			if token.token_type != TokenType::Comment {
				return token;
			}
		};

		let mut token = next_token();
		let mut token_type = if token.token_type == TokenType::Eof {
			"".to_string()
		} else {
			token.token_type.to_string()
		};

		loop {
			state = peek_state(&stack);

			let action = match self
				.action_table
				.get(state)
				.unwrap()
				.literal
				.get(&token_type)
			{
				Some(action) => action,
				None => {
					return Err(format!(
						"invalid syntax; unexpected token detected: {:?}, expect {:?}",
						token,
						self.action_table
							.get(state)
							.unwrap()
							.literal
							.keys()
							.filter(|&token| !token.is_empty())
							.collect::<Vec<&String>>()
					));
				}
			};

			match action.action_type {
				ActionType::Shift => {
					let mut ast = AST::new(token_type.to_string(), true);
					ast.child = Some(token);

					stack.push(StackItem::AST(ast));
					stack.push(StackItem::State(action.next_state));

					token = next_token();
					token_type = if token.token_type == TokenType::Eof {
						"".to_string()
					} else {
						token.token_type.to_string()
					};
				}
				ActionType::Reduce => {
					let mut ast = AST::new(action.reduce_rule_name.clone(), false);

					for _ in 0..action.reduce_count {
						pop_state(&mut stack);
						ast.children.insert(0, pop_ast(&mut stack));
					}

					state = peek_state(&stack);

					stack.push(StackItem::AST(ast));
					stack.push(StackItem::State(
						self.action_table
							.get(state)
							.unwrap()
							.id
							.get(&action.reduce_rule_name)
							.unwrap()
							.next_state,
					));
				}
				ActionType::Accept => {
					pop_state(&mut stack);
					return Ok(pop_ast(&mut stack));
				}
				ActionType::Goto => {
					panic!("unexpected action item; goto is invalid action item.");
				}
			}
		}
	}
}
