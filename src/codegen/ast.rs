use super::super::lexer::TokenType;
use super::super::parser::AST as RawAST;
use itertools::Itertools;
use std::collections::HashSet;

#[derive(Debug)]
pub struct ASTScope {
	pub index: usize,
	pub parent_index: Option<usize>,
	pub child_index_set: HashSet<usize>,
}

#[derive(Debug)]
pub struct AST {
	root_ast_node: Vec<ASTNode>,
	ast_scope_vec: Vec<ASTScope>,
}

#[derive(Debug)]
pub enum ASTNode {
	Block(ASTBlockNode),
	If(ASTIfNode),
	For(ASTForNode),
	With(ASTWithNode),
	Expression(ASTExpressionNode),
}

macro_rules! define_ast_node {
	($name: ident { $($field: ident : $type:ty),* $(,)* }) => {
		#[derive(Debug)]
		pub struct $name {
			// TODO: Add source file and range informations here.
			// TODO: Remove the scope informations here; they should be considered on lower level.
			pub scope_index: usize,
			$(pub $field: $type,)*
		}
	};
}

define_ast_node!(ASTBlockNode {
	ast_node_vec: Vec<ASTNode>,
});

define_ast_node!(ASTIfNode {
	criteria: ASTExpressionNode,
	if_ast_block: ASTBlockNode,
	else_ast_block: Option<ASTIfElseNode>,
});

#[derive(Debug)]
pub enum ASTIfElseNode {
	Else(ASTBlockNode),
	ElseIf(Box<ASTIfNode>),
}

define_ast_node!(ASTForNode {
	label: Option<String>,
	head: ASTForHeadNode,
	body_ast_block: ASTBlockNode,
});

#[derive(Debug)]
pub enum ASTForHeadNode {
	Infinite,
	WithCriteria(ASTExpressionNode),
	WithIterator(ASTForHeadIteratorNode),
}

define_ast_node!(ASTForHeadIteratorNode {
	id_vec: Vec<String>,
	expression: ASTExpressionNode,
});

define_ast_node!(ASTWithNode {
	temporary_vec: Vec<ASTWithTemporaryNode>,
	ast_block: ASTBlockNode,
});

#[derive(Debug)]
pub struct ASTWithTemporaryNode {
	variable: String,
	expression: ASTExpressionNode,
}

#[derive(Debug)]
pub enum ASTExpressionNode {
	Assignment(ASTAssignmentNode),
	AdditionAssignment(ASTAdditionAssignmentNode),
	SubtractionAssignment(ASTSubtractionAssignmentNode),
	MultiplicationAssignment(ASTMultiplicationAssignmentNode),
	DivisionAssignment(ASTDivisionAssignmentNode),
	ModuloAssignment(ASTModuloAssignmentNode),
	ShiftLeftAssignment(ASTShiftLeftAssignmentNode),
	ShiftRightAssignment(ASTShiftRightAssignmentNode),
	BitOrAssignment(ASTBitOrAssignmentNode),
	BitAndAssignment(ASTBitAndAssignmentNode),
	BitXorAssignment(ASTBitXorAssignmentNode),
	BitNotAssignment(ASTBitNotAssignmentNode),
	LogicalOr(ASTLogicalOrNode),
	LogicalAnd(ASTLogicalAndNode),
	LogicalNot(ASTLogicalNotNode),
	TestEqual(ASTTestEqualNode),
	TestNotEqual(ASTTestNotEqualNode),
	TestLess(ASTTestLessNode),
	TestLessEqual(ASTTestLessEqualNode),
	TestGreater(ASTTestGreaterNode),
	TestGreaterEqual(ASTTestGreaterEqualNode),
	Addition(ASTAdditionNode),
	Subtraction(ASTSubtractionNode),
	Multiplication(ASTMultiplicationNode),
	Division(ASTDivisionNode),
	Modulo(ASTModuloNode),
	ShiftLeft(ASTShiftLeftNode),
	ShiftRight(ASTShiftRightNode),
	BitOr(ASTBitOrNode),
	BitAnd(ASTBitAndNode),
	BitXor(ASTBitXorNode),
	BitNot(ASTBitNotNode),
	Cast(ASTCastNode),
	From(ASTFromNode),
	Paren(ASTParenNode),
	UnaryPositive(ASTUnaryPositiveNode),
	UnaryNegative(ASTUnaryNegativeNode),
	Call(ASTCallNode),
	LeftValue(ASTLeftValueNode),
	Literal(ASTLiteralNode),
}

define_ast_node!(ASTAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTAdditionAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTSubtractionAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTMultiplicationAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTDivisionAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTModuloAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTShiftLeftAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTShiftRightAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTBitOrAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTBitAndAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTBitXorAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTBitNotAssignmentNode {
	lhs: ASTLeftValueNode,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTLogicalOrNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTLogicalAndNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTLogicalNotNode {
	lhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTTestEqualNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTTestNotEqualNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTTestLessNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTTestLessEqualNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTTestGreaterNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTTestGreaterEqualNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTAdditionNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTSubtractionNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTMultiplicationNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTDivisionNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTModuloNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTShiftLeftNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTShiftRightNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTBitOrNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTBitAndNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTBitXorNode {
	lhs: Box<ASTExpressionNode>,
	rhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTBitNotNode {
	lhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTCastNode {
	lhs: Box<ASTExpressionNode>,
	// TODO: Add a type notation here.
});

#[derive(Debug)]
pub enum ASTFromNode {
	Block(Box<ASTBlockNode>),
	If(Box<ASTIfNode>),
	For(Box<ASTForNode>),
	With(Box<ASTWithNode>),
}

define_ast_node!(ASTParenNode {
	lhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTUnaryPositiveNode {
	lhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTUnaryNegativeNode {
	lhs: Box<ASTExpressionNode>,
});

define_ast_node!(ASTCallNode {
	function: String,
	argument_vec: Vec<ASTExpressionNode>
});

define_ast_node!(ASTLeftValueNode { variable: String });

#[derive(Debug)]
pub enum ASTLiteralNode {
	Bool(String),
	Integer(String),
	Decimal(String),
	String(String),
}

impl ASTScope {
	pub fn new(index: usize) -> ASTScope {
		ASTScope {
			index,
			parent_index: None,
			child_index_set: HashSet::new(),
		}
	}

	pub fn from_parent(index: usize, parent_ast_scope: &mut ASTScope) -> ASTScope {
		parent_ast_scope.child_index_set.insert(index);

		ASTScope {
			index,
			parent_index: Some(parent_ast_scope.index),
			child_index_set: HashSet::new(),
		}
	}
}

impl AST {
	pub fn from_raw_ast(raw_ast: &RawAST) -> AST {
		let mut ast = AST {
			root_ast_node: Vec::new(),
			ast_scope_vec: vec![ASTScope::new(0)],
		};

		ast.root_ast_node = AST::from_raw_ast_module(&mut ast, 0, raw_ast);

		ast
	}

	fn new_scope(&mut self, parent_scope_index: usize) -> usize {
		let ast_scope_index = self.ast_scope_vec.len();
		let ast_scope = ASTScope::from_parent(
			self.ast_scope_vec.len(),
			&mut self.ast_scope_vec[parent_scope_index],
		);

		self.ast_scope_vec.push(ast_scope);

		ast_scope_index
	}

	fn from_raw_ast_module(ast: &mut AST, scope_index: usize, raw_ast: &RawAST) -> Vec<ASTNode> {
		let mut raw_ast_node_stack = raw_ast.children[0]
			.children
			.iter()
			.rev()
			.collect::<Vec<&RawAST>>();

		while match raw_ast_node_stack.last() {
			Some(last_raw_ast) => last_raw_ast.name == "statement-list",
			None => false,
		} {
			let raw_ast_node_stack_tail = &mut raw_ast_node_stack
				.pop()
				.unwrap()
				.children
				.iter()
				.rev()
				.collect::<Vec<&RawAST>>();

			raw_ast_node_stack.append(raw_ast_node_stack_tail);
		}

		raw_ast_node_stack
			.into_iter()
			.rev()
			.map(|raw_ast| AST::from_raw_ast_node(ast, scope_index, raw_ast))
			.collect::<Vec<ASTNode>>()
	}

	fn from_raw_ast_node(ast: &mut AST, scope_index: usize, raw_ast: &RawAST) -> ASTNode {
		match raw_ast.children[0].name.as_ref() {
			"scope-statement" => ASTNode::Block(AST::from_raw_ast_node_block(
				ast,
				scope_index,
				&raw_ast.children[0],
			)),
			"if-statement" => ASTNode::If(AST::from_raw_ast_node_if(
				ast,
				scope_index,
				&raw_ast.children[0],
			)),
			"for-statement" => ASTNode::For(AST::from_raw_ast_node_for(
				ast,
				scope_index,
				&raw_ast.children[0],
			)),
			"with-statement" => ASTNode::With(AST::from_raw_ast_node_with(
				ast,
				scope_index,
				&raw_ast.children[0],
			)),
			"expression" => ASTNode::Expression(AST::from_raw_ast_node_expression(
				ast,
				scope_index,
				&raw_ast.children[0],
			)),
			_ => unreachable!(),
		}
	}

	fn from_raw_ast_node_block(
		ast: &mut AST,
		scope_index: usize,
		raw_ast: &RawAST,
	) -> ASTBlockNode {
		ASTBlockNode {
			scope_index,
			ast_node_vec: {
				let mut raw_ast_node_stack = raw_ast.children[1]
					.children
					.iter()
					.rev()
					.collect::<Vec<&RawAST>>();

				while match raw_ast_node_stack.last() {
					Some(last_raw_ast) => last_raw_ast.name == "statement-list",
					None => false,
				} {
					let raw_ast_node_stack_tail = &mut raw_ast_node_stack
						.pop()
						.unwrap()
						.children
						.iter()
						.rev()
						.collect::<Vec<&RawAST>>();

					raw_ast_node_stack.append(raw_ast_node_stack_tail);
				}

				let inner_scope_index = ast.new_scope(scope_index);

				raw_ast_node_stack
					.into_iter()
					.rev()
					.map(|raw_ast| AST::from_raw_ast_node(ast, inner_scope_index, raw_ast))
					.collect::<Vec<ASTNode>>()
			},
		}
	}

	fn from_raw_ast_node_if(ast: &mut AST, scope_index: usize, raw_ast: &RawAST) -> ASTIfNode {
		ASTIfNode {
			scope_index,
			criteria: AST::from_raw_ast_node_expression(ast, scope_index, &raw_ast.children[1]),
			if_ast_block: AST::from_raw_ast_node_block(ast, scope_index, &raw_ast.children[2]),
			else_ast_block: match raw_ast.children.len() {
				3 => None,
				5 => {
					Some(match raw_ast.children[4].name.as_ref() {
						"scope-statement" => ASTIfElseNode::Else(AST::from_raw_ast_node_block(
							ast,
							scope_index,
							&raw_ast.children[4],
						)),
						"if-statement" => ASTIfElseNode::ElseIf(Box::new(
							AST::from_raw_ast_node_if(ast, scope_index, &raw_ast.children[4]),
						)),
						_ => unreachable!(),
					})
				}
				_ => unreachable!(),
			},
		}
	}

	fn from_raw_ast_node_for(ast: &mut AST, scope_index: usize, raw_ast: &RawAST) -> ASTForNode {
		let (offset, label) =
			if raw_ast.children[0].child.as_ref().unwrap().token_type == TokenType::KeywordAs {
				(
					2,
					Some(
						raw_ast.children[1]
							.child
							.as_ref()
							.unwrap()
							.token_content
							.clone(),
					),
				)
			} else {
				(0, None)
			};

		let inner_scope_index = ast.new_scope(scope_index);

		ASTForNode {
			scope_index,
			label,
			head: match raw_ast.children.len() - offset {
				2 => ASTForHeadNode::Infinite,
				3 => ASTForHeadNode::WithCriteria(AST::from_raw_ast_node_expression(
					ast,
					inner_scope_index,
					&raw_ast.children[offset + 1],
				)),
				5 => ASTForHeadNode::WithIterator(ASTForHeadIteratorNode {
					scope_index: inner_scope_index,
					id_vec: {
						let mut raw_ast_node_stack = raw_ast.children[offset + 1]
							.children
							.iter()
							.rev()
							.collect::<Vec<&RawAST>>();

						while match raw_ast_node_stack.last() {
							Some(last_raw_ast) => last_raw_ast.name == "for-statement-id-list",
							None => false,
						} {
							let raw_ast_node_stack_tail = &mut raw_ast_node_stack
								.pop()
								.unwrap()
								.children
								.iter()
								.rev()
								.collect::<Vec<&RawAST>>();

							raw_ast_node_stack.append(raw_ast_node_stack_tail);
						}

						raw_ast_node_stack
							.into_iter()
							.rev()
							.filter(|raw_ast| raw_ast.name == "Id")
							.map(|raw_ast| raw_ast.child.as_ref().unwrap().token_content.clone())
							.collect::<Vec<String>>()
					},
					expression: AST::from_raw_ast_node_expression(
						ast,
						inner_scope_index,
						&raw_ast.children[offset + 3],
					),
				}),
				_ => unreachable!(),
			},
			body_ast_block: match raw_ast.children.len() - offset {
				2 => AST::from_raw_ast_node_block(
					ast,
					inner_scope_index,
					&raw_ast.children[offset + 1],
				),
				3 => AST::from_raw_ast_node_block(
					ast,
					inner_scope_index,
					&raw_ast.children[offset + 2],
				),
				5 => AST::from_raw_ast_node_block(
					ast,
					inner_scope_index,
					&raw_ast.children[offset + 4],
				),
				_ => unreachable!(),
			},
		}
	}
	fn from_raw_ast_node_with(ast: &mut AST, scope_index: usize, raw_ast: &RawAST) -> ASTWithNode {
		let inner_scope_index = ast.new_scope(scope_index);

		ASTWithNode {
			scope_index,
			temporary_vec: {
				let mut raw_ast_node_stack = raw_ast.children[1]
					.children
					.iter()
					.rev()
					.collect::<Vec<&RawAST>>();

				while match raw_ast_node_stack.last() {
					Some(last_raw_ast) => last_raw_ast.name == "with-statement-id-list",
					None => false,
				} {
					let raw_ast_node_stack_tail = &mut raw_ast_node_stack
						.pop()
						.unwrap()
						.children
						.iter()
						.rev()
						.collect::<Vec<&RawAST>>();

					raw_ast_node_stack.append(raw_ast_node_stack_tail);
				}

				raw_ast_node_stack
					.into_iter()
					.rev()
					.filter(|raw_ast| raw_ast.name == "expression" || raw_ast.name == "Id")
					.tuples()
					.map(|(raw_ast_id, raw_ast_expression)| ASTWithTemporaryNode {
						variable: raw_ast_id.child.as_ref().unwrap().token_content.clone(),
						expression: AST::from_raw_ast_node_expression(
							ast,
							inner_scope_index,
							raw_ast_expression,
						),
					})
					.collect::<Vec<ASTWithTemporaryNode>>()
			},
			ast_block: AST::from_raw_ast_node_block(ast, inner_scope_index, &raw_ast.children[2]),
		}
	}
}

macro_rules! from_raw_ast_node_assignment {
	($name: ident -> $type: ident($ast: ident, $scope_index: ident, $raw_ast: ident)) => {
		ASTExpressionNode::$name($type {
			$scope_index,
			lhs: AST::from_raw_ast_node_left_value($ast, $scope_index, &$raw_ast.children[0]),
			rhs: Box::new(AST::from_raw_ast_node_expression(
				$ast,
				$scope_index,
				&$raw_ast.children[2],
			)),
			})
	};
}

macro_rules! from_raw_ast_node_unary {
	($name: ident -> $type: ident($ast: ident, $scope_index: ident, $raw_ast: ident)) => {
		ASTExpressionNode::$name($type {
			$scope_index,
			lhs: Box::new(AST::from_raw_ast_node_expression(
				$ast,
				$scope_index,
				&$raw_ast.children[1],
			)),
			})
	};
}

macro_rules! from_raw_ast_node_binary {
	($name: ident -> $type: ident($ast: ident, $scope_index: ident, $raw_ast: ident)) => {
		ASTExpressionNode::$name($type {
			$scope_index,
			lhs: Box::new(AST::from_raw_ast_node_expression(
				$ast,
				$scope_index,
				&$raw_ast.children[0],
			)),
			rhs: Box::new(AST::from_raw_ast_node_expression(
				$ast,
				$scope_index,
				&$raw_ast.children[2],
			)),
			})
	};
}

impl AST {
	fn from_raw_ast_node_expression(
		ast: &mut AST,
		scope_index: usize,
		raw_ast: &RawAST,
	) -> ASTExpressionNode {
		if raw_ast.children.len() == 1 && !raw_ast.children[0].is_terminal {
			return AST::from_raw_ast_node_expression(ast, scope_index, &raw_ast.children[0]);
		}

		match raw_ast.name.as_ref() {
			"assignment" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
				TokenType::OpAssign => {
					from_raw_ast_node_assignment!(Assignment -> ASTAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignAdd => {
					from_raw_ast_node_assignment!(AdditionAssignment -> ASTAdditionAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignSub => {
					from_raw_ast_node_assignment!(SubtractionAssignment -> ASTSubtractionAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignMul => {
					from_raw_ast_node_assignment!(MultiplicationAssignment -> ASTMultiplicationAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignDiv => {
					from_raw_ast_node_assignment!(DivisionAssignment -> ASTDivisionAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignMod => {
					from_raw_ast_node_assignment!(ModuloAssignment -> ASTModuloAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignShiftL => {
					from_raw_ast_node_assignment!(ShiftLeftAssignment -> ASTShiftLeftAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignShiftR => {
					from_raw_ast_node_assignment!(ShiftRightAssignment -> ASTShiftRightAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignBitOr => {
					from_raw_ast_node_assignment!(BitOrAssignment -> ASTBitOrAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignBitAnd => {
					from_raw_ast_node_assignment!(BitAndAssignment -> ASTBitAndAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignBitXor => {
					from_raw_ast_node_assignment!(BitXorAssignment -> ASTBitXorAssignmentNode(ast, scope_index, raw_ast))
				}
				TokenType::OpAssignBitNot => {
					from_raw_ast_node_assignment!(BitNotAssignment -> ASTBitNotAssignmentNode(ast, scope_index, raw_ast))
				}
				_ => unreachable!(),
			},
			"op-or" => {
				from_raw_ast_node_binary!(LogicalOr -> ASTLogicalOrNode(ast, scope_index, raw_ast))
			}
			"op-and" => {
				from_raw_ast_node_binary!(LogicalAnd -> ASTLogicalAndNode(ast, scope_index, raw_ast))
			}
			"op-cmp" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
				TokenType::OpEq => {
					from_raw_ast_node_binary!(TestEqual -> ASTTestEqualNode(ast, scope_index, raw_ast))
				}
				TokenType::OpNeq => {
					from_raw_ast_node_binary!(TestNotEqual -> ASTTestNotEqualNode(ast, scope_index, raw_ast))
				}
				TokenType::OpLs => {
					from_raw_ast_node_binary!(TestLess -> ASTTestLessNode(ast, scope_index, raw_ast))
				}
				TokenType::OpLsEq => {
					from_raw_ast_node_binary!(TestLessEqual -> ASTTestLessEqualNode(ast, scope_index, raw_ast))
				}
				TokenType::OpGt => {
					from_raw_ast_node_binary!(TestGreater -> ASTTestGreaterNode(ast, scope_index, raw_ast))
				}
				TokenType::OpGtEq => {
					from_raw_ast_node_binary!(TestGreaterEqual -> ASTTestGreaterEqualNode(ast, scope_index, raw_ast))
				}
				_ => unreachable!(),
			},
			"op-addsub" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
				TokenType::OpAdd => {
					from_raw_ast_node_binary!(Addition -> ASTAdditionNode(ast, scope_index, raw_ast))
				}
				TokenType::OpSub => {
					from_raw_ast_node_binary!(Subtraction -> ASTSubtractionNode(ast, scope_index, raw_ast))
				}
				_ => unreachable!(),
			},
			"op-muldivmod" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
				TokenType::OpMul => {
					from_raw_ast_node_binary!(Multiplication -> ASTMultiplicationNode(ast, scope_index, raw_ast))
				}
				TokenType::OpDiv => {
					from_raw_ast_node_binary!(Division -> ASTDivisionNode(ast, scope_index, raw_ast))
				}
				TokenType::OpMod => {
					from_raw_ast_node_binary!(Modulo -> ASTModuloNode(ast, scope_index, raw_ast))
				}
				_ => unreachable!(),
			},
			"op-shift" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
				TokenType::OpShiftL => {
					from_raw_ast_node_binary!(ShiftLeft -> ASTShiftLeftNode(ast, scope_index, raw_ast))
				}
				TokenType::OpShiftR => {
					from_raw_ast_node_binary!(ShiftRight -> ASTShiftRightNode(ast, scope_index, raw_ast))
				}
				_ => unreachable!(),
			},
			"op-bit-or" => {
				from_raw_ast_node_binary!(BitOr -> ASTBitOrNode(ast, scope_index, raw_ast))
			}
			"op-bit-and" => {
				from_raw_ast_node_binary!(BitAnd -> ASTBitAndNode(ast, scope_index, raw_ast))
			}
			"op-bit-xor" => {
				from_raw_ast_node_binary!(BitXor -> ASTBitXorNode(ast, scope_index, raw_ast))
			}
			"op-cast" => {
				ASTExpressionNode::Cast(AST::from_raw_ast_node_cast(ast, scope_index, raw_ast))
			}
			"op-single" => {
				if raw_ast.children.len() == 1 {
					AST::from_raw_ast_node_expression(ast, scope_index, &raw_ast.children[0])
				} else {
					match raw_ast.children[0].child.as_ref().unwrap().token_type {
						TokenType::KeywordFrom => ASTExpressionNode::From(
							AST::from_raw_ast_node_from(ast, scope_index, raw_ast),
						),
						TokenType::ParenL => {
							from_raw_ast_node_unary!(Paren -> ASTParenNode(ast, scope_index, raw_ast))
						}
						TokenType::OpAdd => {
							from_raw_ast_node_unary!(UnaryPositive -> ASTUnaryPositiveNode(ast, scope_index, raw_ast))
						}
						TokenType::OpSub => {
							from_raw_ast_node_unary!(UnaryNegative -> ASTUnaryNegativeNode(ast, scope_index, raw_ast))
						}
						TokenType::OpNot => {
							from_raw_ast_node_unary!(LogicalNot -> ASTLogicalNotNode(ast, scope_index, raw_ast))
						}
						TokenType::OpBitNot => {
							from_raw_ast_node_unary!(BitNot -> ASTBitNotNode(ast, scope_index, raw_ast))
						}
						_ => unreachable!(),
					}
				}
			}
			"function-call" => {
				ASTExpressionNode::Call(AST::from_raw_ast_node_call(ast, scope_index, raw_ast))
			}
			"left-value" => ASTExpressionNode::LeftValue(AST::from_raw_ast_node_left_value(
				ast,
				scope_index,
				raw_ast,
			)),
			"literal" => ASTExpressionNode::Literal(AST::from_raw_ast_node_literal(
				ast,
				scope_index,
				raw_ast,
			)),
			_ => unreachable!(),
		}
	}

	fn from_raw_ast_node_cast(ast: &mut AST, scope_index: usize, raw_ast: &RawAST) -> ASTCastNode {
		ASTCastNode {
			scope_index,
			lhs: Box::new(AST::from_raw_ast_node_expression(
				ast,
				scope_index,
				&raw_ast.children[0],
			)),
		}
	}

	fn from_raw_ast_node_from(ast: &mut AST, scope_index: usize, raw_ast: &RawAST) -> ASTFromNode {
		match raw_ast.children[1].name.as_ref() {
			"scope-statement" => ASTFromNode::Block(Box::new(AST::from_raw_ast_node_block(
				ast,
				scope_index,
				&raw_ast.children[1],
			))),
			"if-statement" => ASTFromNode::If(Box::new(AST::from_raw_ast_node_if(
				ast,
				scope_index,
				&raw_ast.children[1],
			))),
			"for-statement" => ASTFromNode::For(Box::new(AST::from_raw_ast_node_for(
				ast,
				scope_index,
				&raw_ast.children[1],
			))),
			"with-statement" => ASTFromNode::With(Box::new(AST::from_raw_ast_node_with(
				ast,
				scope_index,
				&raw_ast.children[1],
			))),
			_ => unreachable!(),
		}
	}

	fn from_raw_ast_node_call(ast: &mut AST, scope_index: usize, raw_ast: &RawAST) -> ASTCallNode {
		ASTCallNode {
			scope_index,
			function: raw_ast.children[0]
				.child
				.as_ref()
				.unwrap()
				.token_content
				.clone(),
			argument_vec: match raw_ast.children.len() {
				3 => vec![],
				4 => {
					let mut raw_ast_node_stack = raw_ast.children[2]
						.children
						.iter()
						.rev()
						.collect::<Vec<&RawAST>>();

					while match raw_ast_node_stack.last() {
						Some(last_raw_ast) => last_raw_ast.name == "function-call-argument-list",
						None => false,
					} {
						let raw_ast_node_stack_tail = &mut raw_ast_node_stack
							.pop()
							.unwrap()
							.children
							.iter()
							.rev()
							.collect::<Vec<&RawAST>>();

						raw_ast_node_stack.append(raw_ast_node_stack_tail);
					}

					raw_ast_node_stack
						.into_iter()
						.rev()
						.filter(|raw_ast| raw_ast.name == "expression")
						.map(|raw_ast| AST::from_raw_ast_node_expression(ast, scope_index, raw_ast))
						.collect::<Vec<ASTExpressionNode>>()
				}
				_ => unreachable!(),
			},
		}
	}

	fn from_raw_ast_node_left_value(
		_ast: &mut AST,
		scope_index: usize,
		raw_ast: &RawAST,
	) -> ASTLeftValueNode {
		ASTLeftValueNode {
			scope_index,
			variable: raw_ast.children[0]
				.child
				.as_ref()
				.unwrap()
				.token_content
				.clone(),
		}
	}

	fn from_raw_ast_node_literal(
		_ast: &mut AST,
		_scope_index: usize,
		raw_ast: &RawAST,
	) -> ASTLiteralNode {
		let token = raw_ast.children[0].child.as_ref().unwrap();

		match token.token_type {
			TokenType::LiteralBool => ASTLiteralNode::Bool(token.token_content.clone()),
			TokenType::LiteralInteger => ASTLiteralNode::Integer(token.token_content.clone()),
			TokenType::LiteralDecimal => ASTLiteralNode::Decimal(token.token_content.clone()),
			TokenType::LiteralString => ASTLiteralNode::String(token.token_content.clone()),
			_ => unreachable!(),
		}
	}
}
