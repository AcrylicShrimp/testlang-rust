use super::super::lexer::TokenType;
use super::super::parser::AST as RawAST;
use itertools::Itertools;

#[derive(Clone, Debug)]
pub enum ASTNode {
	Block(ASTBlockNode),
	If(ASTIfNode),
	For(ASTForNode),
	With(ASTWithNode),
	Let(ASTLetNode),
	Ret(ASTRetNode),
	Break(ASTBreakNode),
	Continue(ASTContinueNode),
	Expression(ASTExpressionNode),
}

#[derive(Clone, Debug)]
pub enum ASTType {
	Void,
	Bool,
	I8,
	I16,
	I32,
	I64,
	I128,
	U8,
	U16,
	U32,
	U64,
	U128,
	F16,
	F32,
	F64,
	String,
}

macro_rules! define_ast_node {
	($name: ident { $($field: ident : $type:ty),* $(,)* }) => {
		#[derive(Clone, Debug)]
		pub struct $name {
			// TODO: Add source file and range informations here.
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

#[derive(Clone, Debug)]
pub enum ASTIfElseNode {
	Else(ASTBlockNode),
	ElseIf(Box<ASTIfNode>),
}

define_ast_node!(ASTForNode {
	label: Option<String>,
	head: ASTForHeadNode,
	body_ast_block: ASTBlockNode,
});

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct ASTWithTemporaryNode {
	pub variable: String,
	pub expression: ASTExpressionNode,
}

define_ast_node!(ASTLetNode {
	variable: String,
	expression: Option<ASTExpressionNode>,
	ty: Option<ASTType>,
});

define_ast_node!(ASTRetNode {
	expression: Option<ASTExpressionNode>,
});

define_ast_node!(ASTBreakNode {
	label: Option<String>
});

define_ast_node!(ASTContinueNode {
	label: Option<String>
});

#[derive(Clone, Debug)]
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
	ty: ASTType,
});

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum ASTLiteralNode {
	Bool(String),
	Integer(String),
	Decimal(String),
	String(String),
}

pub fn from_raw_ast(raw_ast: &RawAST) -> Vec<ASTNode> {
	from_raw_ast_module(raw_ast)
}

fn from_raw_ast_module(raw_ast: &RawAST) -> Vec<ASTNode> {
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
		.map(|raw_ast| from_raw_ast_node(raw_ast))
		.collect::<Vec<ASTNode>>()
}

fn from_raw_ast_node(raw_ast: &RawAST) -> ASTNode {
	match raw_ast.children[0].name.as_ref() {
		"scope-statement" => ASTNode::Block(from_raw_ast_node_block(&raw_ast.children[0])),
		"if-statement" => ASTNode::If(from_raw_ast_node_if(&raw_ast.children[0])),
		"for-statement" => ASTNode::For(from_raw_ast_node_for(&raw_ast.children[0])),
		"with-statement" => ASTNode::With(from_raw_ast_node_with(&raw_ast.children[0])),
		"let-statement" => ASTNode::Let(from_raw_ast_node_let(&raw_ast.children[0])),
		"ret-statement" => ASTNode::Ret(from_raw_ast_node_ret(&raw_ast.children[0])),
		"break-statement" => ASTNode::Break(from_raw_ast_node_break(&raw_ast.children[0])),
		"continue-statement" => ASTNode::Continue(from_raw_ast_node_continue(&raw_ast.children[0])),
		"expression" => ASTNode::Expression(from_raw_ast_node_expression(&raw_ast.children[0])),
		_ => unreachable!(),
	}
}

fn from_raw_ast_node_block(raw_ast: &RawAST) -> ASTBlockNode {
	ASTBlockNode {
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

			raw_ast_node_stack
				.into_iter()
				.rev()
				.map(|raw_ast| from_raw_ast_node(raw_ast))
				.collect::<Vec<ASTNode>>()
		},
	}
}

fn from_raw_ast_node_if(raw_ast: &RawAST) -> ASTIfNode {
	ASTIfNode {
		criteria: from_raw_ast_node_expression(&raw_ast.children[1]),
		if_ast_block: from_raw_ast_node_block(&raw_ast.children[2]),
		else_ast_block: match raw_ast.children.len() {
			3 => None,
			5 => Some(match raw_ast.children[4].name.as_ref() {
				"scope-statement" => {
					ASTIfElseNode::Else(from_raw_ast_node_block(&raw_ast.children[4]))
				}
				"if-statement" => {
					ASTIfElseNode::ElseIf(Box::new(from_raw_ast_node_if(&raw_ast.children[4])))
				}
				_ => unreachable!(),
			}),
			_ => unreachable!(),
		},
	}
}

fn from_raw_ast_node_for(raw_ast: &RawAST) -> ASTForNode {
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

	ASTForNode {
		label,
		head: match raw_ast.children.len() - offset {
			2 => ASTForHeadNode::Infinite,
			3 => ASTForHeadNode::WithCriteria(from_raw_ast_node_expression(
				&raw_ast.children[offset + 1],
			)),
			5 => ASTForHeadNode::WithIterator(ASTForHeadIteratorNode {
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
				expression: from_raw_ast_node_expression(&raw_ast.children[offset + 3]),
			}),
			_ => unreachable!(),
		},
		body_ast_block: match raw_ast.children.len() - offset {
			2 => from_raw_ast_node_block(&raw_ast.children[offset + 1]),
			3 => from_raw_ast_node_block(&raw_ast.children[offset + 2]),
			5 => from_raw_ast_node_block(&raw_ast.children[offset + 4]),
			_ => unreachable!(),
		},
	}
}

fn from_raw_ast_node_with(raw_ast: &RawAST) -> ASTWithNode {
	ASTWithNode {
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
					expression: from_raw_ast_node_expression(raw_ast_expression),
				})
				.collect::<Vec<ASTWithTemporaryNode>>()
		},
		ast_block: from_raw_ast_node_block(&raw_ast.children[2]),
	}
}

fn from_raw_ast_node_let(raw_ast: &RawAST) -> ASTLetNode {
	ASTLetNode {
		variable: raw_ast.children[1]
			.child
			.as_ref()
			.unwrap()
			.token_content
			.clone(),
		expression: match raw_ast.children.len() {
			2 => None,
			4 => Some(from_raw_ast_node_expression(&raw_ast.children[3])),
			3 => None,
			5 => Some(from_raw_ast_node_expression(&raw_ast.children[4])),
			_ => unreachable!(),
		},
		ty: match raw_ast.children.len() {
			2 => None,
			4 => None,
			3 | 5 => Some(
				match raw_ast.children[2].child.as_ref().unwrap().token_type {
					TokenType::KeywordVoid => ASTType::Void,
					TokenType::KeywordBool => ASTType::Bool,
					TokenType::KeywordI8 => ASTType::I8,
					TokenType::KeywordI16 => ASTType::I16,
					TokenType::KeywordI32 => ASTType::I32,
					TokenType::KeywordI64 => ASTType::I64,
					TokenType::KeywordI128 => ASTType::I128,
					TokenType::KeywordU8 => ASTType::U8,
					TokenType::KeywordU16 => ASTType::U16,
					TokenType::KeywordU32 => ASTType::U32,
					TokenType::KeywordU64 => ASTType::U64,
					TokenType::KeywordU128 => ASTType::U128,
					TokenType::KeywordF16 => ASTType::F16,
					TokenType::KeywordF32 => ASTType::F32,
					TokenType::KeywordF64 => ASTType::F64,
					TokenType::KeywordStr => ASTType::String,
					_ => unreachable!(),
				},
			),
			_ => unreachable!(),
		},
	}
}

fn from_raw_ast_node_ret(raw_ast: &RawAST) -> ASTRetNode {
	ASTRetNode {
		expression: match raw_ast.children.len() {
			1 => None,
			2 => Some(from_raw_ast_node_expression(&raw_ast.children[1])),
			_ => unreachable!(),
		},
	}
}

fn from_raw_ast_node_break(raw_ast: &RawAST) -> ASTBreakNode {
	ASTBreakNode {
		label: match raw_ast.children.len() {
			1 => None,
			2 => Some(
				raw_ast.children[1]
					.child
					.as_ref()
					.unwrap()
					.token_content
					.clone(),
			),
			_ => unreachable!(),
		},
	}
}

fn from_raw_ast_node_continue(raw_ast: &RawAST) -> ASTContinueNode {
	ASTContinueNode {
		label: match raw_ast.children.len() {
			1 => None,
			2 => Some(
				raw_ast.children[1]
					.child
					.as_ref()
					.unwrap()
					.token_content
					.clone(),
			),
			_ => unreachable!(),
		},
	}
}

macro_rules! from_raw_ast_node_assignment {
	($name: ident -> $type: ident($raw_ast: ident)) => {
		ASTExpressionNode::$name($type {
			lhs: from_raw_ast_node_left_value(&$raw_ast.children[0]),
			rhs: Box::new(from_raw_ast_node_expression(&$raw_ast.children[2])),
			})
	};
}

macro_rules! from_raw_ast_node_unary {
	($name: ident -> $type: ident($raw_ast: ident)) => {
		ASTExpressionNode::$name($type {
			lhs: Box::new(from_raw_ast_node_expression(&$raw_ast.children[1])),
			})
	};
}

macro_rules! from_raw_ast_node_binary {
	($name: ident -> $type: ident( $raw_ast: ident)) => {
		ASTExpressionNode::$name($type {
			lhs: Box::new(from_raw_ast_node_expression(&$raw_ast.children[0])),
			rhs: Box::new(from_raw_ast_node_expression(&$raw_ast.children[2])),
			})
	};
}

fn from_raw_ast_node_expression(raw_ast: &RawAST) -> ASTExpressionNode {
	if raw_ast.children.len() == 1 && !raw_ast.children[0].is_terminal {
		return from_raw_ast_node_expression(&raw_ast.children[0]);
	}

	match raw_ast.name.as_ref() {
		"assignment" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
			TokenType::OpAssign => {
				from_raw_ast_node_assignment!(Assignment -> ASTAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignAdd => {
				from_raw_ast_node_assignment!(AdditionAssignment -> ASTAdditionAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignSub => {
				from_raw_ast_node_assignment!(SubtractionAssignment -> ASTSubtractionAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignMul => {
				from_raw_ast_node_assignment!(MultiplicationAssignment -> ASTMultiplicationAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignDiv => {
				from_raw_ast_node_assignment!(DivisionAssignment -> ASTDivisionAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignMod => {
				from_raw_ast_node_assignment!(ModuloAssignment -> ASTModuloAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignShiftL => {
				from_raw_ast_node_assignment!(ShiftLeftAssignment -> ASTShiftLeftAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignShiftR => {
				from_raw_ast_node_assignment!(ShiftRightAssignment -> ASTShiftRightAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignBitOr => {
				from_raw_ast_node_assignment!(BitOrAssignment -> ASTBitOrAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignBitAnd => {
				from_raw_ast_node_assignment!(BitAndAssignment -> ASTBitAndAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignBitXor => {
				from_raw_ast_node_assignment!(BitXorAssignment -> ASTBitXorAssignmentNode(  raw_ast))
			}
			TokenType::OpAssignBitNot => {
				from_raw_ast_node_assignment!(BitNotAssignment -> ASTBitNotAssignmentNode(  raw_ast))
			}
			_ => unreachable!(),
		},
		"op-or" => from_raw_ast_node_binary!(LogicalOr -> ASTLogicalOrNode(  raw_ast)),
		"op-and" => from_raw_ast_node_binary!(LogicalAnd -> ASTLogicalAndNode(  raw_ast)),
		"op-cmp" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
			TokenType::OpEq => from_raw_ast_node_binary!(TestEqual -> ASTTestEqualNode(  raw_ast)),
			TokenType::OpNeq => {
				from_raw_ast_node_binary!(TestNotEqual -> ASTTestNotEqualNode(  raw_ast))
			}
			TokenType::OpLs => from_raw_ast_node_binary!(TestLess -> ASTTestLessNode(  raw_ast)),
			TokenType::OpLsEq => {
				from_raw_ast_node_binary!(TestLessEqual -> ASTTestLessEqualNode(  raw_ast))
			}
			TokenType::OpGt => {
				from_raw_ast_node_binary!(TestGreater -> ASTTestGreaterNode(  raw_ast))
			}
			TokenType::OpGtEq => {
				from_raw_ast_node_binary!(TestGreaterEqual -> ASTTestGreaterEqualNode(  raw_ast))
			}
			_ => unreachable!(),
		},
		"op-addsub" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
			TokenType::OpAdd => from_raw_ast_node_binary!(Addition -> ASTAdditionNode(  raw_ast)),
			TokenType::OpSub => {
				from_raw_ast_node_binary!(Subtraction -> ASTSubtractionNode(  raw_ast))
			}
			_ => unreachable!(),
		},
		"op-muldivmod" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
			TokenType::OpMul => {
				from_raw_ast_node_binary!(Multiplication -> ASTMultiplicationNode(  raw_ast))
			}
			TokenType::OpDiv => from_raw_ast_node_binary!(Division -> ASTDivisionNode(  raw_ast)),
			TokenType::OpMod => from_raw_ast_node_binary!(Modulo -> ASTModuloNode(  raw_ast)),
			_ => unreachable!(),
		},
		"op-shift" => match raw_ast.children[1].child.as_ref().unwrap().token_type {
			TokenType::OpShiftL => {
				from_raw_ast_node_binary!(ShiftLeft -> ASTShiftLeftNode(  raw_ast))
			}
			TokenType::OpShiftR => {
				from_raw_ast_node_binary!(ShiftRight -> ASTShiftRightNode(  raw_ast))
			}
			_ => unreachable!(),
		},
		"op-bit-or" => from_raw_ast_node_binary!(BitOr -> ASTBitOrNode(  raw_ast)),
		"op-bit-and" => from_raw_ast_node_binary!(BitAnd -> ASTBitAndNode(  raw_ast)),
		"op-bit-xor" => from_raw_ast_node_binary!(BitXor -> ASTBitXorNode(  raw_ast)),
		"op-cast" => ASTExpressionNode::Cast(from_raw_ast_node_cast(raw_ast)),
		"op-single" => {
			if raw_ast.children.len() == 1 {
				from_raw_ast_node_expression(&raw_ast.children[0])
			} else {
				match raw_ast.children[0].child.as_ref().unwrap().token_type {
					TokenType::KeywordFrom => {
						ASTExpressionNode::From(from_raw_ast_node_from(raw_ast))
					}
					TokenType::ParenL => from_raw_ast_node_unary!(Paren -> ASTParenNode(  raw_ast)),
					TokenType::OpAdd => {
						from_raw_ast_node_unary!(UnaryPositive -> ASTUnaryPositiveNode(  raw_ast))
					}
					TokenType::OpSub => {
						from_raw_ast_node_unary!(UnaryNegative -> ASTUnaryNegativeNode(  raw_ast))
					}
					TokenType::OpNot => {
						from_raw_ast_node_unary!(LogicalNot -> ASTLogicalNotNode(  raw_ast))
					}
					TokenType::OpBitNot => {
						from_raw_ast_node_unary!(BitNot -> ASTBitNotNode(  raw_ast))
					}
					_ => unreachable!(),
				}
			}
		}
		"function-call" => ASTExpressionNode::Call(from_raw_ast_node_call(raw_ast)),
		"left-value" => ASTExpressionNode::LeftValue(from_raw_ast_node_left_value(raw_ast)),
		"literal" => ASTExpressionNode::Literal(from_raw_ast_node_literal(raw_ast)),
		_ => unreachable!(),
	}
}

fn from_raw_ast_node_cast(raw_ast: &RawAST) -> ASTCastNode {
	ASTCastNode {
		lhs: Box::new(from_raw_ast_node_expression(&raw_ast.children[0])),
		ty: match raw_ast.children[2].child.as_ref().unwrap().token_type {
			TokenType::KeywordVoid => ASTType::Void,
			TokenType::KeywordBool => ASTType::Bool,
			TokenType::KeywordI8 => ASTType::I8,
			TokenType::KeywordI16 => ASTType::I16,
			TokenType::KeywordI32 => ASTType::I32,
			TokenType::KeywordI64 => ASTType::I64,
			TokenType::KeywordI128 => ASTType::I128,
			TokenType::KeywordU8 => ASTType::U8,
			TokenType::KeywordU16 => ASTType::U16,
			TokenType::KeywordU32 => ASTType::U32,
			TokenType::KeywordU64 => ASTType::U64,
			TokenType::KeywordU128 => ASTType::U128,
			TokenType::KeywordF16 => ASTType::F16,
			TokenType::KeywordF32 => ASTType::F32,
			TokenType::KeywordF64 => ASTType::F64,
			TokenType::KeywordStr => ASTType::String,
			_ => unreachable!(),
		},
	}
}

fn from_raw_ast_node_from(raw_ast: &RawAST) -> ASTFromNode {
	match raw_ast.children[1].name.as_ref() {
		"scope-statement" => {
			ASTFromNode::Block(Box::new(from_raw_ast_node_block(&raw_ast.children[1])))
		}
		"if-statement" => ASTFromNode::If(Box::new(from_raw_ast_node_if(&raw_ast.children[1]))),
		"for-statement" => ASTFromNode::For(Box::new(from_raw_ast_node_for(&raw_ast.children[1]))),
		"with-statement" => {
			ASTFromNode::With(Box::new(from_raw_ast_node_with(&raw_ast.children[1])))
		}
		_ => unreachable!(),
	}
}

fn from_raw_ast_node_call(raw_ast: &RawAST) -> ASTCallNode {
	ASTCallNode {
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
					.map(|raw_ast| from_raw_ast_node_expression(raw_ast))
					.collect::<Vec<ASTExpressionNode>>()
			}
			_ => unreachable!(),
		},
	}
}

fn from_raw_ast_node_left_value(raw_ast: &RawAST) -> ASTLeftValueNode {
	ASTLeftValueNode {
		variable: raw_ast.children[0]
			.child
			.as_ref()
			.unwrap()
			.token_content
			.clone(),
	}
}

fn from_raw_ast_node_literal(raw_ast: &RawAST) -> ASTLiteralNode {
	let token = raw_ast.children[0].child.as_ref().unwrap();

	match token.token_type {
		TokenType::LiteralBool => ASTLiteralNode::Bool(token.token_content.clone()),
		TokenType::LiteralInteger => ASTLiteralNode::Integer(token.token_content.clone()),
		TokenType::LiteralDecimal => ASTLiteralNode::Decimal(token.token_content.clone()),
		TokenType::LiteralString => ASTLiteralNode::String(token.token_content.clone()),
		_ => unreachable!(),
	}
}
