use super::ast::*;

#[derive(Clone, Debug)]
pub enum IR {
	Block(IRBlock),
	If(IRIf),
	Loop(IRLoop),
	Let(IRLet),
	Ret(IRRet),
	Break(IRBreak),
	Continue(IRContinue),
	Expression(IRExpression),
}

#[derive(Clone, Debug)]
pub struct IRBlock {
	pub ir_vec: Vec<IR>,
}

#[derive(Clone, Debug)]
pub struct IRIf {
	pub criteria: IRExpression,
	pub if_block: IRBlock,
	pub else_block: Option<IRBlock>,
}

#[derive(Clone, Debug)]
pub struct IRLoop {
	pub label: Option<String>,
	pub body_block: IRBlock,
}

#[derive(Clone, Debug)]
pub struct IRLet {
	pub name: String,
	pub expression: Option<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRRet {
	pub expression: Option<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBreak {
	pub label: Option<String>,
}

#[derive(Clone, Debug)]
pub struct IRContinue {
	pub label: Option<String>,
}

#[derive(Clone, Debug)]
pub enum IRExpression {
	Assignment(IRAssignment),
	AdditionAssignment(IRAdditionAssignment),
	SubtractionAssignment(IRSubtractionAssignment),
	MultiplicationAssignment(IRMultiplicationAssignment),
	DivisionAssignment(IRDivisionAssignment),
	ModuloAssignment(IRModuloAssignment),
	ShiftLeftAssignment(IRShiftLeftAssignment),
	ShiftRightAssignment(IRShiftRightAssignment),
	BitOrAssignment(IRBitOrAssignment),
	BitAndAssignment(IRBitAndAssignment),
	BitXorAssignment(IRBitXorAssignment),
	BitNotAssignment(IRBitNotAssignment),
	LogicalOr(IRLogicalOr),
	LogicalAnd(IRLogicalAnd),
	LogicalNot(IRLogicalNot),
	TestEqual(IRTestEqual),
	TestNotEqual(IRTestNotEqual),
	TestLess(IRTestLess),
	TestLessEqual(IRTestLessEqual),
	TestGreater(IRTestGreater),
	TestGreaterEqual(IRTestGreaterEqual),
	Addition(IRAddition),
	Subtraction(IRSubtraction),
	Multiplication(IRMultiplication),
	Division(IRDivision),
	Modulo(IRModulo),
	ShiftLeft(IRShiftLeft),
	ShiftRight(IRShiftRight),
	BitOr(IRBitOr),
	BitAnd(IRBitAnd),
	BitXor(IRBitXor),
	BitNot(IRBitNot),
	Cast(IRCast),
	UnaryNegative(IRUnaryNegative),
	Call(IRCall),
	LeftValue(IRLeftValue),
	Literal(IRLiteral),
}

#[derive(Clone, Debug)]
pub struct IRAssignment {
	pub lhs: IRLeftValue,
	pub rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRAdditionAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRSubtractionAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRMultiplicationAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRDivisionAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRModuloAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRShiftLeftAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRShiftRightAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBitOrAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBitAndAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBitXorAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBitNotAssignment {
	lhs: IRLeftValue,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRLogicalOr {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRLogicalAnd {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRLogicalNot {
	lhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRTestEqual {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRTestNotEqual {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRTestLess {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRTestLessEqual {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRTestGreater {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRTestGreaterEqual {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRAddition {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRSubtraction {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRMultiplication {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRDivision {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRModulo {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRShiftLeft {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRShiftRight {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBitOr {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBitAnd {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBitXor {
	lhs: Box<IRExpression>,
	rhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRBitNot {
	lhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRCast {
	lhs: Box<IRExpression>,
	// TODO: Add a type notation here.
}

#[derive(Clone, Debug)]
pub struct IRUnaryNegative {
	lhs: Box<IRExpression>,
}

#[derive(Clone, Debug)]
pub struct IRCall {
	function: String,
	argument_vec: Vec<IRExpression>,
}

#[derive(Clone, Debug)]
pub enum IRLeftValue {
	Identifier(IRIdentifier),
}

#[derive(Clone, Debug)]
pub struct IRIdentifier {
	name: String,
}

#[derive(Clone, Debug)]
pub enum IRLiteral {
	Bool(String),
	Integer(String),
	Decimal(String),
	String(String),
}

pub fn from_ast(ast: &AST) -> Vec<IR> {
	ast.root_ast_node
		.iter()
		.map(|ast_node| ir_from_ast_node(ast_node))
		.collect()
}

fn ir_from_ast_node(ast_node: &ASTNode) -> IR {
	match ast_node {
		ASTNode::Block(ast_block) => IR::Block(ir_from_ast_node_block(ast_block)),
		ASTNode::If(ast_if) => IR::If(ir_from_ast_node_if(ast_if)),
		ASTNode::For(ast_for) => IR::Loop(ir_from_ast_node_for(ast_for)),
		ASTNode::With(ast_with) => IR::Block(ir_from_ast_node_with(ast_with)),
		ASTNode::Let(ast_let) => IR::Let(ir_from_ast_node_let(ast_let)),
		ASTNode::Ret(ast_ret) => IR::Ret(ir_from_ast_node_ret(ast_ret)),
		ASTNode::Break(ast_break) => IR::Break(ir_from_ast_node_break(ast_break)),
		ASTNode::Continue(ast_continue) => IR::Continue(ir_from_ast_node_continue(ast_continue)),
		ASTNode::Expression(ast_expression) => {
			IR::Expression(ir_from_ast_node_expression(ast_expression))
		}
	}
}

fn ir_from_ast_node_block(ast_block_node: &ASTBlockNode) -> IRBlock {
	IRBlock {
		ir_vec: ast_block_node
			.ast_node_vec
			.iter()
			.map(|ast_node| ir_from_ast_node(ast_node))
			.collect(),
	}
}

fn ir_from_ast_node_if(ast_if_node: &ASTIfNode) -> IRIf {
	IRIf {
		criteria: ir_from_ast_node_expression(&ast_if_node.criteria),
		if_block: ir_from_ast_node_block(&ast_if_node.if_ast_block),
		else_block: ast_if_node
			.else_ast_block
			.clone()
			.map(|else_ast_block| match else_ast_block {
				ASTIfElseNode::Else(block_node) => ir_from_ast_node_block(&block_node),
				ASTIfElseNode::ElseIf(if_node) => IRBlock {
					ir_vec: vec![IR::If(ir_from_ast_node_if(&if_node))],
				},
			}),
	}
}

fn ir_from_ast_node_for(ast_for_node: &ASTForNode) -> IRLoop {
	IRLoop {
		label: ast_for_node.label.clone(),
		body_block: match &ast_for_node.head {
			ASTForHeadNode::Infinite => ir_from_ast_node_block(&ast_for_node.body_ast_block),
			ASTForHeadNode::WithCriteria(criteria_node) => IRBlock {
				ir_vec: vec![IR::If(IRIf {
					criteria: ir_from_ast_node_expression(&criteria_node),
					if_block: IRBlock {
						ir_vec: vec![IR::Break(IRBreak { label: None })],
					},
					else_block: None,
				})]
				.into_iter()
				.chain(ir_from_ast_node_block(&ast_for_node.body_ast_block).ir_vec)
				.collect(),
			},
			ASTForHeadNode::WithIterator(..) => unimplemented!(),
		},
	}
}

fn ir_from_ast_node_with(ast_with_node: &ASTWithNode) -> IRBlock {
	IRBlock {
		ir_vec: ast_with_node
			.temporary_vec
			.iter()
			.map(|temporary| {
				IR::Let(IRLet {
					name: temporary.variable.clone(),
					expression: Some(ir_from_ast_node_expression(&temporary.expression)),
				})
			})
			.chain(ir_from_ast_node_block(&ast_with_node.ast_block).ir_vec)
			.collect(),
	}
}

fn ir_from_ast_node_let(ast_let_node: &ASTLetNode) -> IRLet {
	IRLet {
		name: ast_let_node.variable.clone(),
		expression: ast_let_node
			.expression
			.clone()
			.map(|expression| ir_from_ast_node_expression(&expression)),
	}
}

fn ir_from_ast_node_ret(ast_ret_node: &ASTRetNode) -> IRRet {
	IRRet {
		expression: ast_ret_node
			.expression
			.clone()
			.map(|expression| ir_from_ast_node_expression(&expression)),
	}
}

fn ir_from_ast_node_break(ast_break_node: &ASTBreakNode) -> IRBreak {
	IRBreak {
		label: ast_break_node.label.clone(),
	}
}

fn ir_from_ast_node_continue(ast_continue_node: &ASTContinueNode) -> IRContinue {
	IRContinue {
		label: ast_continue_node.label.clone(),
	}
}

fn ir_from_ast_node_expression(ast_expression_node: &ASTExpressionNode) -> IRExpression {
	match ast_expression_node {
		ASTExpressionNode::Assignment(inner_node) => IRExpression::Assignment(IRAssignment {
			lhs: ir_from_ast_node_left_value(&inner_node.lhs),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::AdditionAssignment(inner_node) => {
			IRExpression::AdditionAssignment(IRAdditionAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::SubtractionAssignment(inner_node) => {
			IRExpression::SubtractionAssignment(IRSubtractionAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::MultiplicationAssignment(inner_node) => {
			IRExpression::MultiplicationAssignment(IRMultiplicationAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::DivisionAssignment(inner_node) => {
			IRExpression::DivisionAssignment(IRDivisionAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::ModuloAssignment(inner_node) => {
			IRExpression::ModuloAssignment(IRModuloAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::ShiftLeftAssignment(inner_node) => {
			IRExpression::ShiftLeftAssignment(IRShiftLeftAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::ShiftRightAssignment(inner_node) => {
			IRExpression::ShiftRightAssignment(IRShiftRightAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::BitOrAssignment(inner_node) => {
			IRExpression::BitOrAssignment(IRBitOrAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::BitAndAssignment(inner_node) => {
			IRExpression::BitAndAssignment(IRBitAndAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::BitXorAssignment(inner_node) => {
			IRExpression::BitXorAssignment(IRBitXorAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::BitNotAssignment(inner_node) => {
			IRExpression::BitNotAssignment(IRBitNotAssignment {
				lhs: ir_from_ast_node_left_value(&inner_node.lhs),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::LogicalOr(inner_node) => IRExpression::LogicalOr(IRLogicalOr {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::LogicalAnd(inner_node) => IRExpression::LogicalAnd(IRLogicalAnd {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::LogicalNot(inner_node) => IRExpression::LogicalNot(IRLogicalNot {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
		}),
		ASTExpressionNode::TestEqual(inner_node) => IRExpression::TestEqual(IRTestEqual {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::TestNotEqual(inner_node) => IRExpression::TestNotEqual(IRTestNotEqual {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::TestLess(inner_node) => IRExpression::TestLess(IRTestLess {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::TestLessEqual(inner_node) => {
			IRExpression::TestLessEqual(IRTestLessEqual {
				lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::TestGreater(inner_node) => IRExpression::TestGreater(IRTestGreater {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::TestGreaterEqual(inner_node) => {
			IRExpression::TestGreaterEqual(IRTestGreaterEqual {
				lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::Addition(inner_node) => IRExpression::Addition(IRAddition {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::Subtraction(inner_node) => IRExpression::Subtraction(IRSubtraction {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::Multiplication(inner_node) => {
			IRExpression::Multiplication(IRMultiplication {
				lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
				rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
			})
		}
		ASTExpressionNode::Division(inner_node) => IRExpression::Division(IRDivision {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::Modulo(inner_node) => IRExpression::Modulo(IRModulo {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::ShiftLeft(inner_node) => IRExpression::ShiftLeft(IRShiftLeft {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::ShiftRight(inner_node) => IRExpression::ShiftRight(IRShiftRight {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::BitOr(inner_node) => IRExpression::BitOr(IRBitOr {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::BitAnd(inner_node) => IRExpression::BitAnd(IRBitAnd {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::BitXor(inner_node) => IRExpression::BitXor(IRBitXor {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			rhs: Box::new(ir_from_ast_node_expression(&inner_node.rhs)),
		}),
		ASTExpressionNode::BitNot(inner_node) => IRExpression::BitNot(IRBitNot {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
		}),
		ASTExpressionNode::Cast(inner_node) => IRExpression::Cast(IRCast {
			lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
		}),
		ASTExpressionNode::From(..) => unimplemented!(),
		ASTExpressionNode::Paren(inner_node) => ir_from_ast_node_expression(&inner_node.lhs),
		ASTExpressionNode::UnaryPositive(inner_node) => {
			ir_from_ast_node_expression(&inner_node.lhs)
		}
		ASTExpressionNode::UnaryNegative(inner_node) => {
			IRExpression::UnaryNegative(IRUnaryNegative {
				lhs: Box::new(ir_from_ast_node_expression(&inner_node.lhs)),
			})
		}
		ASTExpressionNode::Call(inner_node) => {
			IRExpression::Call(ir_from_ast_node_call(&inner_node))
		}
		ASTExpressionNode::LeftValue(inner_node) => {
			IRExpression::LeftValue(ir_from_ast_node_left_value(&inner_node))
		}
		ASTExpressionNode::Literal(inner_node) => {
			IRExpression::Literal(ir_from_ast_node_literal(&inner_node))
		}
	}
}

fn ir_from_ast_node_call(ast_call_node: &ASTCallNode) -> IRCall {
	IRCall {
		function: ast_call_node.function.clone(),
		argument_vec: ast_call_node
			.argument_vec
			.iter()
			.map(|expression| ir_from_ast_node_expression(expression))
			.collect(),
	}
}

fn ir_from_ast_node_left_value(ast_left_value_node: &ASTLeftValueNode) -> IRLeftValue {
	IRLeftValue::Identifier(IRIdentifier {
		name: ast_left_value_node.variable.clone(),
	})
}

fn ir_from_ast_node_literal(ast_literal_node: &ASTLiteralNode) -> IRLiteral {
	match ast_literal_node {
		ASTLiteralNode::Bool(literal) => IRLiteral::Bool(literal.clone()),
		ASTLiteralNode::Integer(literal) => IRLiteral::Integer(literal.clone()),
		ASTLiteralNode::Decimal(literal) => IRLiteral::Decimal(literal.clone()),
		ASTLiteralNode::String(literal) => IRLiteral::String(literal.clone()),
	}
}
