use std::env::var;

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
    Expression(IRExpressionID),
}

#[derive(Clone, Debug, PartialEq)]
pub enum IRType {
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

impl IRType {
    pub fn from_ast_type(ty: ASTType) -> IRType {
        match ty {
            ASTType::Void => IRType::Void,
            ASTType::Bool => IRType::Bool,
            ASTType::I8 => IRType::I8,
            ASTType::I16 => IRType::I16,
            ASTType::I32 => IRType::I32,
            ASTType::I64 => IRType::I64,
            ASTType::I128 => IRType::I128,
            ASTType::U8 => IRType::U8,
            ASTType::U16 => IRType::U16,
            ASTType::U32 => IRType::U32,
            ASTType::U64 => IRType::U64,
            ASTType::U128 => IRType::U128,
            ASTType::F16 => IRType::F16,
            ASTType::F32 => IRType::F32,
            ASTType::F64 => IRType::F64,
            ASTType::String => IRType::String,
        }
    }
}

#[derive(Clone, Debug)]
pub struct IRBlock {
    pub ir_vec: Vec<IR>,
}

#[derive(Clone, Debug)]
pub struct IRIf {
    pub criteria: IRExpressionID,
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
    pub variable: IRVariableID,
    pub expression: Option<IRExpressionID>,
    pub ty: Option<IRType>,
}

#[derive(Clone, Debug)]
pub struct IRRet {
    pub expression: Option<IRExpressionID>,
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
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRAdditionAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRSubtractionAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRMultiplicationAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRDivisionAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRModuloAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRShiftLeftAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRShiftRightAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRBitOrAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRBitAndAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRBitXorAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRBitNotAssignment {
    pub lhs: IRLeftValue,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRLogicalOr {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRLogicalAnd {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRLogicalNot {
    pub lhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRTestEqual {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRTestNotEqual {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRTestLess {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRTestLessEqual {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRTestGreater {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRTestGreaterEqual {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRAddition {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRSubtraction {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRMultiplication {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRDivision {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRModulo {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRShiftLeft {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRShiftRight {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRBitOr {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRBitAnd {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRBitXor {
    pub lhs: IRExpressionID,
    pub rhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRBitNot {
    pub lhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRCast {
    pub lhs: IRExpressionID,
    pub ty: IRType,
}

#[derive(Clone, Debug)]
pub struct IRUnaryNegative {
    pub lhs: IRExpressionID,
}

#[derive(Clone, Debug)]
pub struct IRCall {
    pub function: String,
    pub argument_vec: Vec<IRExpressionID>,
}

#[derive(Clone, Debug)]
pub enum IRLeftValue {
    Identifier(IRVariableID),
}

#[derive(Clone, Debug)]
pub enum IRLiteral {
    Bool(String),
    Integer(String),
    Decimal(String),
    String(String),
}

pub type IRExpressionID = usize;
pub type IRScopeID = usize;
pub type IRVariableID = usize;

#[derive(Clone, Debug)]
pub struct UntypedIRScope {
    pub variable_vec: Vec<(String, IRVariableID)>,
    pub parent_scope: Option<IRScopeID>,
}

impl UntypedIRScope {
    pub fn new() -> UntypedIRScope {
        UntypedIRScope {
            variable_vec: Vec::new(),
            parent_scope: None,
        }
    }

    pub fn from_parent(parent_scope: IRScopeID) -> UntypedIRScope {
        UntypedIRScope {
            variable_vec: Vec::new(),
            parent_scope: Some(parent_scope),
        }
    }

    pub fn lookup_variable(&self, variable: &str) -> Option<IRVariableID> {
        self.variable_vec
            .iter()
            .rev()
            .find(|(variable_name, _)| variable_name == variable)
            .map(|(_, variable_id)| *variable_id)
    }

    pub fn new_variable(&mut self, variable: &str, variable_id: IRVariableID) {
        self.variable_vec.push((variable.to_owned(), variable_id));
    }
}

#[derive(Debug)]
pub struct UntypedIRContext {
    pub expression_vec: Vec<IRExpression>,
    pub scope_vec: Vec<UntypedIRScope>,
    pub variable_count: usize,
}

impl UntypedIRContext {
    pub fn new() -> UntypedIRContext {
        UntypedIRContext {
            expression_vec: Vec::new(),
            scope_vec: vec![UntypedIRScope {
                variable_vec: Vec::new(),
                parent_scope: None,
            }],
            variable_count: 0,
        }
    }

    pub fn new_expression(&mut self, expression: IRExpression) -> IRExpressionID {
        let expression_id = self.expression_vec.len();
        self.expression_vec.push(expression);
        expression_id
    }

    pub fn new_scope(&mut self, parent_scope: IRScopeID) -> IRScopeID {
        let scope_id = self.scope_vec.len();
        self.scope_vec.push(UntypedIRScope {
            variable_vec: Vec::new(),
            parent_scope: Some(parent_scope),
        });
        scope_id
    }

    pub fn new_variable(&mut self) -> IRVariableID {
        let variable_id = self.variable_count;
        self.variable_count += 1;
        variable_id
    }
}

#[derive(Debug)]
pub struct UntypedIRScopeStackContext {
    pub scope_stack: Vec<IRScopeID>,
}

impl UntypedIRScopeStackContext {
    pub fn new() -> UntypedIRScopeStackContext {
        UntypedIRScopeStackContext {
            scope_stack: vec![0],
        }
    }

    pub fn current(&self) -> IRScopeID {
        *self.scope_stack.last().unwrap()
    }

    pub fn new_scope(&mut self, context: &mut UntypedIRContext) -> IRScopeID {
        let scope_id = context.new_scope(*self.scope_stack.last().unwrap());
        self.scope_stack.push(scope_id);
        scope_id
    }

    pub fn remove_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn lookup_variable(
        &self,
        variable: &str,
        context: &UntypedIRContext,
    ) -> Option<IRVariableID> {
        let mut scope = &context.scope_vec[self.current()];

        loop {
            match scope.lookup_variable(variable) {
                Some(variable_id) => return Some(variable_id),
                None => match scope.parent_scope {
                    Some(parent_scope) => {
                        scope = &context.scope_vec[parent_scope];
                    }
                    None => return None,
                },
            }
        }
    }
}

pub fn from_ast(ast: &Vec<ASTNode>) -> (Vec<IR>, UntypedIRContext) {
    let mut context = UntypedIRContext::new();
    let mut scope_stack_context = UntypedIRScopeStackContext::new();

    (
        ast.iter()
            .map(|ast_node| ir_from_ast_node(ast_node, &mut context, &mut scope_stack_context))
            .collect(),
        context,
    )
}

macro_rules! make_expr {
    ($ast_expression:expr, $context:ident, $scope_stack_context:ident) => {{
        let expression =
            ir_from_ast_node_expression($ast_expression, $context, $scope_stack_context);
        $context.new_expression(expression)
    }};
}

fn ir_from_ast_node(
    ast_node: &ASTNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IR {
    match ast_node {
        ASTNode::Block(ast_block) => IR::Block(ir_from_ast_node_block(
            ast_block,
            context,
            scope_stack_context,
        )),
        ASTNode::If(ast_if) => IR::If(ir_from_ast_node_if(ast_if, context, scope_stack_context)),
        ASTNode::For(ast_for) => {
            IR::Loop(ir_from_ast_node_for(ast_for, context, scope_stack_context))
        }
        ASTNode::With(ast_with) => IR::Block(ir_from_ast_node_with(
            ast_with,
            context,
            scope_stack_context,
        )),
        ASTNode::Let(ast_let) => {
            IR::Let(ir_from_ast_node_let(ast_let, context, scope_stack_context))
        }
        ASTNode::Ret(ast_ret) => {
            IR::Ret(ir_from_ast_node_ret(ast_ret, context, scope_stack_context))
        }
        ASTNode::Break(ast_break) => IR::Break(ir_from_ast_node_break(ast_break)),
        ASTNode::Continue(ast_continue) => IR::Continue(ir_from_ast_node_continue(ast_continue)),
        ASTNode::Expression(ast_expression) => {
            IR::Expression(make_expr!(ast_expression, context, scope_stack_context))
        }
    }
}

fn ir_from_ast_node_block(
    ast_block_node: &ASTBlockNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRBlock {
    IRBlock {
        ir_vec: ast_block_node
            .ast_node_vec
            .iter()
            .map(|ast_node| ir_from_ast_node(ast_node, context, scope_stack_context))
            .collect(),
    }
}

fn ir_from_ast_node_if(
    ast_if_node: &ASTIfNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRIf {
    IRIf {
        criteria: make_expr!(&ast_if_node.criteria, context, scope_stack_context),
        if_block: ir_from_ast_node_block(&ast_if_node.if_ast_block, context, scope_stack_context),
        else_block: ast_if_node
            .else_ast_block
            .clone()
            .map(|else_ast_block| match else_ast_block {
                ASTIfElseNode::Else(block_node) => {
                    ir_from_ast_node_block(&block_node, context, scope_stack_context)
                }
                ASTIfElseNode::ElseIf(if_node) => IRBlock {
                    ir_vec: vec![IR::If(ir_from_ast_node_if(
                        &if_node,
                        context,
                        scope_stack_context,
                    ))],
                },
            }),
    }
}

fn ir_from_ast_node_for(
    ast_for_node: &ASTForNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRLoop {
    IRLoop {
        label: ast_for_node.label.clone(),
        body_block: match &ast_for_node.head {
            ASTForHeadNode::Infinite => {
                ir_from_ast_node_block(&ast_for_node.body_ast_block, context, scope_stack_context)
            }
            ASTForHeadNode::WithCriteria(criteria_node) => IRBlock {
                ir_vec: vec![IR::If(IRIf {
                    criteria: make_expr!(&criteria_node, context, scope_stack_context),
                    if_block: IRBlock {
                        ir_vec: vec![IR::Break(IRBreak { label: None })],
                    },
                    else_block: None,
                })]
                .into_iter()
                .chain(
                    ir_from_ast_node_block(
                        &ast_for_node.body_ast_block,
                        context,
                        scope_stack_context,
                    )
                    .ir_vec,
                )
                .collect(),
            },
            ASTForHeadNode::WithIterator(..) => unimplemented!(),
        },
    }
}

fn ir_from_ast_node_with(
    ast_with_node: &ASTWithNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRBlock {
    IRBlock {
        ir_vec: ast_with_node
            .temporary_vec
            .iter()
            .map(|temporary| {
                IR::Let(IRLet {
                    variable: {
                        let variable_id = context.new_variable();
                        context.scope_vec[scope_stack_context.current()]
                            .new_variable(&temporary.variable, variable_id);
                        variable_id
                    },
                    expression: Some(make_expr!(
                        &temporary.expression,
                        context,
                        scope_stack_context
                    )),
                    ty: None,
                })
            })
            .collect::<Vec<IR>>()
            .into_iter()
            .chain(
                ir_from_ast_node_block(&ast_with_node.ast_block, context, scope_stack_context)
                    .ir_vec,
            )
            .collect(),
    }
}

fn ir_from_ast_node_let(
    ast_let_node: &ASTLetNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRLet {
    IRLet {
        variable: {
            let variable_id = context.new_variable();
            context.scope_vec[scope_stack_context.current()]
                .new_variable(&ast_let_node.variable, variable_id);
            variable_id
        },
        expression: ast_let_node
            .expression
            .clone()
            .map(|expression| make_expr!(&expression, context, scope_stack_context)),
        ty: ast_let_node.ty.clone().map(|ty| IRType::from_ast_type(ty)),
    }
}

fn ir_from_ast_node_ret(
    ast_ret_node: &ASTRetNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRRet {
    IRRet {
        expression: ast_ret_node
            .expression
            .clone()
            .map(|expression| make_expr!(&expression, context, scope_stack_context)),
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

fn ir_from_ast_node_expression(
    ast_expression_node: &ASTExpressionNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRExpression {
    match ast_expression_node {
        ASTExpressionNode::Assignment(inner_node) => IRExpression::Assignment(IRAssignment {
            lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::AdditionAssignment(inner_node) => {
            IRExpression::AdditionAssignment(IRAdditionAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::SubtractionAssignment(inner_node) => {
            IRExpression::SubtractionAssignment(IRSubtractionAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::MultiplicationAssignment(inner_node) => {
            IRExpression::MultiplicationAssignment(IRMultiplicationAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::DivisionAssignment(inner_node) => {
            IRExpression::DivisionAssignment(IRDivisionAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::ModuloAssignment(inner_node) => {
            IRExpression::ModuloAssignment(IRModuloAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::ShiftLeftAssignment(inner_node) => {
            IRExpression::ShiftLeftAssignment(IRShiftLeftAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::ShiftRightAssignment(inner_node) => {
            IRExpression::ShiftRightAssignment(IRShiftRightAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::BitOrAssignment(inner_node) => {
            IRExpression::BitOrAssignment(IRBitOrAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::BitAndAssignment(inner_node) => {
            IRExpression::BitAndAssignment(IRBitAndAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::BitXorAssignment(inner_node) => {
            IRExpression::BitXorAssignment(IRBitXorAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::BitNotAssignment(inner_node) => {
            IRExpression::BitNotAssignment(IRBitNotAssignment {
                lhs: ir_from_ast_node_left_value(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::LogicalOr(inner_node) => IRExpression::LogicalOr(IRLogicalOr {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::LogicalAnd(inner_node) => IRExpression::LogicalAnd(IRLogicalAnd {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::LogicalNot(inner_node) => IRExpression::LogicalNot(IRLogicalNot {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
        }),
        ASTExpressionNode::TestEqual(inner_node) => IRExpression::TestEqual(IRTestEqual {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::TestNotEqual(inner_node) => IRExpression::TestNotEqual(IRTestNotEqual {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::TestLess(inner_node) => IRExpression::TestLess(IRTestLess {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::TestLessEqual(inner_node) => {
            IRExpression::TestLessEqual(IRTestLessEqual {
                lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::TestGreater(inner_node) => IRExpression::TestGreater(IRTestGreater {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::TestGreaterEqual(inner_node) => {
            IRExpression::TestGreaterEqual(IRTestGreaterEqual {
                lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::Addition(inner_node) => IRExpression::Addition(IRAddition {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::Subtraction(inner_node) => IRExpression::Subtraction(IRSubtraction {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::Multiplication(inner_node) => {
            IRExpression::Multiplication(IRMultiplication {
                lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
                rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::Division(inner_node) => IRExpression::Division(IRDivision {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::Modulo(inner_node) => IRExpression::Modulo(IRModulo {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::ShiftLeft(inner_node) => IRExpression::ShiftLeft(IRShiftLeft {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::ShiftRight(inner_node) => IRExpression::ShiftRight(IRShiftRight {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::BitOr(inner_node) => IRExpression::BitOr(IRBitOr {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::BitAnd(inner_node) => IRExpression::BitAnd(IRBitAnd {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::BitXor(inner_node) => IRExpression::BitXor(IRBitXor {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            rhs: make_expr!(&inner_node.rhs, context, scope_stack_context),
        }),
        ASTExpressionNode::BitNot(inner_node) => IRExpression::BitNot(IRBitNot {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
        }),
        ASTExpressionNode::Cast(inner_node) => IRExpression::Cast(IRCast {
            lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            ty: IRType::from_ast_type(inner_node.ty.clone()),
        }),
        ASTExpressionNode::From(..) => unimplemented!(),
        ASTExpressionNode::Paren(inner_node) => {
            ir_from_ast_node_expression(&inner_node.lhs, context, scope_stack_context)
        }
        ASTExpressionNode::UnaryPositive(inner_node) => {
            ir_from_ast_node_expression(&inner_node.lhs, context, scope_stack_context)
        }
        ASTExpressionNode::UnaryNegative(inner_node) => {
            IRExpression::UnaryNegative(IRUnaryNegative {
                lhs: make_expr!(&inner_node.lhs, context, scope_stack_context),
            })
        }
        ASTExpressionNode::Call(inner_node) => IRExpression::Call(ir_from_ast_node_call(
            &inner_node,
            context,
            scope_stack_context,
        )),
        ASTExpressionNode::LeftValue(inner_node) => IRExpression::LeftValue(
            ir_from_ast_node_left_value(&inner_node, context, scope_stack_context),
        ),
        ASTExpressionNode::Literal(inner_node) => {
            IRExpression::Literal(ir_from_ast_node_literal(&inner_node))
        }
    }
}

fn ir_from_ast_node_call(
    ast_call_node: &ASTCallNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRCall {
    IRCall {
        function: ast_call_node.function.clone(),
        argument_vec: ast_call_node
            .argument_vec
            .iter()
            .map(|expression| make_expr!(expression, context, scope_stack_context))
            .collect(),
    }
}

fn ir_from_ast_node_left_value(
    ast_left_value_node: &ASTLeftValueNode,
    context: &mut UntypedIRContext,
    scope_stack_context: &mut UntypedIRScopeStackContext,
) -> IRLeftValue {
    IRLeftValue::Identifier(
        scope_stack_context
            .lookup_variable(&ast_left_value_node.variable, context)
            .expect(&format!(
                "the variable {} is not defined",
                ast_left_value_node.variable
            )),
    )
}

fn ir_from_ast_node_literal(ast_literal_node: &ASTLiteralNode) -> IRLiteral {
    match ast_literal_node {
        ASTLiteralNode::Bool(literal) => IRLiteral::Bool(literal.clone()),
        ASTLiteralNode::Integer(literal) => IRLiteral::Integer(literal.clone()),
        ASTLiteralNode::Decimal(literal) => IRLiteral::Decimal(literal.clone()),
        ASTLiteralNode::String(literal) => IRLiteral::String(literal.clone()),
    }
}
