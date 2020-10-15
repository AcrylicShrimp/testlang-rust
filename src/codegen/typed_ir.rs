use super::untyped_ir::*;
use crate::rule::lexer::Type;
use std::any::TypeId;
use std::collections::HashMap;
use std::env::var;

trait SuperIRType {
    fn super_type(&self, ty: IRType) -> Option<IRType>;
}

impl SuperIRType for IRType {
    fn super_type(&self, ty: IRType) -> Option<IRType> {
        match self {
            IRType::Void => match ty {
                IRType::Void => Some(IRType::Void),
                _ => None,
            },
            IRType::Bool => match ty {
                IRType::Bool => Some(IRType::Bool),
                _ => None,
            },
            IRType::I8 => match ty {
                IRType::I8 | IRType::I16 | IRType::I32 | IRType::I64 | IRType::I128 => Some(ty),
                _ => None,
            },
            IRType::I16 => match ty {
                IRType::I8 => Some(IRType::I16),
                IRType::I16 | IRType::I32 | IRType::I64 | IRType::I128 => Some(ty),
                _ => None,
            },
            IRType::I32 => match ty {
                IRType::I8 | IRType::I16 => Some(IRType::I32),
                IRType::I32 | IRType::I64 | IRType::I128 => Some(ty),
                _ => None,
            },
            IRType::I64 => match ty {
                IRType::I8 | IRType::I16 | IRType::I32 => Some(IRType::I64),
                IRType::I64 | IRType::I128 => Some(ty),
                _ => None,
            },
            IRType::I128 => match ty {
                IRType::I8 | IRType::I16 | IRType::I32 | IRType::I64 | IRType::I128 => {
                    Some(IRType::I128)
                }
                _ => None,
            },
            IRType::U8 => match ty {
                IRType::U8 | IRType::U16 | IRType::U32 | IRType::U64 | IRType::U128 => Some(ty),
                _ => None,
            },
            IRType::U16 => match ty {
                IRType::U8 => Some(IRType::U16),
                IRType::U16 | IRType::U32 | IRType::U64 | IRType::U128 => Some(ty),
                _ => None,
            },
            IRType::U32 => match ty {
                IRType::U8 | IRType::U16 => Some(IRType::U32),
                IRType::U32 | IRType::U64 | IRType::U128 => Some(ty),
                _ => None,
            },
            IRType::U64 => match ty {
                IRType::U8 | IRType::U16 | IRType::U32 => Some(IRType::U64),
                IRType::U64 | IRType::U128 => Some(ty),
                _ => None,
            },
            IRType::U128 => match ty {
                IRType::U8 | IRType::U16 | IRType::U32 | IRType::U64 | IRType::U128 => {
                    Some(IRType::U128)
                }
                _ => None,
            },
            IRType::F16 => match ty {
                IRType::F16 | IRType::F32 | IRType::F64 => Some(ty),
                _ => None,
            },
            IRType::F32 => match ty {
                IRType::F16 => Some(IRType::F32),
                IRType::F32 | IRType::F64 => Some(ty),
                _ => None,
            },
            IRType::F64 => match ty {
                IRType::F16 | IRType::F32 | IRType::F64 => Some(IRType::F64),
                _ => None,
            },
            IRType::String => match ty {
                IRType::String => Some(IRType::String),
                _ => None,
            },
        }
    }
}

// pub struct TypedIRType {
//     pub index: usize,
//     pub state: TypedIRTypeState,
// }
//
// pub enum TypedIRTypeState {
//     Resolved(IRType),
//     // Unresolved(Vec<TypedIRTypeRestriction>),
// }

// pub struct TypedIRTypes {
//     pub type_vec: Vec<TypedIRType>,
//     pub alias_map: HashMap<usize, Option<IRType>>,
// }

// T1, T2, T3, T4, ...
// T1 = ?, T2 = ?, T3 = ?, T4 = ?, ...
// T1 = T2, T3 = T4, T4 = T2, ...

pub type TypeID = usize;

#[derive(Debug)]
pub enum TypeDependency {
    Same,
    Super,
}

#[derive(Debug)]
pub enum TypeConstraint {
    Integer,
    Numeric,
}

#[derive(Debug)]
pub struct TypeInferContext {
    pub expression_map: HashMap<IRExpressionID, TypeID>,
    pub variable_map: HashMap<IRVariableID, TypeID>,
    pub type_map: HashMap<TypeID, IRType>,
    pub type_constraint_map: HashMap<TypeID, Vec<TypeConstraint>>,
    pub type_dependency_map: HashMap<TypeID, Vec<(TypeID, TypeDependency)>>,
}

impl TypeInferContext {
    pub fn new() -> TypeInferContext {
        TypeInferContext {
            expression_map: HashMap::new(),
            variable_map: HashMap::new(),
            type_map: HashMap::new(),
            type_constraint_map: HashMap::new(),
            type_dependency_map: HashMap::new(),
        }
    }

    pub fn expr_depend_on_ty(
        &mut self,
        expression_id: &IRExpressionID,
        dependency_ty: &TypeID,
        how: TypeDependency,
    ) {
        self.type_dependency_map
            .get_mut(dependency_ty)
            .unwrap()
            .push((self.expression_map[expression_id], how));
    }

    pub fn expr_depend_on_expr(
        &mut self,
        expression_id: &IRExpressionID,
        dependency_ty: &IRExpressionID,
        how: TypeDependency,
    ) {
        self.type_dependency_map
            .get_mut(&self.expression_map[dependency_ty])
            .unwrap()
            .push((self.expression_map[expression_id], how));
    }

    pub fn expr_depend_on_var(
        &mut self,
        expression_id: &IRExpressionID,
        dependency_ty: &IRVariableID,
        how: TypeDependency,
    ) {
        self.type_dependency_map
            .get_mut(&self.variable_map[dependency_ty])
            .unwrap()
            .push((self.expression_map[expression_id], how));
    }

    pub fn var_depend_on_ty(
        &mut self,
        variable_id: &IRVariableID,
        dependency_ty: &TypeID,
        how: TypeDependency,
    ) {
        self.type_dependency_map
            .get_mut(dependency_ty)
            .unwrap()
            .push((self.variable_map[variable_id], how));
    }

    pub fn var_depend_on_expr(
        &mut self,
        variable_id: &IRVariableID,
        dependency_ty: &IRExpressionID,
        how: TypeDependency,
    ) {
        self.type_dependency_map
            .get_mut(&self.expression_map[dependency_ty])
            .unwrap()
            .push((self.variable_map[variable_id], how));
    }

    pub fn var_depend_on_var(
        &mut self,
        variable_id: &IRVariableID,
        dependency_ty: &IRVariableID,
        how: TypeDependency,
    ) {
        self.type_dependency_map
            .get_mut(&self.variable_map[dependency_ty])
            .unwrap()
            .push((self.variable_map[variable_id], how));
    }

    pub fn constraint_ty(&mut self, ty: &TypeID, constraint: TypeConstraint) {
        self.type_constraint_map
            .get_mut(ty)
            .unwrap()
            .push(constraint);
    }

    pub fn constraint_expr(&mut self, expression_id: &IRExpressionID, constraint: TypeConstraint) {
        self.type_constraint_map
            .get_mut(&self.expression_map[expression_id])
            .unwrap()
            .push(constraint);
    }

    pub fn constraint_var(&mut self, variable_id: &IRVariableID, constraint: TypeConstraint) {
        self.type_constraint_map
            .get_mut(&self.variable_map[variable_id])
            .unwrap()
            .push(constraint);
    }

    pub fn set_type_into(&mut self, type_id: TypeID, ty: IRType) {
        TypeInferContext::set_type_into_ext(type_id, ty, &mut self.type_map);
    }

    pub fn merge_type_into(&mut self, type_id: TypeID, ty: IRType) {
        TypeInferContext::merge_type_into_ext(type_id, ty, &mut self.type_map);
    }

    pub fn set_type_into_ext(
        type_id: TypeID,
        ty: IRType,
        type_map: &mut HashMap<TypeID, IRType>,
    ) -> bool {
        match type_map.get(&type_id) {
            Some(previous_ty) => {
                if *previous_ty != ty {
                    panic!(
                        "unable to make the types same: {:#?} and {:#?}",
                        previous_ty, ty
                    );
                }
                false
            }
            None => {
                type_map.insert(type_id, ty);
                true
            }
        }
    }

    pub fn merge_type_into_ext(
        type_id: TypeID,
        ty: IRType,
        type_map: &mut HashMap<TypeID, IRType>,
    ) -> bool {
        match type_map.get_mut(&type_id) {
            Some(previous_ty) => match previous_ty.super_type(ty.clone()) {
                Some(super_ty) => {
                    if *previous_ty != super_ty {
                        *previous_ty = super_ty;
                        true
                    } else {
                        false
                    }
                }
                None => panic!(
                    "unable to merge the types: {:#?} and {:#?}",
                    previous_ty, ty
                ),
            },
            None => {
                type_map.insert(type_id, ty);
                true
            }
        }
    }
}

pub fn test(ir_vec: &Vec<IR>, untyped_ir_context: &UntypedIRContext) {
    let mut context = TypeInferContext::new();

    let mut type_id = 0;

    for expression_id in 0..untyped_ir_context.expression_vec.len() {
        context.expression_map.insert(expression_id, type_id);
        type_id += 1;
    }

    for variable_id in 0..untyped_ir_context.variable_count {
        context.variable_map.insert(variable_id, type_id);
        type_id += 1;
    }

    for ty in 0..type_id {
        context.type_constraint_map.insert(ty, Vec::new());
        context.type_dependency_map.insert(ty, Vec::new());
    }

    ir_vec
        .iter()
        .for_each(|ir| infer_type_ir(ir, untyped_ir_context, &mut context));

    let expression_map = context.expression_map;
    let variable_map = context.variable_map;
    let mut type_map = context.type_map;
    let type_constraint_map = context.type_constraint_map;
    let type_dependency_map = context.type_dependency_map;

    loop {
        let mut changed = false;

        for ty in 0..type_id {
            let ir_type = type_map.get(&ty).cloned();

            if let Some(ir_type) = ir_type {
                for (target_id, deps) in type_dependency_map[&ty].iter() {
                    match deps {
                        TypeDependency::Same => {
                            if TypeInferContext::set_type_into_ext(
                                *target_id,
                                ir_type.clone(),
                                &mut type_map,
                            ) {
                                changed = true;
                            }
                        }
                        TypeDependency::Super => {
                            if TypeInferContext::merge_type_into_ext(
                                *target_id,
                                ir_type.clone(),
                                &mut type_map,
                            ) {
                                changed = true;
                            }
                        }
                    }
                }
            }
        }

        if !changed {
            break;
        }
    }

    println!("expression_vec: {:#?}", untyped_ir_context.expression_vec);
    println!("expression_map: {:#?}", expression_map);
    println!("variable_map: {:#?}", variable_map);
    println!("type_map: {:#?}", type_map);
    println!("type_dependency_map: {:#?}", type_dependency_map);

    for ty in 0..type_id {
        if type_map.get(&ty).is_none() {
            panic!("type id {} is not resolved; type annotation needed", ty);
        }
    }

    for ty in 0..type_id {
        for constraint in type_constraint_map[&ty].iter() {
            match constraint {
                TypeConstraint::Integer => match type_map[&ty] {
                    IRType::I8
                    | IRType::I16
                    | IRType::I32
                    | IRType::I64
                    | IRType::I128
                    | IRType::U8
                    | IRType::U16
                    | IRType::U32
                    | IRType::U64
                    | IRType::U128 => {}
                    _ => panic!("type id {} should be a integer", ty),
                },
                TypeConstraint::Numeric => match type_map[&ty] {
                    IRType::I8
                    | IRType::I16
                    | IRType::I32
                    | IRType::I64
                    | IRType::I128
                    | IRType::U8
                    | IRType::U16
                    | IRType::U32
                    | IRType::U64
                    | IRType::U128
                    | IRType::F16
                    | IRType::F32
                    | IRType::F64 => {}
                    _ => panic!("type id {} should be a numeric", ty),
                },
            }
        }
    }
}

pub fn infer_type_ir(
    ir: &IR,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    match ir {
        IR::Block(ir_block) => infer_type_ir_block(ir_block, untyped_ir_context, context),
        IR::If(ir_if) => infer_type_ir_if(ir_if, untyped_ir_context, context),
        IR::Loop(ir_loop) => infer_type_ir_loop(ir_loop, untyped_ir_context, context),
        IR::Let(ir_let) => infer_type_ir_let(ir_let, untyped_ir_context, context),
        IR::Ret(_) => unimplemented!(),
        IR::Break(_) => {}
        IR::Continue(_) => {}
        IR::Expression(ir_expression) => {
            infer_type_expression(*ir_expression, untyped_ir_context, context)
        }
    }
}

fn infer_type_ir_block(
    ir_block: &IRBlock,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    ir_block
        .ir_vec
        .iter()
        .for_each(|ir| infer_type_ir(ir, untyped_ir_context, context));
}

fn infer_type_ir_if(
    ir_if: &IRIf,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    context.set_type_into(ir_if.criteria, IRType::Bool);
    infer_type_expression(ir_if.criteria, untyped_ir_context, context);
    infer_type_ir_block(&ir_if.if_block, untyped_ir_context, context);
    match &ir_if.else_block {
        Some(else_block) => infer_type_ir_block(else_block, untyped_ir_context, context),
        None => {}
    }
}

fn infer_type_ir_loop(
    ir_loop: &IRLoop,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    infer_type_ir_block(&ir_loop.body_block, untyped_ir_context, context);
}

fn infer_type_ir_let(
    ir_let: &IRLet,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    match &ir_let.ty {
        Some(ty) => context.set_type_into(context.variable_map[&ir_let.variable], ty.clone()),
        None => {}
    }
    match &ir_let.expression {
        Some(expression) => {
            context.var_depend_on_expr(&ir_let.variable, expression, TypeDependency::Super);
            infer_type_expression(*expression, untyped_ir_context, context);
        }
        None => {}
    }
}

fn infer_type_expression(
    index: usize,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    match &untyped_ir_context.expression_vec[index] {
        IRExpression::Assignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::AdditionAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Numeric);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::SubtractionAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Numeric);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::MultiplicationAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Numeric);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::DivisionAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Numeric);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::ModuloAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Numeric);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::ShiftLeftAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                // TODO: Add integer constraints to the both operands.
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Integer);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::ShiftRightAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                // TODO: Add integer constraints to the both operands.
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Integer);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::BitOrAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                // TODO: Add integer constraints to the both operands.
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Integer);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::BitAndAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                // TODO: Add integer constraints to the both operands.
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Integer);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::BitXorAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                // TODO: Add integer constraints to the both operands.
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Integer);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::BitNotAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                // TODO: Add integer constraints to the both operands.
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.var_depend_on_expr(variable, &inner_ir.rhs, TypeDependency::Super);
                context.constraint_var(variable, TypeConstraint::Integer);
                context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::LogicalOr(inner_ir) => {
            context.set_type_into(index, IRType::Bool);
            context.set_type_into(inner_ir.lhs, IRType::Bool);
            context.set_type_into(inner_ir.rhs, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::LogicalAnd(inner_ir) => {
            context.set_type_into(index, IRType::Bool);
            context.set_type_into(inner_ir.lhs, IRType::Bool);
            context.set_type_into(inner_ir.rhs, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::LogicalNot(inner_ir) => {
            context.set_type_into(index, IRType::Bool);
            context.set_type_into(inner_ir.lhs, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
        }
        IRExpression::TestEqual(inner_ir) => {
            context.merge_type_into(index, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::TestNotEqual(inner_ir) => {
            context.merge_type_into(index, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::TestLess(inner_ir) => {
            context.merge_type_into(index, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::TestLessEqual(inner_ir) => {
            context.merge_type_into(index, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::TestGreater(inner_ir) => {
            context.merge_type_into(index, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::TestGreaterEqual(inner_ir) => {
            context.merge_type_into(index, IRType::Bool);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Addition(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.expr_depend_on_expr(&index, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.lhs, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.rhs, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Numeric);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Subtraction(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.expr_depend_on_expr(&index, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.lhs, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.rhs, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Numeric);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Multiplication(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.expr_depend_on_expr(&index, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.lhs, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.rhs, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Numeric);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Division(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.expr_depend_on_expr(&index, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.lhs, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.rhs, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Numeric);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Modulo(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.expr_depend_on_expr(&index, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.lhs, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.rhs, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Numeric);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::ShiftLeft(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Integer);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::ShiftRight(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Integer);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::BitOr(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.expr_depend_on_expr(&index, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.lhs, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.rhs, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Integer);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::BitAnd(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.expr_depend_on_expr(&index, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.lhs, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.rhs, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Integer);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::BitXor(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.expr_depend_on_expr(&index, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.lhs, &inner_ir.rhs, TypeDependency::Same);
            context.expr_depend_on_expr(&inner_ir.rhs, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Integer);
            context.constraint_expr(&inner_ir.rhs, TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::BitNot(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            context.constraint_expr(&inner_ir.lhs, TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
        }
        IRExpression::Cast(inner_ir) => {
            context.set_type_into(index, inner_ir.ty.clone());
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
        }
        IRExpression::UnaryNegative(inner_ir) => {
            context.expr_depend_on_expr(&index, &inner_ir.lhs, TypeDependency::Same);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
        }
        IRExpression::Call(inner_ir) => {
            // TODO: Set its type as a return type of the function.
            context.set_type_into(context.expression_map[&index], IRType::Void);
            for expression_id in inner_ir.argument_vec.iter() {
                infer_type_expression(expression_id.clone(), untyped_ir_context, context);
            }
        }
        IRExpression::LeftValue(inner_ir) => match inner_ir {
            IRLeftValue::Identifier(variable) => {
                context.expr_depend_on_var(&index, variable, TypeDependency::Same);
                context.var_depend_on_expr(variable, &index, TypeDependency::Same);
            }
        },
        IRExpression::Literal(inner_ir) => match inner_ir {
            IRLiteral::Bool(_) => context.merge_type_into(index, IRType::Bool),
            IRLiteral::Integer(_) => context.merge_type_into(index, IRType::I32),
            IRLiteral::Decimal(_) => context.merge_type_into(index, IRType::U32),
            IRLiteral::String(_) => context.merge_type_into(index, IRType::String),
        },
    }
}
