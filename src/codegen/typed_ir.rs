use super::untyped_ir::*;
use crate::rule::lexer::Type;
use std::any::TypeId;
use std::collections::HashMap;
use std::collections::HashSet;
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
    Sub,
}

#[derive(Debug)]
pub enum TypeConstraint {
    Integer,
    Numeric,
}

#[derive(Debug)]
pub struct TypeInferContext {
    pub expression_map: HashMap<IRExpressionID, TypeID>,
    pub function_return_map: HashMap<IRFunctionID, TypeID>,
    pub function_parameter_map: HashMap<IRFunctionID, Vec<IRVariableID>>,
    pub variable_map: HashMap<IRVariableID, TypeID>,
    pub type_map: HashMap<TypeID, IRType>,
    pub type_constraint_vec: Vec<(TypeID, TypeConstraint)>,
    pub type_dependency_vec: Vec<(TypeID, TypeID, TypeDependency)>,
}

impl TypeInferContext {
    pub fn new() -> TypeInferContext {
        TypeInferContext {
            expression_map: HashMap::new(),
            function_return_map: HashMap::new(),
            function_parameter_map: HashMap::new(),
            variable_map: HashMap::new(),
            type_map: HashMap::new(),
            type_constraint_vec: Vec::new(),
            type_dependency_vec: Vec::new(),
        }
    }

    pub fn ty_expr(&self, expression_id: IRExpressionID) -> TypeID {
        self.expression_map[&expression_id]
    }

    pub fn ty_var(&self, variable_id: IRVariableID) -> TypeID {
        self.variable_map[&variable_id]
    }

    pub fn ty_depend_ty_equal(&mut self, lhs: TypeID, rhs: TypeID) {
        self.type_dependency_vec
            .push((lhs, rhs, TypeDependency::Same));
        self.type_dependency_vec
            .push((rhs, lhs, TypeDependency::Same));
    }

    pub fn ty_depend_ty_hierarchical(&mut self, super_ty: TypeID, sub_ty: TypeID) {
        self.type_dependency_vec
            .push((super_ty, sub_ty, TypeDependency::Super));
        self.type_dependency_vec
            .push((sub_ty, super_ty, TypeDependency::Sub));
    }

    pub fn ty_constraint(&mut self, ty: TypeID, constraint: TypeConstraint) {
        self.type_constraint_vec.push((ty, constraint));
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
                    "unable to merge the types: {:#?} and {:#?} type id: {}",
                    previous_ty, ty, type_id
                ),
            },
            None => {
                type_map.insert(type_id, ty);
                true
            }
        }
    }

    pub fn merge_subtype_into_ext(
        type_id: TypeID,
        ty: IRType,
        type_map: &mut HashMap<TypeID, IRType>,
    ) -> bool {
        match type_map.get(&type_id) {
            Some(previous_ty) => match previous_ty.super_type(ty.clone()) {
                Some(super_ty) => {
                    if ty != super_ty {
                        panic!(
                            "the type {:#?} should be a subtype of the type {:#?}",
                            previous_ty, ty
                        );
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

pub fn from_untyped_ir(
    ir_vec: &Vec<IR>,
    untyped_ir_context: &UntypedIRContext,
) -> (Vec<IRType>, Vec<(IRType, Vec<IRType>)>) {
    let mut context = TypeInferContext::new();

    let mut type_id = 0;

    for expression_id in 0..untyped_ir_context.expression_vec.len() {
        context.expression_map.insert(expression_id, type_id);
        type_id += 1;
    }

    for function_id in 0..untyped_ir_context.function_vec.len() {
        context.function_return_map.insert(function_id, type_id);
        type_id += 1;

        context.function_parameter_map.insert(
            function_id,
            untyped_ir_context.function_vec[function_id]
                .parameter_vec
                .iter()
                .map(|(_, variable_id, _)| *variable_id)
                .collect(),
        );
    }

    for variable_id in 0..untyped_ir_context.variable_count {
        context.variable_map.insert(variable_id, type_id);
        type_id += 1;
    }

    ir_vec
        .iter()
        .for_each(|ir| infer_type_ir(ir, None, untyped_ir_context, &mut context));

    let expression_map = context.expression_map;
    let variable_map = context.variable_map;
    let mut type_map = context.type_map;
    let type_constraint_vec = context.type_constraint_vec;
    let type_dependency_vec = context.type_dependency_vec;
    let function_return_map = context.function_return_map;
    let function_parameter_map = context.function_parameter_map;

    println!("function_vec: {:#?}", untyped_ir_context.function_vec);
    println!("expression_vec: {:#?}", untyped_ir_context.expression_vec);
    println!("variable_map: {:#?}", variable_map);
    println!("expression_map: {:#?}", expression_map);
    // println!("type_dependency_map: {:#?}", type_dependency_vec);

    loop {
        let mut changed = false;

        for (ty, ty_dependency, how) in type_dependency_vec.iter() {
            match how {
                TypeDependency::Same => {
                    if let Some(ir_type) = type_map.get(ty_dependency).cloned() {
                        changed = TypeInferContext::set_type_into_ext(*ty, ir_type, &mut type_map)
                            || changed;
                    }
                }
                TypeDependency::Super => {
                    if let Some(ir_type) = type_map.get(ty_dependency).cloned() {
                        changed =
                            TypeInferContext::merge_type_into_ext(*ty, ir_type, &mut type_map)
                                || changed;
                    }
                }
                TypeDependency::Sub => {
                    if let Some(ir_type) = type_map.get(ty_dependency).cloned() {
                        changed =
                            TypeInferContext::merge_subtype_into_ext(*ty, ir_type, &mut type_map)
                                || changed;
                    }
                }
            }
        }

        if !changed {
            break;
        }
    }

    for ty in 0..type_id {
        if type_map.get(&ty).is_none() {
            panic!("type id {} is not resolved; type annotation needed", ty);
        }
    }

    println!("type_map: {:#?}", type_map);

    for (ty, constraint) in type_constraint_vec.iter() {
        match constraint {
            TypeConstraint::Integer => match type_map[ty] {
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
            TypeConstraint::Numeric => match type_map[ty] {
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

    let expression_type_vec = (0..untyped_ir_context.expression_vec.len())
        .map(|index| type_map[&expression_map[&index]].clone())
        .collect();
    let function_type_vec = (0..untyped_ir_context.function_vec.len())
        .map(|index| {
            (
                type_map[&function_return_map[&index].clone()].clone(),
                function_parameter_map[&index]
                    .iter()
                    .map(|variable_id| type_map[&variable_map[&variable_id]].clone())
                    .collect(),
            )
        })
        .collect();

    (expression_type_vec, function_type_vec)
}

pub fn infer_type_ir(
    ir: &IR,
    function: Option<IRFunctionID>,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    match ir {
        IR::Block(ir_block) => infer_type_ir_block(ir_block, function, untyped_ir_context, context),
        IR::If(ir_if) => infer_type_ir_if(ir_if, function, untyped_ir_context, context),
        IR::Loop(ir_loop) => infer_type_ir_loop(ir_loop, function, untyped_ir_context, context),
        IR::Let(ir_let) => infer_type_ir_let(ir_let, untyped_ir_context, context),
        IR::Ret(ir_ret) => infer_type_ir_ret(ir_ret, function, untyped_ir_context, context),
        IR::Break(_) => {}
        IR::Continue(_) => {}
        IR::Function(ir_function) => {
            infer_type_function(*ir_function, function, untyped_ir_context, context)
        }
        IR::Expression(ir_expression) => {
            infer_type_expression(*ir_expression, untyped_ir_context, context)
        }
    }
}

fn infer_type_ir_block(
    ir_block: &IRBlock,
    function: Option<IRFunctionID>,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    ir_block
        .ir_vec
        .iter()
        .for_each(|ir| infer_type_ir(ir, function, untyped_ir_context, context));
}

fn infer_type_ir_if(
    ir_if: &IRIf,
    function: Option<IRFunctionID>,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    context.set_type_into(ir_if.criteria, IRType::Bool);
    infer_type_expression(ir_if.criteria, untyped_ir_context, context);
    infer_type_ir_block(&ir_if.if_block, function, untyped_ir_context, context);
    match &ir_if.else_block {
        Some(else_block) => infer_type_ir_block(else_block, function, untyped_ir_context, context),
        None => {}
    }
}

fn infer_type_ir_loop(
    ir_loop: &IRLoop,
    function: Option<IRFunctionID>,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    infer_type_ir_block(&ir_loop.body_block, function, untyped_ir_context, context);
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
    match ir_let.expression {
        Some(expression_id) => {
            context.ty_depend_ty_hierarchical(
                context.ty_var(ir_let.variable),
                context.ty_expr(expression_id),
            );
            infer_type_expression(expression_id, untyped_ir_context, context);
        }
        None => {}
    }
}

fn infer_type_ir_ret(
    ir_ret: &IRRet,
    function: Option<IRFunctionID>,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    match ir_ret.expression {
        Some(expression_id) => {
            context.ty_depend_ty_hierarchical(
                context.function_return_map[&function
                    .expect("ret statement with an expression is not allowed on top level")],
                context.ty_expr(expression_id),
            );
            infer_type_expression(expression_id, untyped_ir_context, context);
        }
        None => match function {
            Some(function) => {
                context.set_type_into(context.function_return_map[&function], IRType::Void)
            }
            None => {}
        },
    }
}

fn infer_type_function(
    ir_function: IRFunctionID,
    _function: Option<IRFunctionID>,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    match &untyped_ir_context.function_vec[ir_function].return_ty {
        Some(return_ty) => {
            context.set_type_into(context.function_return_map[&ir_function], return_ty.clone());
        }
        None => {}
    }

    &untyped_ir_context.function_vec[ir_function]
        .parameter_vec
        .iter()
        .for_each(|(_, variable_id, parameter_ty)| match parameter_ty {
            Some(parameter_ty) => {
                context.set_type_into(context.ty_var(*variable_id), parameter_ty.clone());
            }
            None => {}
        });

    infer_type_ir_block(
        &untyped_ir_context.function_vec[ir_function].block,
        Some(ir_function),
        untyped_ir_context,
        context,
    );
}

fn infer_type_expression(
    index: IRExpressionID,
    untyped_ir_context: &UntypedIRContext,
    context: &mut TypeInferContext,
) {
    match &untyped_ir_context.expression_vec[index] {
        IRExpression::Assignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::AdditionAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Numeric);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::SubtractionAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Numeric);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::MultiplicationAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Numeric);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::DivisionAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Numeric);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::ModuloAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Numeric);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::ShiftLeftAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Integer);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::ShiftRightAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Integer);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::BitOrAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Integer);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::BitAndAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Integer);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::BitXorAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Integer);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
                infer_type_expression(inner_ir.rhs.clone(), untyped_ir_context, context);
            }
        },
        IRExpression::BitNotAssignment(inner_ir) => match &inner_ir.lhs {
            IRLeftValue::Identifier(variable) => {
                context.set_type_into(context.expression_map[&index], IRType::Void);
                context.ty_depend_ty_hierarchical(
                    context.ty_var(*variable),
                    context.ty_expr(*&inner_ir.rhs),
                );
                context.ty_constraint(context.ty_var(*variable), TypeConstraint::Integer);
                context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
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
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.rhs));
            context.ty_depend_ty_equal(
                context.ty_expr(*&inner_ir.lhs),
                context.ty_expr(*&inner_ir.rhs),
            );
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Numeric);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Subtraction(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.rhs));
            context.ty_depend_ty_equal(
                context.ty_expr(*&inner_ir.lhs),
                context.ty_expr(*&inner_ir.rhs),
            );
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Numeric);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Multiplication(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.rhs));
            context.ty_depend_ty_equal(
                context.ty_expr(*&inner_ir.lhs),
                context.ty_expr(*&inner_ir.rhs),
            );
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Numeric);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Division(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.rhs));
            context.ty_depend_ty_equal(
                context.ty_expr(*&inner_ir.lhs),
                context.ty_expr(*&inner_ir.rhs),
            );
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Numeric);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::Modulo(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.rhs));
            context.ty_depend_ty_equal(
                context.ty_expr(*&inner_ir.lhs),
                context.ty_expr(*&inner_ir.rhs),
            );
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Numeric);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::ShiftLeft(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Integer);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::ShiftRight(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Integer);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::BitOr(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.rhs));
            context.ty_depend_ty_equal(
                context.ty_expr(*&inner_ir.lhs),
                context.ty_expr(*&inner_ir.rhs),
            );
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Integer);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::BitAnd(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.rhs));
            context.ty_depend_ty_equal(
                context.ty_expr(*&inner_ir.lhs),
                context.ty_expr(*&inner_ir.rhs),
            );
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Integer);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::BitXor(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.rhs));
            context.ty_depend_ty_equal(
                context.ty_expr(*&inner_ir.lhs),
                context.ty_expr(*&inner_ir.rhs),
            );
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Integer);
            context.ty_constraint(context.ty_expr(*&inner_ir.rhs), TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
            infer_type_expression(inner_ir.rhs, untyped_ir_context, context);
        }
        IRExpression::BitNot(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Integer);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
        }
        IRExpression::Cast(inner_ir) => {
            context.set_type_into(index, inner_ir.ty.clone());
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
        }
        IRExpression::UnaryNegative(inner_ir) => {
            context.ty_depend_ty_equal(context.ty_expr(index), context.ty_expr(*&inner_ir.lhs));
            context.ty_constraint(context.ty_expr(*&inner_ir.lhs), TypeConstraint::Numeric);
            infer_type_expression(inner_ir.lhs, untyped_ir_context, context);
        }
        IRExpression::Call(inner_ir) => {
            context.ty_depend_ty_hierarchical(
                context.ty_expr(index),
                context.function_return_map[&inner_ir.function],
            );

            let mut index = 0;

            for expression_id in inner_ir.argument_vec.iter() {
                context.ty_depend_ty_hierarchical(
                    context.ty_var(context.function_parameter_map[&inner_ir.function][index]),
                    context.expression_map[expression_id],
                );
                infer_type_expression(*expression_id, untyped_ir_context, context);

                index += 1;
            }
        }
        IRExpression::LeftValue(inner_ir) => match inner_ir {
            IRLeftValue::Identifier(variable) => {
                context.ty_depend_ty_equal(context.ty_expr(index), context.ty_var(*variable));
            }
        },
        IRExpression::Literal(inner_ir) => match inner_ir {
            IRLiteral::Bool(_) => context.set_type_into(index, IRType::Bool),
            IRLiteral::Integer(_) => context.set_type_into(index, IRType::I32),
            IRLiteral::Decimal(_) => context.set_type_into(index, IRType::U32),
            IRLiteral::String(_) => context.set_type_into(index, IRType::String),
        },
    }
}
