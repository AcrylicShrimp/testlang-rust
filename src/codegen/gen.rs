extern crate either;
extern crate inkwell;

use super::value::{Value, ValueHandler, ValueType, ValueTypeHandler};
use crate::lexer::TokenType;
use crate::parser::AST;

use either::Either;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::{Context, ContextRef};
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use std::boxed::Box;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::rc::Rc;
use std::vec::Vec;

use itertools::Itertools;

pub struct ModuleGen<'ctx> {
    pub name: String,
    pub module: Module<'ctx>,
    pub function_prototype_table: HashMap<String, FuncPrototype<'ctx>>,
}

#[derive(Clone)]
pub struct FuncPrototype<'ctx> {
    pub return_type: ValueType,
    pub param_type_vec: Vec<ValueType>,
    pub param_name_vec: Vec<String>,
    pub variadic_param: bool,
    pub function: FunctionValue<'ctx>,
}

// TODO: Remove unneeded fields.
// We need to make some ways to perform statical analysis about control flow, dead code detection, etc.
// How about the AST preprocessing?
// After the module AST fully constructed, we dive into that and collect below data:
// 1. Functions: its name, signature, and linkage types with its scope range.
// 1. Scopes - NOTE: We must allocate scopes for each depth levels.
// 1. Loop locations - We can identify loops as astElemLoop or astElemFor, so we can just 'register' them.
pub struct FuncGen<'ctx> {
    pub prototype: FuncPrototype<'ctx>,
    pub scope_stack: Vec<ScopeGen<'ctx>>, // Is there no other way to represent scopes?
                                          // pub loop_stack: Vec<astElemStmtFor>
}

pub struct BlockGen<'ctx> {
    pub block: BasicBlock,
    pub builder: Builder<'ctx>,
}

pub struct FuncBlockGen<'ctx> {
    pub func: FuncGen<'ctx>,
    pub last_block: BlockGen<'ctx>,
}

pub struct ScopeGen<'ctx> {
    pub variable_map: HashMap<String, (ValueType, PointerValue<'ctx>)>,
    pub stack_position: Option<BasicValueEnum<'ctx>>,
}

impl<'mdl, 'ctx: 'mdl> ModuleGen<'ctx> {
    pub fn new(ctx: &'ctx Context, name: &str) -> ModuleGen<'ctx> {
        let mut module = ModuleGen {
            name: name.to_owned(),
            module: ctx.create_module(name),
            function_prototype_table: HashMap::new(),
        };

        module.decl_function(ctx, "llvm.stacksave", ValueType::Str, vec![], vec![], false);
        module.decl_function(
            ctx,
            "llvm.stackrestore",
            ValueType::Void,
            vec![ValueType::Str],
            vec!["ptr".to_owned()],
            false,
        );

        module
    }

    pub fn decl_function(
        &'mdl mut self,
        ctx: &'ctx Context,
        name: &str,
        return_type: ValueType,
        param_type_vec: Vec<ValueType>,
        param_name_vec: Vec<String>,
        variadic_param: bool,
    ) -> (FuncPrototype<'ctx>, FunctionValue<'ctx>) {
        let param_basic_type_vec = param_type_vec
            .clone()
            .into_iter()
            .map(|param_type| param_type.to_basic_type(ctx))
            .collect::<Vec<BasicTypeEnum<'ctx>>>();

        let fn_type = return_type.invoke_handler(
            ctx,
            ValueTypeHandler::new()
                .handle_void(&|_, ty| ty.fn_type(param_basic_type_vec.as_slice(), variadic_param))
                .handle_bool(&|_, ty| ty.fn_type(param_basic_type_vec.as_slice(), variadic_param))
                .handle_int(&|_, ty| ty.fn_type(param_basic_type_vec.as_slice(), variadic_param))
                .handle_unsigned_int(&|_, ty| {
                    ty.fn_type(param_basic_type_vec.as_slice(), variadic_param)
                })
                .handle_float(&|_, ty| ty.fn_type(param_basic_type_vec.as_slice(), variadic_param))
                .handle_str(&|_, ty| ty.fn_type(param_basic_type_vec.as_slice(), variadic_param)),
        );

        let function = self.module.add_function(name, fn_type, None);
        let function_prototype = FuncPrototype {
            return_type: return_type,
            param_type_vec: param_type_vec,
            param_name_vec: param_name_vec,
            variadic_param: variadic_param,
            function: function.clone(),
        };

        self.function_prototype_table
            .insert(name.to_owned(), function_prototype.clone());

        (function_prototype, function)
    }

    pub fn create_function(
        &'mdl mut self,
        ctx: &'ctx Context,
        name: &str,
        return_type: ValueType,
        param_type_vec: Vec<ValueType>,
        param_name_vec: Vec<String>,
        variadic_param: bool,
    ) -> FuncBlockGen<'ctx> {
        if self.function_prototype_table.contains_key(name) {
            panic!("function {} is already exists.", name);
        }

        let function_with_prototype = self.decl_function(
            ctx,
            name,
            return_type,
            param_type_vec,
            param_name_vec,
            variadic_param,
        );
        let func_gen = FuncGen {
            prototype: function_with_prototype.0,
            scope_stack: Vec::new(),
        };
        let last_block = BlockGen::new(ctx, function_with_prototype.1, "entry");
        let mut func_block_gen = FuncBlockGen {
            func: func_gen,
            last_block,
        };

        func_block_gen
            .func
            .create_scope(self, &mut func_block_gen.last_block, false);

        for index in 0..func_block_gen.func.prototype.param_type_vec.len() {
            let variable = func_block_gen.func.create_variable(
                ctx,
                &mut func_block_gen.last_block,
                func_block_gen.func.prototype.param_name_vec[index].clone(),
                func_block_gen.func.prototype.param_type_vec[index],
            );

            Value::from_basic_value(
                func_block_gen.func.prototype.param_type_vec[index],
                func_block_gen
                    .func
                    .prototype
                    .function
                    .get_nth_param(index.try_into().unwrap())
                    .unwrap(),
            )
            .invoke_handler(
                ValueHandler::new()
                    .handle_bool(&mut |_, value| {
                        func_block_gen
                            .last_block
                            .builder
                            .build_store(variable, value);
                    })
                    .handle_int(&|_, value| {
                        func_block_gen
                            .last_block
                            .builder
                            .build_store(variable, value);
                    })
                    .handle_unsigned_int(&|_, value| {
                        func_block_gen
                            .last_block
                            .builder
                            .build_store(variable, value);
                    })
                    .handle_float(&|_, value| {
                        func_block_gen
                            .last_block
                            .builder
                            .build_store(variable, value);
                    })
                    .handle_str(&|_, value| {
                        func_block_gen
                            .last_block
                            .builder
                            .build_store(variable, value);
                    }),
            );
        }

        func_block_gen
    }
}

impl<'bdr, 'fnc: 'bdr, 'mdl: 'fnc, 'ctx: 'mdl> FuncGen<'ctx> {
    pub fn end_function(
        mut self,
        module: &'mdl mut ModuleGen<'ctx>,
        last_block: &'fnc mut BlockGen<'ctx>,
    ) {
        if self.scope_stack.len() != 1 {
            panic!("scope not yet closed.");
        }

        self.end_scope(module, last_block);

        let blocks = self.prototype.function.get_basic_blocks();

        if self.prototype.return_type == ValueType::Void
            && blocks.last().unwrap().get_terminator().is_none()
        {
            last_block.builder.build_return(None);
        }

        let pass_mgr = PassManager::<FunctionValue<'ctx>>::create(&module.module);
        pass_mgr.add_cfg_simplification_pass();
        pass_mgr.add_verifier_pass();
        pass_mgr.initialize();
        pass_mgr.run_on(&self.prototype.function);
    }

    pub fn create_scope(
        &'fnc mut self,
        module: &'mdl mut ModuleGen<'ctx>,
        last_block: &'fnc mut BlockGen<'ctx>,
        need_explicit_removal: bool,
    ) {
        self.scope_stack.push(ScopeGen {
            variable_map: HashMap::new(),
            stack_position: if need_explicit_removal {
                Some(
                    last_block
                        .builder
                        .build_call(
                            module
                                .function_prototype_table
                                .get("llvm.stacksave")
                                .expect("intrinsic function 'llvm.stacksave' not found.")
                                .function,
                            vec![].as_slice(),
                            "stacksave",
                        )
                        .try_as_basic_value()
                        .left()
                        .unwrap(),
                )
            } else {
                None
            },
        });
    }

    pub fn end_scope(
        &'fnc mut self,
        module: &'mdl mut ModuleGen<'ctx>,
        last_block: &'fnc mut BlockGen<'ctx>,
    ) {
        if self.scope_stack.last().unwrap().stack_position.is_some() {
            last_block.builder.build_call(
                module
                    .function_prototype_table
                    .get("llvm.stackrestore")
                    .expect("intrinsic function 'llvm.stackrestore' not found.")
                    .function,
                vec![self.scope_stack.pop().unwrap().stack_position.unwrap()].as_slice(),
                "stackrestore",
            );
        }

        self.scope_stack.pop();
    }

    pub fn end_scope_runtime(
        &'fnc mut self,
        module: &'mdl mut ModuleGen<'ctx>,
        last_block: &'fnc mut BlockGen<'ctx>,
        count: usize,
    ) {
        for index in 0..self.scope_stack.len() - count {
            if self.scope_stack[self.scope_stack.len() - 1 - index]
                .stack_position
                .is_some()
            {
                last_block.builder.build_call(
                    module
                        .function_prototype_table
                        .get("llvm.stackrestore")
                        .expect("intrinsic function 'llvm.stackrestore' not found.")
                        .function,
                    vec![self.scope_stack[self.scope_stack.len() - 1 - index]
                        .stack_position
                        .unwrap()]
                    .as_slice(),
                    "stackrestore RUNTIME",
                );
            }
        }
    }

    pub fn create_variable(
        &'fnc mut self,
        context: &'ctx Context,
        last_block: &'fnc mut BlockGen<'ctx>,
        name: String,
        value_type: ValueType,
    ) -> PointerValue<'ctx> {
        let scope = self.scope_stack.last_mut().expect("scope empty.");

        if scope.variable_map.contains_key(&name) {
            panic!("variable {} is already declared in this scope.", name);
        }

        let address = last_block
            .builder
            .build_alloca(value_type.to_basic_type(context), &name);

        scope
            .variable_map
            .insert(name, (value_type, address.clone()));
        address
    }

    pub fn resolve_variable(&'fnc mut self, name: &str) -> (ValueType, PointerValue<'ctx>) {
        for scope in self.scope_stack.iter().rev() {
            match scope.variable_map.get(name) {
                Some(variable) => {
                    return variable.clone();
                }
                None => (),
            }
        }
        panic!("variable {} is not declared in this scope.", name)
    }
}

impl<'ctx> BlockGen<'ctx> {
    pub fn new(context: &'ctx Context, function: FunctionValue<'ctx>, name: &str) -> Self {
        let block = context.append_basic_block(function, name);
        let builder = context.create_builder();
        builder.position_at_end(&block);

        BlockGen {
            block: block,
            builder: builder,
        }
    }

    pub fn from(context: &'ctx Context, block: BasicBlock) -> Self {
        let builder = context.create_builder();
        builder.position_at_end(&block);
        BlockGen {
            block: block,
            builder: builder,
        }
    }

    pub fn connect_to(&self, to: &Self) {
        self.builder.build_unconditional_branch(&to.block);
    }

    pub fn connect_to_if(&self, criteria: IntValue<'ctx>, to_then: &Self, to_else: &Self) {
        self.builder
            .build_conditional_branch(criteria, &to_then.block, &to_else.block);
    }
}
