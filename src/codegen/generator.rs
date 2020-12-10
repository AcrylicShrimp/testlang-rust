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

pub struct Generator {
    pub context: Context,
}

pub struct ModuleGen<'ctx> {
    pub name: String,
    pub module: Module<'ctx>,
    pub context: &'ctx Context,
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

pub struct FuncGen<'ctx> {
    pub function: FunctionValue<'ctx>,
    pub prototype: FuncPrototype<'ctx>,
    pub scope_stack: Vec<ScopeGen<'ctx>>,
    pub loop_label_stack: Vec<(String, usize)>,
    pub loop_statement_entry_stack: Vec<BasicBlock<'ctx>>,
    pub loop_statement_exit_stack: Vec<BasicBlock<'ctx>>,
    pub loop_statement_scopesize_stack: Vec<usize>,
    pub last_block: BlockGen<'ctx>,
}

pub struct BlockGen<'ctx> {
    pub block: BasicBlock<'ctx>,
    pub builder: Builder<'ctx>,
}

pub struct ScopeGen<'ctx> {
    pub variable_map: HashMap<String, (ValueType, PointerValue<'ctx>)>,
    pub stack_position: Option<BasicValueEnum<'ctx>>,
}

impl<'ctx> Generator {
    pub fn new() -> Generator {
        Generator {
            context: Context::create(),
        }
    }

    pub fn create_module(&'ctx mut self, name: &str) -> ModuleGen<'ctx> {
        let mut module = ModuleGen {
            name: name.to_owned(),
            module: self.context.create_module(name),
            context: &self.context,
            function_prototype_table: HashMap::new(),
        };

        // LLVM Intrinsic functions.
        module.decl_function("llvm.stacksave", ValueType::Str, vec![], vec![], false);
        module.decl_function(
            "llvm.stackrestore",
            ValueType::Void,
            vec![ValueType::Str],
            vec!["ptr".to_owned()],
            false,
        );

        module
    }
}

impl<'fnc, 'mdl: 'fnc, 'ctx: 'mdl> ModuleGen<'ctx> {
    pub fn decl_function(
        &'mdl mut self,
        name: &str,
        return_type: ValueType,
        param_type_vec: Vec<ValueType>,
        param_name_vec: Vec<String>,
        variadic_param: bool,
    ) -> (FuncPrototype<'ctx>, FunctionValue<'ctx>) {
        let param_basic_type_vec = param_type_vec
            .clone()
            .into_iter()
            .map(|param_type| param_type.to_basic_type(self.context))
            .collect::<Vec<BasicTypeEnum<'ctx>>>();

        let fn_type = return_type.invoke_handler(
            self.context,
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
        name: &str,
        return_type: ValueType,
        param_type_vec: Vec<ValueType>,
        param_name_vec: Vec<String>,
        variadic_param: bool,
    ) -> FuncGen<'ctx> {
        if self.function_prototype_table.contains_key(name) {
            panic!("function {} is already exists.", name);
        }

        let function_with_prototype = self.decl_function(
            name,
            return_type,
            param_type_vec,
            param_name_vec,
            variadic_param,
        );
        let mut func_gen = FuncGen {
            function: function_with_prototype.1,
            prototype: function_with_prototype.0,
            scope_stack: Vec::new(),
            loop_label_stack: Vec::new(),
            loop_statement_entry_stack: Vec::new(),
            loop_statement_exit_stack: Vec::new(),
            loop_statement_scopesize_stack: Vec::new(),
            last_block: BlockGen::new(self.context, function_with_prototype.1, "entry"),
        };
        func_gen.create_scope(self, false);

        for index in 0..func_gen.prototype.param_type_vec.len() {
            let variable = func_gen.create_variable(
                self.context,
                func_gen.prototype.param_name_vec[index].clone(),
                func_gen.prototype.param_type_vec[index],
            );

            Value::from_basic_value(
                func_gen.prototype.param_type_vec[index],
                func_gen
                    .function
                    .get_nth_param(index.try_into().unwrap())
                    .unwrap(),
            )
            .invoke_handler(
                ValueHandler::new()
                    .handle_bool(&mut |_, value| {
                        func_gen.last_block.builder.build_store(variable, value);
                    })
                    .handle_int(&|_, value| {
                        func_gen.last_block.builder.build_store(variable, value);
                    })
                    .handle_unsigned_int(&|_, value| {
                        func_gen.last_block.builder.build_store(variable, value);
                    })
                    .handle_float(&|_, value| {
                        func_gen.last_block.builder.build_store(variable, value);
                    })
                    .handle_str(&|_, value| {
                        func_gen.last_block.builder.build_store(variable, value);
                    }),
            );
        }

        func_gen.create_scope(self, false);

        func_gen
    }
}

impl<'bdr, 'fnc: 'bdr, 'mdl: 'fnc, 'ctx: 'mdl> FuncGen<'ctx> {
    pub fn end_function(mut self, module: &'mdl mut ModuleGen<'ctx>) {
        if self.scope_stack.len() != 2 {
            panic!("scope not yet closed.");
        }

        if !self.loop_label_stack.is_empty() {
            panic!("loop statement not yet closed.");
        }

        if !self.loop_statement_entry_stack.is_empty() {
            panic!("loop statement not yet closed.");
        }

        if !self.loop_statement_exit_stack.is_empty() {
            panic!("loop statement not yet closed.");
        }

        self.end_scope(module);
        self.end_scope(module);

        let blocks = self.function.get_basic_blocks();

        if self.prototype.return_type == ValueType::Void
            && blocks.last().unwrap().get_terminator().is_none()
        {
            self.last_block.builder.build_return(None);
        }

        let pass_mgr = PassManager::<FunctionValue<'ctx>>::create(&module.module);
        pass_mgr.add_cfg_simplification_pass();
        pass_mgr.add_verifier_pass();
        pass_mgr.initialize();
        pass_mgr.run_on(&self.function);
    }

    pub fn create_scope(
        &'fnc mut self,
        module: &'mdl mut ModuleGen<'ctx>,
        need_explicit_removal: bool,
    ) {
        self.scope_stack.push(ScopeGen {
            variable_map: HashMap::new(),
            stack_position: if need_explicit_removal {
                Some(
                    self.last_block
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

    pub fn end_scope(&'fnc mut self, module: &'mdl mut ModuleGen<'ctx>) {
        if self.scope_stack.last().unwrap().stack_position.is_some() {
            self.last_block.builder.build_call(
                module
                    .function_prototype_table
                    .get("llvm.stackrestore")
                    .expect("intrinsic function 'llvm.stackrestore' not found.")
                    .function,
                vec![self.scope_stack.pop().unwrap().stack_position.unwrap()].as_slice(),
                "stackrestore",
            );
        } else {
            self.scope_stack.pop();
        }
    }

    pub fn end_scope_runtime(&'fnc mut self, module: &'mdl mut ModuleGen<'ctx>, count: usize) {
        for index in 0..self.scope_stack.len() - count {
            if self.scope_stack[self.scope_stack.len() - 1 - index]
                .stack_position
                .is_some()
            {
                self.last_block.builder.build_call(
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
        name: String,
        value_type: ValueType,
    ) -> PointerValue<'ctx> {
        let scope = self.scope_stack.last_mut().expect("scope empty.");

        if scope.variable_map.contains_key(&name) {
            panic!("variable {} is already declared in this scope.", name);
        }

        let address = self
            .last_block
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
        builder.position_at_end(block);

        BlockGen {
            block: block,
            builder: builder,
        }
    }

    pub fn from(context: &'ctx Context, block: BasicBlock<'ctx>) -> Self {
        let builder = context.create_builder();
        builder.position_at_end(block);
        BlockGen {
            block,
            builder: builder,
        }
    }

    pub fn connect_to(&self, to: &Self) {
        self.builder.build_unconditional_branch(to.block);
    }

    pub fn connect_to_if(&self, criteria: IntValue<'ctx>, to_then: &Self, to_else: &Self) {
        self.builder
            .build_conditional_branch(criteria, to_then.block, to_else.block);
    }
}

impl<'mdl, 'ctx: 'mdl> ModuleGen<'ctx> {
    pub fn generate_code(&'mdl mut self, ast: &AST) {
        if ast.name != "module" {
            panic!("module AST expected, got {}.", ast.name);
        }

        self.create_function("main", ValueType::Void, vec![], vec![], true)
            .generate_code_statement_list(self.context, self, &ast.children[0])
            .end_function(self);
    }
}

impl<'fnc, 'mdl: 'fnc, 'ctx: 'mdl> FuncGen<'ctx> {
    pub fn generate_code_statement_list(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.name != "statement-list" {
            panic!("statement-list AST expected, got {}.", ast.name);
        }

        if ast.children.is_empty() {
            return self;
        }

        let mut statement_ast_stack: Vec<&AST> = ast.children.iter().rev().collect();

        while statement_ast_stack.last().unwrap().name == "statement-list" {
            let mut statement_vec: Vec<&AST> = statement_ast_stack
                .pop()
                .unwrap()
                .children
                .iter()
                .rev()
                .collect();

            statement_ast_stack.append(&mut statement_vec);
        }

        for ast in statement_ast_stack.into_iter().rev() {
            self = self.generate_code_statement(context, module, ast);
        }

        self
    }

    pub fn generate_code_statement(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.name != "statement" {
            panic!("statement AST expected, got {}.", ast.name);
        }

        match ast.children[0].name.as_ref() {
            "scope-statement" => {
                self.generate_code_statement_scope(context, module, &ast.children[0], true)
            }
            "if-statement" => self.generate_code_statement_if(context, module, &ast.children[0]),
            "for-statement" => self.generate_code_statement_for(context, module, &ast.children[0]),
            "with-statement" => unimplemented!(),
            "ret-statement" => self.generate_code_statement_ret(context, module, &ast.children[0]),
            "let-statement" => self.generate_code_statement_let(context, module, &ast.children[0]),
            "break-statement" => {
                self.generate_code_statement_break(context, module, &ast.children[0])
            }
            "continue-statement" => {
                self.generate_code_statement_continue(context, module, &ast.children[0])
            }
            "func-def-statement" => {
                self.generate_code_statement_func_def(context, module, &ast.children[0])
            }
            "expression" => {
                self.generate_code_expression(context, module, &ast.children[0]);
                self
            }
            _ => unreachable!(),
        }
    }

    pub fn generate_code_statement_scope(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
        need_explicit_removal: bool,
    ) -> Self {
        if ast.name != "scope-statement" {
            panic!("scope-statement AST expected, got {}.", ast.name);
        }

        if ast.children.len() == 3 {
            self.create_scope(module, need_explicit_removal);
            self = self.generate_code_statement_list(context, module, &ast.children[1]);
            self.end_scope(module);
        }

        self
    }

    pub fn generate_code_statement_if(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.name != "if-statement" {
            panic!("if-statement AST expected, got {}.", ast.name);
        }

        let criteria = self.generate_code_expression(context, module, &ast.children[1]);

        if criteria.get_type() != ValueType::Bool {
            panic!(
                "type error; {} type extected, got {} type.",
                ValueType::Bool,
                criteria.get_type()
            );
        }

        if ast.children.len() == 3 {
            let temp_last_block = BlockGen::from(context, self.last_block.block);

            // Then block.
            self.last_block = BlockGen::new(context, self.function, "THEN");
            let temp_then_block = BlockGen::from(context, self.last_block.block);

            self = self.generate_code_statement_scope(context, module, &ast.children[2], true);

            let temp_then_block_end = BlockGen::from(context, self.last_block.block);

            // End block.
            self.last_block = BlockGen::new(context, self.function, "IF END");
            let temp_end_block = BlockGen::from(context, self.last_block.block);

            criteria.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
                temp_last_block.connect_to_if(value, &temp_then_block, &temp_end_block)
            }));
            temp_then_block_end.connect_to(&temp_end_block);
        } else {
            let temp_last_block = BlockGen::from(context, self.last_block.block);

            // Then block.
            self.last_block = BlockGen::new(context, self.function, "THEN");
            let temp_then_block = BlockGen::from(context, self.last_block.block);

            self = self.generate_code_statement_scope(context, module, &ast.children[2], true);

            let temp_then_block_end = BlockGen::from(context, self.last_block.block);

            // Else block.
            self.last_block = BlockGen::new(context, self.function, "ELSE");
            let temp_else_block = BlockGen::from(context, self.last_block.block);

            self = if ast.children[4].name == "scope-statement" {
                self.generate_code_statement_scope(context, module, &ast.children[4], true)
            } else {
                self.generate_code_statement_if(context, module, &ast.children[4])
            };

            let temp_else_block_end = BlockGen::from(context, self.last_block.block);

            // End block.
            self.last_block = BlockGen::new(context, self.function, "IF END");
            let temp_end_block = BlockGen::from(context, self.last_block.block);

            criteria.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
                temp_last_block.connect_to_if(value, &temp_then_block, &temp_else_block)
            }));
            temp_then_block_end.connect_to(&temp_end_block);
            temp_else_block_end.connect_to(&temp_end_block);
        }

        self
    }

    pub fn generate_code_statement_for(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.name != "for-statement" {
            panic!("for-statement AST expected, got {}.", ast.name);
        }

        if ast.children.len() == 2 {
            let loop_block = BlockGen::new(context, self.function, "LOOP");
            let exit_block = BlockGen::new(context, self.function, "LOOP EXIT");
            self.last_block.connect_to(&loop_block);

            self.loop_statement_entry_stack.push(loop_block.block);
            self.loop_statement_exit_stack.push(exit_block.block);
            self.loop_statement_scopesize_stack
                .push(self.scope_stack.len());

            // Loop block.
            self.last_block = loop_block;
            let temp_loop_block = BlockGen::from(context, self.last_block.block);
            self = self.generate_code_statement_scope(context, module, &ast.children[1], true);
            self.last_block.connect_to(&temp_loop_block);

            self.loop_statement_entry_stack.pop();
            self.loop_statement_exit_stack.pop();
            self.loop_statement_scopesize_stack.pop();

            // End block.
            self.last_block = BlockGen::new(context, self.function, "LOOP END");
            exit_block.connect_to(&self.last_block);
        } else if ast.children.len() == 4 {
            let loop_block = BlockGen::new(context, self.function, "LOOP");
            let exit_block = BlockGen::new(context, self.function, "LOOP EXIT");
            self.last_block.connect_to(&loop_block);

            self.loop_label_stack.push((
                ast.children[1]
                    .child
                    .as_ref()
                    .unwrap()
                    .token_content
                    .clone(),
                self.loop_statement_entry_stack.len(),
            ));
            self.loop_statement_entry_stack.push(loop_block.block);
            self.loop_statement_exit_stack.push(exit_block.block);
            self.loop_statement_scopesize_stack
                .push(self.scope_stack.len());

            // Loop block.
            self.last_block = loop_block;
            let temp_loop_block = BlockGen::from(context, self.last_block.block);
            self = self.generate_code_statement_scope(context, module, &ast.children[3], true);
            self.last_block.connect_to(&temp_loop_block);

            self.loop_label_stack.pop();
            self.loop_statement_entry_stack.pop();
            self.loop_statement_exit_stack.pop();
            self.loop_statement_scopesize_stack.pop();

            // End block.
            self.last_block = BlockGen::new(context, self.function, "LOOP END");
            exit_block.connect_to(&self.last_block);
        } else if ast.children.len() == 3 {
            let criteria_block = BlockGen::new(context, self.function, "CRITERIA");
            let loop_block = BlockGen::new(context, self.function, "LOOP");
            let exit_block = BlockGen::new(context, self.function, "LOOP EXIT");
            self.last_block.connect_to(&criteria_block);

            self.loop_statement_entry_stack.push(self.last_block.block);
            self.loop_statement_exit_stack.push(exit_block.block);
            self.loop_statement_scopesize_stack
                .push(self.scope_stack.len());

            // Criteria block.
            self.last_block = criteria_block;
            let temp_criteria_block = BlockGen::from(context, self.last_block.block);
            let criteria = self.generate_code_expression(context, module, &ast.children[1]);

            if criteria.get_type() != ValueType::Bool {
                panic!(
                    "type error; {} type extected, got {} type.",
                    ValueType::Bool,
                    criteria.get_type()
                );
            }

            criteria.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
                self.last_block
                    .connect_to_if(value, &loop_block, &exit_block);
            }));

            // Loop block.
            self.last_block = loop_block;
            self = self.generate_code_statement_scope(context, module, &ast.children[2], true);
            self.last_block.connect_to(&temp_criteria_block);

            self.loop_statement_entry_stack.pop();
            self.loop_statement_exit_stack.pop();
            self.loop_statement_scopesize_stack.pop();

            // Exit block.
            self.last_block = BlockGen::new(context, self.function, "LOOP END");
            exit_block.connect_to(&self.last_block);
        } else if ast.children.len() == 5 {
            let criteria_block = BlockGen::new(context, self.function, "CRITERIA");
            let loop_block = BlockGen::new(context, self.function, "LOOP");
            let exit_block = BlockGen::new(context, self.function, "LOOP EXIT");
            self.last_block.connect_to(&criteria_block);

            self.loop_statement_entry_stack.push(self.last_block.block);
            self.loop_statement_exit_stack.push(exit_block.block);
            self.loop_statement_scopesize_stack
                .push(self.scope_stack.len());

            // Criteria block.
            self.last_block = criteria_block;
            let temp_criteria_block = BlockGen::from(context, self.last_block.block);
            let criteria = self.generate_code_expression(context, module, &ast.children[1]);

            if criteria.get_type() != ValueType::Bool {
                panic!(
                    "type error; {} type extected, got {} type.",
                    ValueType::Bool,
                    criteria.get_type()
                );
            }

            criteria.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
                self.last_block
                    .connect_to_if(value, &loop_block, &exit_block);
            }));

            self.loop_label_stack.push((
                ast.children[1]
                    .child
                    .as_ref()
                    .unwrap()
                    .token_content
                    .clone(),
                self.loop_statement_entry_stack.len(),
            ));

            // Loop block.
            self.last_block = loop_block;
            self = self.generate_code_statement_scope(context, module, &ast.children[4], true);
            self.last_block.connect_to(&temp_criteria_block);

            self.loop_label_stack.pop();
            self.loop_statement_entry_stack.pop();
            self.loop_statement_exit_stack.pop();
            self.loop_statement_scopesize_stack.pop();

            // End block.
            self.last_block = BlockGen::new(context, self.function, "LOOP END");
            exit_block.connect_to(&self.last_block);
        } else {
            unimplemented!();
        }

        self
    }

    pub fn generate_code_statement_ret(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.children.len() == 1 {
            if self.prototype.return_type != ValueType::Void {
                panic!(
                    "type error; {} value must be provided to return.",
                    self.prototype.return_type
                );
            }

            self.last_block.builder.build_return(None);
        } else {
            let mut return_value = self.generate_code_expression(context, module, &ast.children[1]);
            return_value =
                self.match_type_single(context, return_value, self.prototype.return_type);

            return_value.invoke_handler(
                ValueHandler::new()
                    .handle_bool(&|_, value| self.last_block.builder.build_return(Some(&value)))
                    .handle_int(&|_, value| self.last_block.builder.build_return(Some(&value)))
                    .handle_unsigned_int(&|_, value| {
                        self.last_block.builder.build_return(Some(&value))
                    })
                    .handle_float(&|_, value| self.last_block.builder.build_return(Some(&value)))
                    .handle_str(&|_, value| self.last_block.builder.build_return(Some(&value))),
            );
        }

        self.last_block = BlockGen::new(context, self.function, "RET");

        self
    }

    pub fn generate_code_statement_let(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.children.len() == 4 {
            let initial_value = self.generate_code_expression(context, module, &ast.children[3]);

            if initial_value.get_type() == ValueType::Void {
                panic!("type error; {} type is not allowed.", ValueType::Void);
            }

            let variable_address = self.create_variable(
                context,
                ast.children[1]
                    .child
                    .as_ref()
                    .unwrap()
                    .token_content
                    .clone(),
                initial_value.get_type(),
            );

            initial_value.invoke_handler(
                ValueHandler::new()
                    .handle_bool(&|_, value| {
                        self.last_block.builder.build_store(variable_address, value);
                    })
                    .handle_int(&|_, value| {
                        self.last_block.builder.build_store(variable_address, value);
                    })
                    .handle_unsigned_int(&|_, value| {
                        self.last_block.builder.build_store(variable_address, value);
                    })
                    .handle_float(&|_, value| {
                        self.last_block.builder.build_store(variable_address, value);
                    })
                    .handle_str(&|_, value| {
                        self.last_block.builder.build_store(variable_address, value);
                    }),
            );
        } else {
            unimplemented!();
        }

        self
    }

    pub fn generate_code_statement_break(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.children.len() == 1 {
            self.end_scope_runtime(
                module,
                *self
                    .loop_statement_scopesize_stack
                    .last()
                    .expect("wrong usage; break statements can be placed in loop statements"),
            );
            self.last_block.connect_to(&BlockGen::from(
                context,
                self.loop_statement_exit_stack
                    .last()
                    .expect("wrong usage; break statements can be placed in loop statements")
                    .clone(),
            ));
        } else {
            unimplemented!();
        }

        self.last_block = BlockGen::new(context, self.function, "BREAK");

        self
    }

    pub fn generate_code_statement_continue(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.children.len() == 1 {
            self.end_scope_runtime(
                module,
                *self
                    .loop_statement_scopesize_stack
                    .last()
                    .expect("wrong usage; break statements can be placed in loop statements"),
            );
            self.last_block.connect_to(&BlockGen::from(
                context,
                self.loop_statement_entry_stack
                    .last()
                    .expect("wrong usage; continue statements can be placed in loop statements")
                    .clone(),
            ));
        } else {
            unimplemented!();
        }

        self.last_block = BlockGen::new(context, self.function, "CONTINUE");

        self
    }

    pub fn generate_code_statement_func_def(
        mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Self {
        if ast.children.len() == 6 {
            let return_type = ast.children[3].children[0]
                .child
                .as_ref()
                .unwrap()
                .token_type;

            let return_value_type = match return_type {
                TokenType::KeywordVoid => ValueType::Void,
                TokenType::KeywordBool => ValueType::Bool,
                TokenType::KeywordI8 => ValueType::I8,
                TokenType::KeywordI16 => ValueType::I16,
                TokenType::KeywordI32 => ValueType::I32,
                TokenType::KeywordI64 => ValueType::I64,
                TokenType::KeywordI128 => ValueType::I128,
                TokenType::KeywordU8 => ValueType::U8,
                TokenType::KeywordU16 => ValueType::U16,
                TokenType::KeywordU32 => ValueType::U32,
                TokenType::KeywordU64 => ValueType::U64,
                TokenType::KeywordU128 => ValueType::U128,
                TokenType::KeywordF16 => ValueType::F16,
                TokenType::KeywordF32 => ValueType::F32,
                TokenType::KeywordF64 => ValueType::F64,
                TokenType::KeywordStr => ValueType::Str,
                _ => unreachable!(),
            };

            module
                .create_function(
                    &ast.children[0].child.as_ref().unwrap().token_content,
                    return_value_type,
                    vec![],
                    vec![],
                    false,
                )
                .generate_code_statement_scope(context, module, &ast.children[5], false)
                .end_function(module);
        } else {
            let return_type = ast.children[4].children[0]
                .child
                .as_ref()
                .unwrap()
                .token_type;

            let return_value_type = match return_type {
                TokenType::KeywordVoid => ValueType::Void,
                TokenType::KeywordBool => ValueType::Bool,
                TokenType::KeywordI8 => ValueType::I8,
                TokenType::KeywordI16 => ValueType::I16,
                TokenType::KeywordI32 => ValueType::I32,
                TokenType::KeywordI64 => ValueType::I64,
                TokenType::KeywordI128 => ValueType::I128,
                TokenType::KeywordU8 => ValueType::U8,
                TokenType::KeywordU16 => ValueType::U16,
                TokenType::KeywordU32 => ValueType::U32,
                TokenType::KeywordU64 => ValueType::U64,
                TokenType::KeywordU128 => ValueType::U128,
                TokenType::KeywordF16 => ValueType::F16,
                TokenType::KeywordF32 => ValueType::F32,
                TokenType::KeywordF64 => ValueType::F64,
                TokenType::KeywordStr => ValueType::Str,
                _ => unreachable!(),
            };

            let mut parameter_type_vec: Vec<ValueType> = Vec::new();
            let mut parameter_name_vec: Vec<String> = Vec::new();
            let mut parameter_stack: Vec<&AST> = ast.children[2].children.iter().rev().collect();

            while parameter_stack.last().unwrap().name == "func-def-statement-param-list" {
                let mut parameter_vec: Vec<&AST> = parameter_stack
                    .pop()
                    .unwrap()
                    .children
                    .iter()
                    .rev()
                    .collect();

                parameter_stack.append(&mut parameter_vec);
            }

            for (id, ty) in parameter_stack
                .into_iter()
                .rev()
                .filter(|ast| ast.name != "Comma")
                .tuples()
            {
                let param_type = ty.children[0].child.as_ref().unwrap().token_type;

                if param_type == TokenType::KeywordVoid {
                    panic!("wrong type; {} cannot be used here", ValueType::Void);
                }

                let param_value_type = match param_type {
                    TokenType::KeywordBool => ValueType::Bool,
                    TokenType::KeywordI8 => ValueType::I8,
                    TokenType::KeywordI16 => ValueType::I16,
                    TokenType::KeywordI32 => ValueType::I32,
                    TokenType::KeywordI64 => ValueType::I64,
                    TokenType::KeywordI128 => ValueType::I128,
                    TokenType::KeywordU8 => ValueType::U8,
                    TokenType::KeywordU16 => ValueType::U16,
                    TokenType::KeywordU32 => ValueType::U32,
                    TokenType::KeywordU64 => ValueType::U64,
                    TokenType::KeywordU128 => ValueType::U128,
                    TokenType::KeywordF16 => ValueType::F16,
                    TokenType::KeywordF32 => ValueType::F32,
                    TokenType::KeywordF64 => ValueType::F64,
                    TokenType::KeywordStr => ValueType::Str,
                    _ => unreachable!(),
                };

                parameter_type_vec.push(param_value_type);
                parameter_name_vec.push(id.child.as_ref().unwrap().token_content.clone());
            }

            module
                .create_function(
                    &ast.children[0].child.as_ref().unwrap().token_content,
                    return_value_type,
                    parameter_type_vec,
                    parameter_name_vec,
                    false,
                )
                .generate_code_statement_scope(context, module, &ast.children[6], false)
                .end_function(module);
        }

        self
    }

    pub fn generate_code_expression(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        match ast.name.as_ref() {
            "expression" => self.generate_code_expression(context, module, &ast.children[0]),
            "assignment" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_assignment(context, module, ast)
                }
            }
            "op-or" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_or(context, module, ast)
                }
            }
            "op-and" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_and(context, module, ast)
                }
            }
            "op-cmp" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_cmp(context, module, ast)
                }
            }
            "op-addsub" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_addsub(context, module, ast)
                }
            }
            "op-muldivmod" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_muldivmod(context, module, ast)
                }
            }
            "op-shift" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_shift(context, module, ast)
                }
            }
            "op-bit-or" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_bit_or(context, module, ast)
                }
            }
            "op-bit-and" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_bit_and(context, module, ast)
                }
            }
            "op-bit-xor" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_bit_xor(context, module, ast)
                }
            }
            "op-cast" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_cast(context, module, ast)
                }
            }
            "op-single" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_op_single(context, module, ast)
                }
            }
            "function-call" => {
                if ast.children.len() == 1 {
                    self.generate_code_expression(context, module, &ast.children[0])
                } else {
                    self.generate_code_expression_function_call(context, module, ast)
                }
            }
            "left-value" => self.generate_code_expression_left_value(context, module, ast),
            "literal" => self.generate_code_expression_literal(context, module, ast),
            _ => panic!("not implemented AST; {} received.", ast.name),
        }
    }

    pub fn generate_code_expression_assignment(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut value = self.generate_code_expression(context, module, &ast.children[2]);
        let variable = self.get_left_value(context, module, &ast.children[0]);

        value = self.match_type_single(context, value, variable.0);
        value = match ast.children[1].child.as_ref().unwrap().token_type {
            TokenType::OpAssign => value,
            TokenType::OpAssignAdd => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_nsw_add(
                                        lhs_value,
                                        rhs_value,
                                        "temporal ADD",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_add(
                                        lhs_value,
                                        rhs_value,
                                        "temporal ADD",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, rhs_value| {
                        Value::from_float_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_float(&|_, lhs_value| {
                                    self.last_block.builder.build_float_add(
                                        lhs_value,
                                        rhs_value,
                                        "temporal ADD",
                                    )
                                }),
                            ),
                        )
                    }),
            ),
            TokenType::OpAssignSub => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_nsw_sub(
                                        lhs_value,
                                        rhs_value,
                                        "temporal SUB",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_sub(
                                        lhs_value,
                                        rhs_value,
                                        "temporal SUB",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, rhs_value| {
                        Value::from_float_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_float(&|_, lhs_value| {
                                    self.last_block.builder.build_float_sub(
                                        lhs_value,
                                        rhs_value,
                                        "temporal SUB",
                                    )
                                }),
                            ),
                        )
                    }),
            ),
            TokenType::OpAssignMul => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_nsw_mul(
                                        lhs_value,
                                        rhs_value,
                                        "temporal MUL",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_mul(
                                        lhs_value,
                                        rhs_value,
                                        "temporal MUL",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, rhs_value| {
                        Value::from_float_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_float(&|_, lhs_value| {
                                    self.last_block.builder.build_float_mul(
                                        lhs_value,
                                        rhs_value,
                                        "temporal MUL",
                                    )
                                }),
                            ),
                        )
                    }),
            ),
            TokenType::OpAssignDiv => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_signed_div(
                                        lhs_value,
                                        rhs_value,
                                        "temporal DIV",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_unsigned_div(
                                        lhs_value,
                                        rhs_value,
                                        "temporal DIV",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, rhs_value| {
                        Value::from_float_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_float(&|_, lhs_value| {
                                    self.last_block.builder.build_float_div(
                                        lhs_value,
                                        rhs_value,
                                        "temporal DIV",
                                    )
                                }),
                            ),
                        )
                    }),
            ),
            TokenType::OpAssignMod => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_signed_rem(
                                        lhs_value,
                                        rhs_value,
                                        "temporal MOD",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_int_unsigned_rem(
                                        lhs_value,
                                        rhs_value,
                                        "temporal MOD",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, rhs_value| {
                        Value::from_float_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_float(&|_, lhs_value| {
                                    self.last_block.builder.build_float_rem(
                                        lhs_value,
                                        rhs_value,
                                        "temporal REM",
                                    )
                                }),
                            ),
                        )
                    }),
            ),
            TokenType::OpAssignShiftL => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_left_shift(
                                        lhs_value,
                                        rhs_value,
                                        "temporal LSH",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_left_shift(
                                        lhs_value,
                                        rhs_value,
                                        "temporal LSH",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, _| {
                        panic!(
                            "type error; bitwise operations between {}s are not allowed.",
                            variable.0
                        )
                    }),
            ),
            TokenType::OpAssignShiftR => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_right_shift(
                                        lhs_value,
                                        rhs_value,
                                        true,
                                        "temporal RSH",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_right_shift(
                                        lhs_value,
                                        rhs_value,
                                        false,
                                        "temporal RSH",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, _| {
                        panic!(
                            "type error; bitwise operations between {}s are not allowed.",
                            variable.0
                        )
                    }),
            ),
            TokenType::OpAssignBitOr => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_or(
                                        lhs_value,
                                        rhs_value,
                                        "temporal BITWISE OR",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_or(
                                        lhs_value,
                                        rhs_value,
                                        "temporal BITWISE OR",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, _| {
                        panic!(
                            "type error; bitwise operations between {}s are not allowed.",
                            variable.0
                        )
                    }),
            ),
            TokenType::OpAssignBitAnd => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_and(
                                        lhs_value,
                                        rhs_value,
                                        "temporal BITWISE AND",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_and(
                                        lhs_value,
                                        rhs_value,
                                        "temporal BITWISE AND",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, _| {
                        panic!(
                            "type error; bitwise operations between {}s are not allowed.",
                            variable.0
                        )
                    }),
            ),
            TokenType::OpAssignBitXor => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_int(&|_, lhs_value| {
                                    self.last_block.builder.build_xor(
                                        lhs_value,
                                        rhs_value,
                                        "temporal BITWISE XOR",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            Value::from_basic_value(
                                variable.0,
                                self.last_block
                                    .builder
                                    .build_load(variable.1, "temporal LOAD"),
                            )
                            .invoke_handler(
                                ValueHandler::new().handle_unsigned_int(&|_, lhs_value| {
                                    self.last_block.builder.build_xor(
                                        lhs_value,
                                        rhs_value,
                                        "temporal BITWISE XOR",
                                    )
                                }),
                            ),
                        )
                    })
                    .handle_float(&|_, _| {
                        panic!(
                            "type error; bitwise operations between {}s are not allowed.",
                            variable.0
                        )
                    }),
            ),
            TokenType::OpAssignBitNot => value.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, rhs_value| {
                        Value::from_int_value(
                            variable.0.get_bitwidth(),
                            self.last_block
                                .builder
                                .build_not(rhs_value, "temporal BITWISE NOT"),
                        )
                    })
                    .handle_unsigned_int(&|_, rhs_value| {
                        Value::from_unsigned_int_value(
                            variable.0.get_bitwidth(),
                            self.last_block
                                .builder
                                .build_not(rhs_value, "temporal BITWISE NOT"),
                        )
                    })
                    .handle_float(&|_, _| {
                        panic!(
                            "type error; bitwise operations between {}s are not allowed.",
                            variable.0
                        )
                    }),
            ),
            _ => unreachable!(),
        };

        self.last_block
            .builder
            .build_store(variable.1, value.to_basic_value());

        value
    }

    pub fn generate_code_expression_op_or(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let lhs_block = self.last_block.block;

        let rhs_block = BlockGen::new(context, self.function, "OR rhs");
        let end_block = BlockGen::new(context, self.function, "OR end");
        lhs.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
            self.last_block.connect_to_if(value, &end_block, &rhs_block);
        }));

        self.last_block = rhs_block;
        let rhs = self.generate_code_expression(context, module, &ast.children[2]);
        self.last_block.connect_to(&end_block);

        if lhs.get_type() != ValueType::Bool || rhs.get_type() != ValueType::Bool {
            panic!(
                "type error; boolean operations are only allowed between {}s.",
                ValueType::Bool
            );
        }

        let result = end_block.builder.build_phi(context.bool_type(), "OR");
        result.add_incoming(&[(&context.bool_type().const_int(1, false), lhs_block)]);
        result.add_incoming(&[(&rhs.to_basic_value(), self.last_block.block)]);

        Value::from_basic_value(ValueType::Bool, result.as_basic_value())
    }

    pub fn generate_code_expression_op_and(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let lhs_block = self.last_block.block;

        let rhs_block = BlockGen::new(context, self.function, "AND rhs");
        let end_block = BlockGen::new(context, self.function, "AND end");
        lhs.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
            self.last_block.connect_to_if(value, &rhs_block, &end_block);
        }));

        self.last_block = rhs_block;
        let rhs = self.generate_code_expression(context, module, &ast.children[2]);
        self.last_block.connect_to(&end_block);

        if lhs.get_type() != ValueType::Bool || rhs.get_type() != ValueType::Bool {
            panic!(
                "type error; boolean operations are only allowed between {}s.",
                ValueType::Bool
            );
        }

        let result = end_block.builder.build_phi(context.bool_type(), "AND");
        result.add_incoming(&[(&context.bool_type().const_int(0, false), lhs_block)]);
        result.add_incoming(&[(&rhs.to_basic_value(), self.last_block.block)]);

        Value::from_basic_value(ValueType::Bool, result.as_basic_value())
    }

    pub fn generate_code_expression_op_cmp(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let mut rhs = self.generate_code_expression(context, module, &ast.children[2]);

        let matched = self.match_type(context, lhs, rhs);

        match matched.0.get_type() {
            ValueType::Void | ValueType::Str => panic!(
                "type error; relational operations between {}s are not allowed.",
                matched.0.get_type()
            ),
            _ => (),
        }

        lhs = matched.0;
        rhs = matched.1;

        let int_predicate = match ast.children[1].child.as_ref().unwrap().token_type {
            TokenType::OpEq => IntPredicate::EQ,
            TokenType::OpNeq => IntPredicate::NE,
            TokenType::OpLs => IntPredicate::ULT,
            TokenType::OpLsEq => IntPredicate::ULE,
            TokenType::OpGt => IntPredicate::UGT,
            TokenType::OpGtEq => IntPredicate::UGE,
            _ => unreachable!(),
        };
        let float_predicate = match ast.children[1].child.as_ref().unwrap().token_type {
            TokenType::OpEq => FloatPredicate::OEQ,
            TokenType::OpNeq => FloatPredicate::ONE,
            TokenType::OpLs => FloatPredicate::OLT,
            TokenType::OpLsEq => FloatPredicate::OLE,
            TokenType::OpGt => FloatPredicate::OGT,
            TokenType::OpGtEq => FloatPredicate::OGE,
            _ => unreachable!(),
        };

        lhs.invoke_handler(
			ValueHandler::new()
				.handle_bool(&|_, lhs_value| {
					rhs.invoke_handler(ValueHandler::new().handle_bool(&|_, rhs_value| {
						Value::Bool {
							value: self.last_block.builder.build_int_compare(
								int_predicate,
								lhs_value,
								rhs_value,
								"CMP",
							),
						}
					}))
				})
				.handle_int(&|_, lhs_value| {
					rhs.invoke_handler(ValueHandler::new().handle_int(&|_, rhs_value| {
						Value::Bool {
							value: self.last_block.builder.build_int_compare(
								int_predicate,
								lhs_value,
								rhs_value,
								"CMP",
							),
						}
					}))
				})
				.handle_unsigned_int(&|_, lhs_value| {
					rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
						Value::Bool {
							value: self.last_block.builder.build_int_compare(
								int_predicate,
								lhs_value,
								rhs_value,
								"CMP",
							),
						}
					}))
				})
				.handle_float(&|_, lhs_value| {
					rhs.invoke_handler(ValueHandler::new().handle_float(&|_, rhs_value| {
						Value::Bool {
							value: self.last_block.builder.build_float_compare(
								float_predicate,
								lhs_value,
								rhs_value,
								"CMP",
							),
						}
					}))
				})
				//
				// TODO: Implement the str cmp functionality here.
				//
				// .handle_str(&|_, lhs_value| {
				// 	rhs.invoke_handler(ValueHandler::new().handle_str(&|_, rhs_value| {
				// 		Value::Bool {
				// 			value: block.builder.build_int_compare(
				// 				int_predicate,
				// 				lhs_value,
				// 				rhs_value,
				// 				"CMP",
				// 			),
				// 		}
				// 	}))
				// }),
		)
    }

    pub fn generate_code_expression_op_addsub(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let mut rhs = self.generate_code_expression(context, module, &ast.children[2]);

        let matched = self.match_type(context, lhs, rhs);

        match matched.0.get_type() {
            ValueType::Void | ValueType::Bool | ValueType::Str => panic!(
                "type error; arithmetic operations between {}s are not allowed.",
                matched.0.get_type()
            ),
            _ => (),
        }

        lhs = matched.0;
        rhs = matched.1;

        let subtraction = ast.children[1].child.as_ref().unwrap().token_type == TokenType::OpSub;

        lhs.invoke_handler(
            ValueHandler::new()
                .handle_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::I8 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            16 => Value::I16 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            32 => Value::I32 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            64 => Value::I64 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            128 => Value::I128 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_nsw_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            _ => unreachable!(),
                        }
                    }))
                })
                .handle_unsigned_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::U8 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            16 => Value::U16 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            32 => Value::U32 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            64 => Value::U64 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            128 => Value::U128 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_int_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_int_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            _ => unreachable!(),
                        }
                    }))
                })
                .handle_float(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_float(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            16 => Value::F16 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_float_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_float_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            32 => Value::F32 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_float_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_float_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            64 => Value::F64 {
                                value: if subtraction {
                                    self.last_block
                                        .builder
                                        .build_float_sub(lhs_value, rhs_value, "SUB")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_float_add(lhs_value, rhs_value, "ADD")
                                },
                            },
                            _ => unreachable!(),
                        }
                    }))
                }),
        )
    }

    pub fn generate_code_expression_op_muldivmod(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let mut rhs = self.generate_code_expression(context, module, &ast.children[2]);

        let matched = self.match_type(context, lhs, rhs);

        match matched.0.get_type() {
            ValueType::Void | ValueType::Bool | ValueType::Str => panic!(
                "type error; arithmetic operations between {}s are not allowed.",
                matched.0.get_type()
            ),
            _ => (),
        }

        lhs = matched.0;
        rhs = matched.1;

        let op_token_type = ast.children[1].child.as_ref().unwrap().token_type;

        lhs.invoke_handler(
            ValueHandler::new()
                .handle_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::I8 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_nsw_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_signed_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_signed_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            16 => Value::I16 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_nsw_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_signed_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_signed_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            32 => Value::I32 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_nsw_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_signed_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_signed_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            64 => Value::I64 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_nsw_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_signed_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_signed_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            128 => Value::I128 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_nsw_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_signed_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_signed_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            _ => unreachable!(),
                        }
                    }))
                })
                .handle_unsigned_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::U8 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            16 => Value::U16 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            32 => Value::U32 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            64 => Value::U64 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            128 => Value::U128 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_int_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            _ => unreachable!(),
                        }
                    }))
                })
                .handle_float(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_float(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            16 => Value::F16 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_float_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_float_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_float_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            32 => Value::F32 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_float_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_float_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_float_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            64 => Value::F64 {
                                value: match op_token_type {
                                    TokenType::OpMul => self
                                        .last_block
                                        .builder
                                        .build_float_mul(lhs_value, rhs_value, "MUL"),
                                    TokenType::OpDiv => self
                                        .last_block
                                        .builder
                                        .build_float_div(lhs_value, rhs_value, "DIV"),
                                    TokenType::OpMod => self
                                        .last_block
                                        .builder
                                        .build_float_rem(lhs_value, rhs_value, "MOD"),
                                    _ => unreachable!(),
                                },
                            },
                            _ => unreachable!(),
                        }
                    }))
                }),
        )
    }

    pub fn generate_code_expression_op_shift(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let mut rhs = self.generate_code_expression(context, module, &ast.children[2]);

        let matched = self.match_type(context, lhs, rhs);

        match matched.0.get_type() {
            ValueType::Void
            | ValueType::Bool
            | ValueType::F16
            | ValueType::F32
            | ValueType::F64
            | ValueType::Str => panic!(
                "type error; bitwise operations between {}s are not allowed.",
                matched.0.get_type()
            ),
            _ => (),
        }

        lhs = matched.0;
        rhs = matched.1;

        let right = ast.children[1].child.as_ref().unwrap().token_type == TokenType::OpShiftR;

        lhs.invoke_handler(
            ValueHandler::new()
                .handle_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::I8 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, true, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            16 => Value::I16 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, true, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            32 => Value::I32 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, true, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            64 => Value::I64 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, true, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            128 => Value::I128 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, true, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            _ => unreachable!(),
                        }
                    }))
                })
                .handle_unsigned_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::U8 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, false, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            16 => Value::U16 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, false, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            32 => Value::U32 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, false, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            64 => Value::U64 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, false, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            128 => Value::U128 {
                                value: if right {
                                    self.last_block
                                        .builder
                                        .build_right_shift(lhs_value, rhs_value, false, "RSH")
                                } else {
                                    self.last_block
                                        .builder
                                        .build_left_shift(lhs_value, rhs_value, "LSH")
                                },
                            },
                            _ => unreachable!(),
                        }
                    }))
                }),
        )
    }

    pub fn generate_code_expression_op_bit_or(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let mut rhs = self.generate_code_expression(context, module, &ast.children[2]);

        let matched = self.match_type(context, lhs, rhs);

        match matched.0.get_type() {
            ValueType::Void
            | ValueType::Bool
            | ValueType::F16
            | ValueType::F32
            | ValueType::F64
            | ValueType::Str => panic!(
                "type error; bitwise operations between {}s are not allowed.",
                matched.0.get_type()
            ),
            _ => (),
        }

        lhs = matched.0;
        rhs = matched.1;

        lhs.invoke_handler(
            ValueHandler::new()
                .handle_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::I8 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            16 => Value::I16 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            32 => Value::I32 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            64 => Value::I64 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            128 => Value::I128 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            _ => unreachable!(),
                        }
                    }))
                })
                .handle_unsigned_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::U8 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            16 => Value::U16 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            32 => Value::U32 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            64 => Value::U64 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            128 => Value::U128 {
                                value: self.last_block.builder.build_or(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE OR",
                                ),
                            },
                            _ => unreachable!(),
                        }
                    }))
                }),
        )
    }

    pub fn generate_code_expression_op_bit_and(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let mut rhs = self.generate_code_expression(context, module, &ast.children[2]);

        let matched = self.match_type(context, lhs, rhs);

        match matched.0.get_type() {
            ValueType::Void
            | ValueType::Bool
            | ValueType::F16
            | ValueType::F32
            | ValueType::F64
            | ValueType::Str => panic!(
                "type error; bitwise operations between {}s are not allowed.",
                matched.0.get_type()
            ),
            _ => (),
        }

        lhs = matched.0;
        rhs = matched.1;

        lhs.invoke_handler(
            ValueHandler::new()
                .handle_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::I8 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            16 => Value::I16 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            32 => Value::I32 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            64 => Value::I64 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            128 => Value::I128 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            _ => unreachable!(),
                        }
                    }))
                })
                .handle_unsigned_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::U8 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            16 => Value::U16 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            32 => Value::U32 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            64 => Value::U64 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            128 => Value::U128 {
                                value: self.last_block.builder.build_and(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE AND",
                                ),
                            },
                            _ => unreachable!(),
                        }
                    }))
                }),
        )
    }

    pub fn generate_code_expression_op_bit_xor(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut lhs = self.generate_code_expression(context, module, &ast.children[0]);
        let mut rhs = self.generate_code_expression(context, module, &ast.children[2]);

        let matched = self.match_type(context, lhs, rhs);

        match matched.0.get_type() {
            ValueType::Void
            | ValueType::Bool
            | ValueType::F16
            | ValueType::F32
            | ValueType::F64
            | ValueType::Str => panic!(
                "type error; bitwise operations between {}s are not allowed.",
                matched.0.get_type()
            ),
            _ => (),
        }

        lhs = matched.0;
        rhs = matched.1;

        lhs.invoke_handler(
            ValueHandler::new()
                .handle_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::I8 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            16 => Value::I16 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            32 => Value::I32 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            64 => Value::I64 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            128 => Value::I128 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            _ => unreachable!(),
                        }
                    }))
                })
                .handle_unsigned_int(&|_, lhs_value| {
                    rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
                        match rhs.get_type().get_bitwidth() {
                            8 => Value::U8 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            16 => Value::U16 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            32 => Value::U32 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            64 => Value::U64 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            128 => Value::U128 {
                                value: self.last_block.builder.build_xor(
                                    lhs_value,
                                    rhs_value,
                                    "BITWISE XOR",
                                ),
                            },
                            _ => unreachable!(),
                        }
                    }))
                }),
        )
    }

    pub fn generate_code_expression_op_cast(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let cast_type = ast.children[2].children[0]
            .child
            .as_ref()
            .unwrap()
            .token_type;

        if cast_type == TokenType::KeywordVoid {
            panic!("type error; unable to perform cast to {}s", ValueType::Void);
        }

        let operand = self.generate_code_expression(context, module, &ast.children[0]);

        if operand.get_type() == ValueType::Void {
            panic!(
                "type error; unable to perform cast from {}s.",
                ValueType::Void
            );
        }

        operand.invoke_handler(
            ValueHandler::new()
                .handle_bool(&|_, value| match cast_type {
                    TokenType::KeywordBool => operand,
                    TokenType::KeywordI8 => Value::I8 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i8_type(),
                            "CAST -> i8",
                        ),
                    },
                    TokenType::KeywordI16 => Value::I16 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i16_type(),
                            "CAST -> i16",
                        ),
                    },
                    TokenType::KeywordI32 => Value::I32 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i32_type(),
                            "CAST -> i32",
                        ),
                    },
                    TokenType::KeywordI64 => Value::I64 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i64_type(),
                            "CAST -> i64",
                        ),
                    },
                    TokenType::KeywordI128 => Value::I128 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i128_type(),
                            "CAST -> i128",
                        ),
                    },
                    TokenType::KeywordU8 => Value::U8 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i8_type(),
                            "CAST -> u8",
                        ),
                    },
                    TokenType::KeywordU16 => Value::U16 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i16_type(),
                            "CAST -> u16",
                        ),
                    },
                    TokenType::KeywordU32 => Value::U32 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i32_type(),
                            "CAST -> u32",
                        ),
                    },
                    TokenType::KeywordU64 => Value::U64 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i64_type(),
                            "CAST -> u64",
                        ),
                    },
                    TokenType::KeywordU128 => Value::U128 {
                        value: self.last_block.builder.build_int_z_extend(
                            value,
                            context.i128_type(),
                            "CAST -> u128",
                        ),
                    },
                    TokenType::KeywordF16 => Value::F16 {
                        value: self.last_block.builder.build_unsigned_int_to_float(
                            value,
                            context.f16_type(),
                            "CAST -> f16",
                        ),
                    },
                    TokenType::KeywordF32 => Value::F32 {
                        value: self.last_block.builder.build_unsigned_int_to_float(
                            value,
                            context.f32_type(),
                            "CAST -> f32",
                        ),
                    },
                    TokenType::KeywordF64 => Value::F64 {
                        value: self.last_block.builder.build_unsigned_int_to_float(
                            value,
                            context.f64_type(),
                            "CAST -> f64",
                        ),
                    },
                    TokenType::KeywordStr => panic!(
                        "type error; unable to cast {}s to {}s.",
                        ValueType::Bool,
                        ValueType::Str
                    ),
                    _ => unreachable!(),
                })
                .handle_int(&|_, value| match cast_type {
                    TokenType::KeywordBool => Value::Bool {
                        value: self.last_block.builder.build_int_compare(
                            IntPredicate::NE,
                            value,
                            match operand.get_type().get_bitwidth() {
                                8 => context.i8_type().const_int(0, false),
                                16 => context.i16_type().const_int(0, false),
                                32 => context.i32_type().const_int(0, false),
                                64 => context.i64_type().const_int(0, false),
                                128 => context.i128_type().const_int(0, false),
                                _ => unreachable!(),
                            },
                            "CAST -> bool",
                        ),
                    },
                    TokenType::KeywordI8 => {
                        if operand.get_type().get_bitwidth() == 8 {
                            operand
                        } else {
                            Value::I8 {
                                value: self.last_block.builder.build_int_truncate(
                                    value,
                                    context.i8_type(),
                                    "CAST -> i8",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordI16 => {
                        if operand.get_type().get_bitwidth() == 16 {
                            operand
                        } else if operand.get_type().get_bitwidth() < 16 {
                            Value::I16 {
                                value: self.last_block.builder.build_int_s_extend(
                                    value,
                                    context.i16_type(),
                                    "CAST -> i16",
                                ),
                            }
                        } else {
                            Value::I16 {
                                value: self.last_block.builder.build_int_truncate(
                                    value,
                                    context.i16_type(),
                                    "CAST -> i16",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordI32 => {
                        if operand.get_type().get_bitwidth() == 32 {
                            operand
                        } else if operand.get_type().get_bitwidth() < 32 {
                            Value::I32 {
                                value: self.last_block.builder.build_int_s_extend(
                                    value,
                                    context.i32_type(),
                                    "CAST -> i32",
                                ),
                            }
                        } else {
                            Value::I32 {
                                value: self.last_block.builder.build_int_truncate(
                                    value,
                                    context.i32_type(),
                                    "CAST -> i32",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordI64 => {
                        if operand.get_type().get_bitwidth() == 64 {
                            operand
                        } else if operand.get_type().get_bitwidth() < 64 {
                            Value::I64 {
                                value: self.last_block.builder.build_int_s_extend(
                                    value,
                                    context.i64_type(),
                                    "CAST -> i64",
                                ),
                            }
                        } else {
                            Value::I64 {
                                value: self.last_block.builder.build_int_truncate(
                                    value,
                                    context.i64_type(),
                                    "CAST -> i64",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordI128 => {
                        if operand.get_type().get_bitwidth() == 128 {
                            operand
                        } else {
                            Value::I128 {
                                value: self.last_block.builder.build_int_s_extend(
                                    value,
                                    context.i128_type(),
                                    "CAST -> i128",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordU8 => Value::U8 {
                        value: if operand.get_type().get_bitwidth() == 8 {
                            value
                        } else {
                            self.last_block.builder.build_int_truncate(
                                value,
                                context.i8_type(),
                                "CAST -> u8",
                            )
                        },
                    },
                    TokenType::KeywordU16 => Value::U16 {
                        value: if operand.get_type().get_bitwidth() == 16 {
                            value
                        } else if operand.get_type().get_bitwidth() < 16 {
                            self.last_block.builder.build_int_s_extend(
                                value,
                                context.i16_type(),
                                "CAST -> u16",
                            )
                        } else {
                            self.last_block.builder.build_int_truncate(
                                value,
                                context.i16_type(),
                                "CAST -> u16",
                            )
                        },
                    },
                    TokenType::KeywordU32 => Value::U32 {
                        value: if operand.get_type().get_bitwidth() == 32 {
                            value
                        } else if operand.get_type().get_bitwidth() < 32 {
                            self.last_block.builder.build_int_s_extend(
                                value,
                                context.i32_type(),
                                "CAST -> u32",
                            )
                        } else {
                            self.last_block.builder.build_int_truncate(
                                value,
                                context.i32_type(),
                                "CAST -> u32",
                            )
                        },
                    },
                    TokenType::KeywordU64 => Value::U64 {
                        value: if operand.get_type().get_bitwidth() == 64 {
                            value
                        } else if operand.get_type().get_bitwidth() < 64 {
                            self.last_block.builder.build_int_s_extend(
                                value,
                                context.i64_type(),
                                "CAST -> u64",
                            )
                        } else {
                            self.last_block.builder.build_int_truncate(
                                value,
                                context.i64_type(),
                                "CAST -> u64",
                            )
                        },
                    },
                    TokenType::KeywordU128 => Value::U128 {
                        value: if operand.get_type().get_bitwidth() == 128 {
                            value
                        } else {
                            self.last_block.builder.build_int_s_extend(
                                value,
                                context.i128_type(),
                                "CAST -> u128",
                            )
                        },
                    },
                    TokenType::KeywordF16 => Value::F16 {
                        value: self.last_block.builder.build_signed_int_to_float(
                            value,
                            context.f16_type(),
                            "CAST -> f16",
                        ),
                    },
                    TokenType::KeywordF32 => Value::F32 {
                        value: self.last_block.builder.build_signed_int_to_float(
                            value,
                            context.f32_type(),
                            "CAST -> f32",
                        ),
                    },
                    TokenType::KeywordF64 => Value::F64 {
                        value: self.last_block.builder.build_signed_int_to_float(
                            value,
                            context.f64_type(),
                            "CAST -> f64",
                        ),
                    },
                    TokenType::KeywordStr => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::Str
                    ),
                    _ => unreachable!(),
                })
                .handle_unsigned_int(&|_, value| match cast_type {
                    TokenType::KeywordBool => Value::Bool {
                        value: self.last_block.builder.build_int_compare(
                            IntPredicate::NE,
                            value,
                            match operand.get_type().get_bitwidth() {
                                8 => context.i8_type().const_int(0, false),
                                16 => context.i16_type().const_int(0, false),
                                32 => context.i32_type().const_int(0, false),
                                64 => context.i64_type().const_int(0, false),
                                128 => context.i128_type().const_int(0, false),
                                _ => unreachable!(),
                            },
                            "CAST -> bool",
                        ),
                    },
                    TokenType::KeywordI8 => Value::I8 {
                        value: if operand.get_type().get_bitwidth() == 8 {
                            value
                        } else {
                            self.last_block.builder.build_int_truncate(
                                value,
                                context.i8_type(),
                                "CAST -> u8",
                            )
                        },
                    },
                    TokenType::KeywordI16 => Value::I16 {
                        value: if operand.get_type().get_bitwidth() == 16 {
                            value
                        } else if operand.get_type().get_bitwidth() < 16 {
                            self.last_block.builder.build_int_z_extend(
                                value,
                                context.i16_type(),
                                "CAST -> i16",
                            )
                        } else {
                            self.last_block.builder.build_int_truncate(
                                value,
                                context.i16_type(),
                                "CAST -> i16",
                            )
                        },
                    },
                    TokenType::KeywordI32 => Value::I32 {
                        value: if operand.get_type().get_bitwidth() == 32 {
                            value
                        } else if operand.get_type().get_bitwidth() < 32 {
                            self.last_block.builder.build_int_z_extend(
                                value,
                                context.i32_type(),
                                "CAST -> i32",
                            )
                        } else {
                            self.last_block.builder.build_int_truncate(
                                value,
                                context.i32_type(),
                                "CAST -> i32",
                            )
                        },
                    },
                    TokenType::KeywordI64 => Value::I64 {
                        value: if operand.get_type().get_bitwidth() == 64 {
                            value
                        } else if operand.get_type().get_bitwidth() < 64 {
                            self.last_block.builder.build_int_z_extend(
                                value,
                                context.i64_type(),
                                "CAST -> i64",
                            )
                        } else {
                            self.last_block.builder.build_int_truncate(
                                value,
                                context.i64_type(),
                                "CAST -> i64",
                            )
                        },
                    },
                    TokenType::KeywordI128 => Value::I128 {
                        value: if operand.get_type().get_bitwidth() == 128 {
                            value
                        } else {
                            self.last_block.builder.build_int_z_extend(
                                value,
                                context.i128_type(),
                                "CAST -> i128",
                            )
                        },
                    },
                    TokenType::KeywordU8 => {
                        if operand.get_type().get_bitwidth() == 8 {
                            operand
                        } else {
                            Value::U8 {
                                value: self.last_block.builder.build_int_truncate(
                                    value,
                                    context.i8_type(),
                                    "CAST -> u8",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordU16 => {
                        if operand.get_type().get_bitwidth() == 16 {
                            operand
                        } else if operand.get_type().get_bitwidth() < 16 {
                            Value::U16 {
                                value: self.last_block.builder.build_int_s_extend(
                                    value,
                                    context.i16_type(),
                                    "CAST -> u16",
                                ),
                            }
                        } else {
                            Value::U16 {
                                value: self.last_block.builder.build_int_truncate(
                                    value,
                                    context.i16_type(),
                                    "CAST -> u16",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordU32 => {
                        if operand.get_type().get_bitwidth() == 32 {
                            operand
                        } else if operand.get_type().get_bitwidth() < 32 {
                            Value::U32 {
                                value: self.last_block.builder.build_int_s_extend(
                                    value,
                                    context.i32_type(),
                                    "CAST -> u32",
                                ),
                            }
                        } else {
                            Value::U32 {
                                value: self.last_block.builder.build_int_truncate(
                                    value,
                                    context.i32_type(),
                                    "CAST -> u32",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordU64 => {
                        if operand.get_type().get_bitwidth() == 64 {
                            operand
                        } else if operand.get_type().get_bitwidth() < 64 {
                            Value::U64 {
                                value: self.last_block.builder.build_int_s_extend(
                                    value,
                                    context.i64_type(),
                                    "CAST -> u64",
                                ),
                            }
                        } else {
                            Value::U64 {
                                value: self.last_block.builder.build_int_truncate(
                                    value,
                                    context.i64_type(),
                                    "CAST -> u64",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordU128 => {
                        if operand.get_type().get_bitwidth() == 128 {
                            operand
                        } else {
                            Value::U128 {
                                value: self.last_block.builder.build_int_s_extend(
                                    value,
                                    context.i128_type(),
                                    "CAST -> u128",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordF16 => Value::F16 {
                        value: self.last_block.builder.build_unsigned_int_to_float(
                            value,
                            context.f16_type(),
                            "CAST -> f16",
                        ),
                    },
                    TokenType::KeywordF32 => Value::F32 {
                        value: self.last_block.builder.build_unsigned_int_to_float(
                            value,
                            context.f32_type(),
                            "CAST -> f32",
                        ),
                    },
                    TokenType::KeywordF64 => Value::F64 {
                        value: self.last_block.builder.build_unsigned_int_to_float(
                            value,
                            context.f64_type(),
                            "CAST -> f64",
                        ),
                    },
                    TokenType::KeywordStr => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::Str
                    ),
                    _ => unreachable!(),
                })
                .handle_float(&|_, value| match cast_type {
                    TokenType::KeywordBool => Value::Bool {
                        value: self.last_block.builder.build_float_compare(
                            FloatPredicate::ONE,
                            value,
                            match operand.get_type().get_bitwidth() {
                                16 => context.f16_type().const_float(0.0),
                                32 => context.f32_type().const_float(0.0),
                                64 => context.f64_type().const_float(0.0),
                                _ => unreachable!(),
                            },
                            "CAST -> bool",
                        ),
                    },
                    TokenType::KeywordI8 => Value::I8 {
                        value: self.last_block.builder.build_float_to_signed_int(
                            value,
                            context.i8_type(),
                            "CAST -> i8",
                        ),
                    },
                    TokenType::KeywordI16 => Value::I16 {
                        value: self.last_block.builder.build_float_to_signed_int(
                            value,
                            context.i16_type(),
                            "CAST -> i16",
                        ),
                    },
                    TokenType::KeywordI32 => Value::I32 {
                        value: self.last_block.builder.build_float_to_signed_int(
                            value,
                            context.i32_type(),
                            "CAST -> i32",
                        ),
                    },
                    TokenType::KeywordI64 => Value::I64 {
                        value: self.last_block.builder.build_float_to_signed_int(
                            value,
                            context.i64_type(),
                            "CAST -> i64",
                        ),
                    },
                    TokenType::KeywordI128 => Value::I128 {
                        value: self.last_block.builder.build_float_to_signed_int(
                            value,
                            context.i128_type(),
                            "CAST -> i128",
                        ),
                    },
                    TokenType::KeywordU8 => Value::U8 {
                        value: self.last_block.builder.build_float_to_unsigned_int(
                            value,
                            context.i8_type(),
                            "CAST -> u8",
                        ),
                    },
                    TokenType::KeywordU16 => Value::U16 {
                        value: self.last_block.builder.build_float_to_unsigned_int(
                            value,
                            context.i16_type(),
                            "CAST -> u16",
                        ),
                    },
                    TokenType::KeywordU32 => Value::U32 {
                        value: self.last_block.builder.build_float_to_unsigned_int(
                            value,
                            context.i32_type(),
                            "CAST -> u32",
                        ),
                    },
                    TokenType::KeywordU64 => Value::U64 {
                        value: self.last_block.builder.build_float_to_unsigned_int(
                            value,
                            context.i64_type(),
                            "CAST -> u64",
                        ),
                    },
                    TokenType::KeywordU128 => Value::U128 {
                        value: self.last_block.builder.build_float_to_unsigned_int(
                            value,
                            context.i128_type(),
                            "CAST -> u128",
                        ),
                    },
                    TokenType::KeywordF16 => {
                        if operand.get_type().get_bitwidth() == 16 {
                            operand
                        } else {
                            Value::F16 {
                                value: self.last_block.builder.build_float_trunc(
                                    value,
                                    context.f16_type(),
                                    "CAST -> f16",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordF32 => {
                        if operand.get_type().get_bitwidth() == 32 {
                            operand
                        } else if operand.get_type().get_bitwidth() < 32 {
                            Value::F32 {
                                value: self.last_block.builder.build_float_ext(
                                    value,
                                    context.f32_type(),
                                    "CAST -> f32",
                                ),
                            }
                        } else {
                            Value::F32 {
                                value: self.last_block.builder.build_float_trunc(
                                    value,
                                    context.f32_type(),
                                    "CAST -> f32",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordF64 => {
                        if operand.get_type().get_bitwidth() == 64 {
                            operand
                        } else {
                            Value::F64 {
                                value: self.last_block.builder.build_float_ext(
                                    value,
                                    context.f64_type(),
                                    "CAST -> f64",
                                ),
                            }
                        }
                    }
                    TokenType::KeywordStr => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::Str
                    ),
                    _ => unreachable!(),
                })
                .handle_str(&|_, _| match cast_type {
                    TokenType::KeywordBool => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::Bool
                    ),
                    TokenType::KeywordI8 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::I8
                    ),
                    TokenType::KeywordI16 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::I16
                    ),
                    TokenType::KeywordI32 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::I32
                    ),
                    TokenType::KeywordI64 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::I64
                    ),
                    TokenType::KeywordI128 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::I128
                    ),
                    TokenType::KeywordU8 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::U8
                    ),
                    TokenType::KeywordU16 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::U16
                    ),
                    TokenType::KeywordU32 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::U32
                    ),
                    TokenType::KeywordU64 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::U64
                    ),
                    TokenType::KeywordU128 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::U128
                    ),
                    TokenType::KeywordF16 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::F16
                    ),
                    TokenType::KeywordF32 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::F32
                    ),
                    TokenType::KeywordF64 => panic!(
                        "type error; unable to cast {}s to {}s.",
                        operand.get_type(),
                        ValueType::F64
                    ),
                    TokenType::KeywordStr => operand,
                    _ => unreachable!(),
                }),
        )
    }

    pub fn generate_code_expression_op_single(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        match ast.children[0].child.as_ref().unwrap().token_type {
            TokenType::KeywordFrom => unimplemented!(), // TODO: Implement the from syntax.
            TokenType::ParenL => self.generate_code_expression(context, module, &ast.children[1]),
            TokenType::OpAdd => self.generate_code_expression(context, module, &ast.children[1]),
            TokenType::OpSub => self.generate_code_expression_op_neg(context, module, &ast),
            TokenType::OpNot => self.generate_code_expression_op_not(context, module, &ast),
            TokenType::OpBitNot => self.generate_code_expression_op_bit_not(context, module, &ast),
            _ => unreachable!(),
        }
    }

    pub fn generate_code_expression_op_neg(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let lhs = self.generate_code_expression(context, module, &ast.children[1]);

        match lhs.get_type() {
            ValueType::Void
            | ValueType::Bool
            | ValueType::U8
            | ValueType::U16
            | ValueType::U32
            | ValueType::U64
            | ValueType::U128
            | ValueType::Str => panic!(
                "type error; unary negation operations between {}s are not allowed.",
                lhs.get_type()
            ),
            _ => (),
        }

        lhs.invoke_handler(
            ValueHandler::new()
                .handle_int(&|_, lhs_value| match lhs.get_type().get_bitwidth() {
                    8 => Value::I8 {
                        value: self.last_block.builder.build_int_nsw_sub(
                            context.i8_type().const_int(0, false),
                            lhs_value,
                            "NEG",
                        ),
                    },
                    16 => Value::I16 {
                        value: self.last_block.builder.build_int_nsw_sub(
                            context.i16_type().const_int(0, false),
                            lhs_value,
                            "NEG",
                        ),
                    },
                    32 => Value::I32 {
                        value: self.last_block.builder.build_int_nsw_sub(
                            context.i32_type().const_int(0, false),
                            lhs_value,
                            "NEG",
                        ),
                    },
                    64 => Value::I64 {
                        value: self.last_block.builder.build_int_nsw_sub(
                            context.i64_type().const_int(0, false),
                            lhs_value,
                            "NEG",
                        ),
                    },
                    128 => Value::I128 {
                        value: self.last_block.builder.build_int_nsw_sub(
                            context.i128_type().const_int(0, false),
                            lhs_value,
                            "NEG",
                        ),
                    },
                    _ => unreachable!(),
                })
                .handle_float(&|_, lhs_value| match lhs.get_type().get_bitwidth() {
                    16 => Value::F16 {
                        value: self.last_block.builder.build_float_neg(lhs_value, "NEG"),
                    },
                    32 => Value::F32 {
                        value: self.last_block.builder.build_float_neg(lhs_value, "NEG"),
                    },
                    64 => Value::F64 {
                        value: self.last_block.builder.build_float_neg(lhs_value, "NEG"),
                    },
                    _ => unreachable!(),
                }),
        )
    }

    pub fn generate_code_expression_op_not(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let lhs = self.generate_code_expression(context, module, &ast.children[1]);

        if lhs.get_type() != ValueType::Bool {
            panic!(
                "type error; boolean operations are only allowed between {}s.",
                ValueType::Bool
            );
        }

        let result = lhs.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
            self.last_block.builder.build_int_compare(
                IntPredicate::EQ,
                value,
                context.bool_type().const_int(0, false),
                "NOT",
            )
        }));

        Value::Bool { value: result }
    }

    pub fn generate_code_expression_op_bit_not(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let lhs = self.generate_code_expression(context, module, &ast.children[1]);

        match lhs.get_type() {
            ValueType::Void
            | ValueType::Bool
            | ValueType::F16
            | ValueType::F32
            | ValueType::F64
            | ValueType::Str => panic!(
                "type error; bitwise operations between {}s are not allowed.",
                lhs.get_type()
            ),
            _ => (),
        }

        lhs.invoke_handler(
            ValueHandler::new()
                .handle_int(&|_, lhs_value| match lhs.get_type().get_bitwidth() {
                    8 => Value::I8 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    16 => Value::I16 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    32 => Value::I32 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    64 => Value::I64 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    128 => Value::I128 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    _ => unreachable!(),
                })
                .handle_unsigned_int(&|_, lhs_value| match lhs.get_type().get_bitwidth() {
                    8 => Value::U8 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    16 => Value::U16 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    32 => Value::U32 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    64 => Value::U64 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    128 => Value::U128 {
                        value: self.last_block.builder.build_not(lhs_value, "BITWISE NOT"),
                    },
                    _ => unreachable!(),
                }),
        )
    }

    fn generate_code_expression_function_call(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let mut parameter_value_vec: Vec<Value<'ctx>> = Vec::new();

        if ast.children.len() == 4 {
            let mut parameter_stack: Vec<&AST> = ast.children[2].children.iter().rev().collect();

            while parameter_stack.last().unwrap().name == "function-call-argument-list" {
                let mut parameter_vec: Vec<&AST> = parameter_stack
                    .pop()
                    .unwrap()
                    .children
                    .iter()
                    .rev()
                    .collect();

                parameter_stack.append(&mut parameter_vec);
            }

            parameter_value_vec = parameter_stack
                .iter()
                .rev()
                .filter(|ast| ast.name == "expression") // Ignores "Comma"s.
                .map(|ast| self.generate_code_expression(context, module, ast))
                .collect();
        }

        let function_name = &ast.children[0].child.as_ref().unwrap().token_content;
        let function = match module.function_prototype_table.get(function_name) {
            Some(function) => function,
            None => {
                panic!(
                    "unknown function detected; unable to find {} function.",
                    function_name
                );
            }
        };

        if function.variadic_param && parameter_value_vec.len() < function.param_type_vec.len() {
            panic!(
					"wrong argument passed; {} function takes at least {} arguments, but {} was supplied.",
					function_name,
					function.param_type_vec.len(),
					parameter_value_vec.len()
				);
        } else if !function.variadic_param
            && parameter_value_vec.len() != function.param_type_vec.len()
        {
            panic!(
                "wrong argument passed; {} function takes {} arguments, but {} was supplied.",
                function_name,
                function.param_type_vec.len(),
                parameter_value_vec.len()
            );
        }

        for param_index in 0..function.param_type_vec.len() {
            if parameter_value_vec[param_index].get_type() != function.param_type_vec[param_index] {
                panic!(
					"wrong argument passed; {} function takes {} type as {}th argument, but {} type was supplied.",
					function_name,
					function.param_type_vec[param_index],
					param_index + 1,
					parameter_value_vec[param_index].get_type()
				);
            }
        }

        let result = self.last_block.builder.build_call(
            function.function,
            &parameter_value_vec
                .iter()
                .map(|value| value.to_basic_value())
                .collect::<Vec<BasicValueEnum<'ctx>>>(),
            "CALL",
        );

        match result.try_as_basic_value() {
            Either::Left(basic_value) => Value::from_basic_value(function.return_type, basic_value),
            Either::Right(_) => Value::Void,
        }
    }

    fn generate_code_expression_left_value(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let variable_name = &ast.children[0].child.as_ref().unwrap().token_content;
        let variable = self.resolve_variable(variable_name);

        Value::from_basic_value(
            variable.0,
            self.last_block.builder.build_load(variable.1, "LOAD"),
        )
    }

    fn generate_code_expression_literal(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> Value<'ctx> {
        let content = &ast.children[0].child.as_ref().unwrap().token_content;

        match ast.children[0].name.as_ref() {
            "LiteralBool" => Value::Bool {
                value: context.bool_type().const_int(
                    if content.parse::<bool>().unwrap() {
                        1
                    } else {
                        0
                    },
                    false,
                ),
            },
            "LiteralInteger" => Value::I32 {
                value: context.i32_type().const_int(
                    if content.starts_with("+") {
                        content[1..].parse::<u64>().unwrap()
                    } else if content.starts_with("-") {
                        !content[1..].parse::<u64>().unwrap() + 1
                    } else {
                        content.parse::<u64>().unwrap()
                    },
                    true,
                ),
            },
            "LiteralDecimal" => Value::F32 {
                value: context
                    .f32_type()
                    .const_float(content.parse::<f64>().unwrap()),
            },
            "LiteralString" => Value::Str {
                value: self
                    .last_block
                    .builder
                    .build_global_string_ptr(content, "str literal")
                    .as_pointer_value(),
            },
            _ => unreachable!(),
        }
    }

    pub fn match_type(
        &'fnc mut self,
        context: &'ctx Context,
        lhs: Value<'ctx>,
        rhs: Value<'ctx>,
    ) -> (Value<'ctx>, Value<'ctx>) {
        let mut lhs = lhs;
        let mut rhs = rhs;

        if lhs.get_type().get_group() != rhs.get_type().get_group() {
            panic!(
                "incompatible type; unable to match {} and {}",
                lhs.get_type(),
                rhs.get_type()
            );
        }

        let perform_cast = |from: Value<'ctx>, to: ValueType| -> Value<'ctx> {
            from.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, value| match to {
                        ValueType::I16 => Value::I16 {
                            value: self.last_block.builder.build_int_s_extend(
                                value,
                                context.i16_type(),
                                "CAST -> i16",
                            ),
                        },
                        ValueType::I32 => Value::I32 {
                            value: self.last_block.builder.build_int_s_extend(
                                value,
                                context.i32_type(),
                                "CAST -> i32",
                            ),
                        },
                        ValueType::I64 => Value::I64 {
                            value: self.last_block.builder.build_int_s_extend(
                                value,
                                context.i64_type(),
                                "CAST -> i64",
                            ),
                        },
                        ValueType::I128 => Value::I128 {
                            value: self.last_block.builder.build_int_s_extend(
                                value,
                                context.i128_type(),
                                "CAST -> i128",
                            ),
                        },
                        _ => unreachable!(),
                    })
                    .handle_unsigned_int(&|_, value| match to {
                        ValueType::U16 => Value::U16 {
                            value: self.last_block.builder.build_int_z_extend(
                                value,
                                context.i16_type(),
                                "CAST -> u16",
                            ),
                        },
                        ValueType::U32 => Value::U32 {
                            value: self.last_block.builder.build_int_z_extend(
                                value,
                                context.i32_type(),
                                "CAST -> u32",
                            ),
                        },
                        ValueType::U64 => Value::U64 {
                            value: self.last_block.builder.build_int_z_extend(
                                value,
                                context.i64_type(),
                                "CAST -> u64",
                            ),
                        },
                        ValueType::U128 => Value::U128 {
                            value: self.last_block.builder.build_int_z_extend(
                                value,
                                context.i128_type(),
                                "CAST -> u128",
                            ),
                        },
                        _ => unreachable!(),
                    })
                    .handle_float(&|_, value| match to {
                        ValueType::F32 => Value::F32 {
                            value: self.last_block.builder.build_float_ext(
                                value,
                                context.f32_type(),
                                "CAST -> f32",
                            ),
                        },
                        ValueType::F64 => Value::F64 {
                            value: self.last_block.builder.build_float_ext(
                                value,
                                context.f64_type(),
                                "CAST -> f64",
                            ),
                        },
                        _ => unreachable!(),
                    }),
            )
        };

        if lhs.get_type().get_bitwidth() < rhs.get_type().get_bitwidth() {
            lhs = perform_cast(lhs, rhs.get_type());
        } else if lhs.get_type().get_bitwidth() > rhs.get_type().get_bitwidth() {
            rhs = perform_cast(rhs, lhs.get_type());
        }

        (lhs, rhs)
    }

    pub fn match_type_single(
        &'fnc mut self,
        context: &'ctx Context,
        value: Value<'ctx>,
        value_type: ValueType,
    ) -> Value<'ctx> {
        if value.get_type().get_group() != value_type.get_group() {
            panic!(
                "incompatible type; unable to match {} and {}",
                value.get_type(),
                value_type
            );
        }

        let perform_cast = |from: Value<'ctx>, to: ValueType| -> Value<'ctx> {
            from.invoke_handler(
                ValueHandler::new()
                    .handle_int(&|_, value| match to {
                        ValueType::I16 => Value::I16 {
                            value: self.last_block.builder.build_int_s_extend(
                                value,
                                context.i16_type(),
                                "CAST -> i16",
                            ),
                        },
                        ValueType::I32 => Value::I32 {
                            value: self.last_block.builder.build_int_s_extend(
                                value,
                                context.i32_type(),
                                "CAST -> i32",
                            ),
                        },
                        ValueType::I64 => Value::I64 {
                            value: self.last_block.builder.build_int_s_extend(
                                value,
                                context.i64_type(),
                                "CAST -> i64",
                            ),
                        },
                        ValueType::I128 => Value::I128 {
                            value: self.last_block.builder.build_int_s_extend(
                                value,
                                context.i128_type(),
                                "CAST -> i128",
                            ),
                        },
                        _ => unreachable!(),
                    })
                    .handle_unsigned_int(&|_, value| match to {
                        ValueType::U16 => Value::U16 {
                            value: self.last_block.builder.build_int_z_extend(
                                value,
                                context.i16_type(),
                                "CAST -> u16",
                            ),
                        },
                        ValueType::U32 => Value::U32 {
                            value: self.last_block.builder.build_int_z_extend(
                                value,
                                context.i32_type(),
                                "CAST -> u32",
                            ),
                        },
                        ValueType::U64 => Value::U64 {
                            value: self.last_block.builder.build_int_z_extend(
                                value,
                                context.i64_type(),
                                "CAST -> u64",
                            ),
                        },
                        ValueType::U128 => Value::U128 {
                            value: self.last_block.builder.build_int_z_extend(
                                value,
                                context.i128_type(),
                                "CAST -> u128",
                            ),
                        },
                        _ => unreachable!(),
                    })
                    .handle_float(&|_, value| match to {
                        ValueType::F32 => Value::F32 {
                            value: self.last_block.builder.build_float_ext(
                                value,
                                context.f32_type(),
                                "CAST -> f32",
                            ),
                        },
                        ValueType::F64 => Value::F64 {
                            value: self.last_block.builder.build_float_ext(
                                value,
                                context.f64_type(),
                                "CAST -> f64",
                            ),
                        },
                        _ => unreachable!(),
                    }),
            )
        };

        if value.get_type().get_bitwidth() < value_type.get_bitwidth() {
            perform_cast(value, value_type)
        } else if value.get_type().get_bitwidth() > value_type.get_bitwidth() {
            panic!(
                "explicit cast required; casting from {} to {} may alter its value",
                value.get_type(),
                value_type
            );
        } else {
            value
        }
    }

    pub fn get_left_value(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl mut ModuleGen<'ctx>,
        ast: &AST,
    ) -> (ValueType, PointerValue<'ctx>) {
        // Currently, we only have Ids for left-values.
        return self.resolve_variable(&ast.children[0].child.as_ref().unwrap().token_content);
    }
}
