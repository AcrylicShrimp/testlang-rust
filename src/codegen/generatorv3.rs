extern crate either;
extern crate inkwell;

use super::value::{Value, ValueHandler, ValueType, ValueTypeHandler};
use crate::lexer::TokenType;
use crate::parser::AST;

use either::Either;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use std::boxed::Box;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::vec::Vec;

pub struct Generator<'ctx> {
    pub context: Context,
    pub module_map: HashMap<String, ModuleGen<'ctx>>,
}

pub struct ModuleGen<'ctx> {
    pub name: String,
    pub module: Module<'ctx>,
    pub function_prototype_table: HashMap<String, FuncPrototype<'ctx>>,
}

#[derive(Clone)]
pub struct FuncPrototype<'ctx> {
    pub return_type: ValueType,
    pub param_type_vec: Vec<ValueType>,
    pub variadic_param: bool,
    pub function: FunctionValue<'ctx>,
}

pub struct FuncGen<'ctx> {
    pub function: FunctionValue<'ctx>,
    pub prototype: FuncPrototype<'ctx>,
    pub scope_stack: Vec<ScopeGen<'ctx>>,
    pub loop_statement_entry_stack: Vec<BasicBlock>,
    pub loop_statement_exit_stack: Vec<BasicBlock>,
    pub last_block: BlockGen<'ctx>,
}

pub struct BlockGen<'ctx> {
    pub block: BasicBlock,
    pub builder: Builder<'ctx>,
}

pub struct ScopeGen<'ctx> {
    pub variable_map: HashMap<String, (ValueType, PointerValue<'ctx>)>,
    pub stack_position: BasicValueEnum<'ctx>,
}

impl<'mdl, 'ctx: 'mdl> Generator<'ctx> {
    pub fn new() -> Generator<'ctx> {
        Generator {
            context: Context::create(),
            module_map: HashMap::new(),
        }
    }

    pub fn create_module(&'ctx mut self, name: String) -> &'mdl mut ModuleGen<'ctx> {
        if self.module_map.contains_key(&name) {
            panic!("module {} is already exists.", name);
        }

        self.module_map.insert(
            name.clone(),
            ModuleGen {
                name: name.clone(),
                module: self.context.create_module(&name),
                function_prototype_table: HashMap::new(),
            },
        );

        let module = self.module_map.get_mut(&name).unwrap();

        // LLVM Intrinsic functions.
        module.decl_function(
            &self.context,
            "llvm.stacksave".to_owned(),
            ValueType::Str,
            vec![],
            false,
        );
        module.decl_function(
            &self.context,
            "llvm.stackrestore".to_owned(),
            ValueType::Void,
            vec![ValueType::Str],
            false,
        );

        module
    }
}

impl<'fnc, 'mdl: 'fnc, 'ctx: 'mdl> ModuleGen<'ctx> {
    pub fn decl_function(
        &'mdl mut self,
        context: &'ctx Context,
        name: String,
        return_type: ValueType,
        param_type_vec: Vec<ValueType>,
        variadic_param: bool,
    ) -> (FuncPrototype<'ctx>, FunctionValue<'ctx>) {
        let param_basic_type_vec = param_type_vec
            .clone()
            .into_iter()
            .map(|param_type| param_type.to_basic_type(context))
            .collect::<Vec<BasicTypeEnum<'ctx>>>();

        let fn_type = return_type.invoke_handler(
            context,
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

        let function = self.module.add_function(&name, fn_type, None);
        let function_prototype = FuncPrototype {
            return_type: return_type,
            param_type_vec: param_type_vec,
            variadic_param: variadic_param,
            function: function.clone(),
        };

        self.function_prototype_table
            .insert(name.to_owned(), function_prototype.clone());

        (function_prototype, function)
    }

    pub fn create_function(
        &'mdl mut self,
        context: &'ctx Context,
        name: String,
        return_type: ValueType,
        param_type_vec: Vec<ValueType>,
        variadic_param: bool,
    ) -> FuncGen<'ctx> {
        if self.function_prototype_table.contains_key(&name) {
            panic!("function {} is already exists.", name);
        }

        let function_with_prototype =
            self.decl_function(context, name, return_type, param_type_vec, variadic_param);

        let entry_block = context.append_basic_block(function_with_prototype.1, "entry");
        let entry_builder = context.create_builder();
        entry_builder.position_at_end(&entry_block);

        let func_gen = FuncGen {
            function: function_with_prototype.1,
            prototype: function_with_prototype.0,
            scope_stack: Vec::new(),
            loop_statement_entry_stack: Vec::new(),
            loop_statement_exit_stack: Vec::new(),
            last_block: BlockGen {
                block: entry_block,
                builder: entry_builder,
            },
        };

        func_gen
    }
}

impl<'bdr, 'fnc: 'bdr, 'mdl: 'fnc, 'ctx: 'mdl> FuncGen<'ctx> {
    pub fn end_function(self, context: &'ctx Context) {
        if self.scope_stack.len() != 1 {
            panic!("scope not yet closed.");
        }

        if self.loop_statement_entry_stack.len() != 1 {
            panic!("loop statement not yet closed.");
        }

        if self.loop_statement_exit_stack.len() != 1 {
            panic!("loop statement not yet closed.");
        }

        let blocks = self.function.get_basic_blocks();

        if self.prototype.return_type == ValueType::Void
            && blocks.last().unwrap().get_terminator().is_none()
        {
            self.last_block.builder.build_return(None);
        }

        for block in self.function.get_basic_blocks() {
            if block.get_terminator().is_none() {
                panic!("non-return path detected.");
            }
        }

        self.scope_stack.pop();
    }

    pub fn create_scope(
        &'fnc mut self,
        module: &'mdl ModuleGen<'ctx>,
        builder: &'bdr Builder<'ctx>,
    ) {
        self.scope_stack.push(ScopeGen {
            variable_map: HashMap::new(),
            stack_position: builder
                .build_call(
                    module
                        .function_prototype_table
                        .get("llvm.stacksave")
                        .unwrap()
                        .function,
                    vec![].as_slice(),
                    "stacksave",
                )
                .try_as_basic_value()
                .left()
                .unwrap(),
        });
    }

    pub fn end_scope(&'fnc mut self, module: &'mdl ModuleGen<'ctx>, builder: &'bdr Builder<'ctx>) {
        builder.build_call(
            module
                .function_prototype_table
                .get("llvm.stackrestore")
                .unwrap()
                .function,
            vec![self.scope_stack.pop().unwrap().stack_position].as_slice(),
            "stackrestore",
        );
    }

    pub fn create_variable(
        &'fnc mut self,
        context: &'ctx Context,
        builder: &'bdr Builder<'ctx>,
        name: String,
        value_type: ValueType,
    ) -> PointerValue<'ctx> {
        let scope = self.scope_stack.last_mut().unwrap();

        if scope.variable_map.contains_key(&name) {
            panic!("variable {} is already declared in this scope.", name);
        }

        let address = builder.build_alloca(value_type.to_basic_type(context), &name);

        scope
            .variable_map
            .insert(name, (value_type, address.clone()));
        address
    }

    pub fn resolve_variable(&'fnc mut self, name: String) -> (ValueType, PointerValue<'ctx>) {
        let scope = self.scope_stack.last_mut().unwrap();

        match scope.variable_map.get(&name) {
            Some(variable) => variable.clone(),
            None => panic!("variable {} is not declared in this scope.", name),
        }
    }
}

impl<'mdl, 'ctx: 'mdl> ModuleGen<'ctx> {
    pub fn generate_code(&'mdl mut self, context: &'ctx Context, ast: &AST) {
        if ast.name != "module" {
            panic!("module AST expected, got {}.", ast.name);
        }

        self.create_function(context, "main".to_owned(), ValueType::Void, vec![], true)
            .generate_code_statement_list(context, self, &ast.children[0])
            .end_function(context);
    }
}

impl<'fnc, 'mdl: 'fnc, 'ctx: 'mdl> FuncGen<'ctx> {
    pub fn generate_code_statement_list(
        self,
        context: &'ctx Context,
        module: &'mdl Module,
        ast: &AST,
    ) -> Self {
        if ast.name != "statement-list" {
            panic!("statement-list AST expected, got {}.", ast.name);
        }

        if ast.children.is_empty() {
            return;
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
        self,
        context: &'ctx Context,
        module: &'mdl Module,
        &ast: &AST,
    ) -> Self {
        if ast.name != "statement" {
            panic!("statement AST expected, got {}.", ast.name);
        }

        match ast.children[0].name.as_ref() {
            "scope-statement" => {
                self.generate_code_statement_scope(context, module, &ast.children[0])
            }
            "if-statement" => unimplemented!(),
            "for-statement" => unimplemented!(),
            "with-statement" => unimplemented!(),
            "ret-statement" => unimplemented!(),
            "let-statement" => unimplemented!(),
            "expression" => unimplemented!(),
            _ => unreachable!(),
        }
    }

    pub fn generate_code_statement_scope(
        self,
        context: &'ctx Context,
        module: &'mdl Module,
        &ast: &AST,
    ) -> Self {
        if ast.name != "scope-statement" {
            panic!("scope-statement AST expected, got {}.", ast.name);
        }

        if ast.children.len() == 3 {
            self.create_scope(module, self.last_block.builder)
                .generate_code_statement_list(context, module, &ast.children[1])
                .end_scope(module, self.last_block.builder)
        }

        self
    }

    pub fn generate_code_statement_if(
        self,
        context: &'ctx Context,
        module: &'mdl Module,
        &ast: &AST,
    ) -> Self {
        let criteria = self.generate_code_expression(context, module, &ast.children[1]);

        if criteria.get_type() != ValueType::Bool {
			panic!(
				"type error; {} type extected, got {} type.",
				ValueType::Bool,
				criteria.get_type()
			);
        }
        
        // TODO : Create a new builder and connect it.

        self
    }

    pub fn generate_code_expression(
        &'fnc mut self,
        context: &'ctx Context,
        module: &'mdl Module,
        &ast: &AST,
    ) -> Value<'ctx> {
    }
}
