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
use std::collections::HashMap;
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

pub struct FuncPrototype<'ctx> {
	pub return_type: ValueType,
	pub param_type_vec: Vec<ValueType>,
	pub variadic_param: bool,
	pub function: FunctionValue<'ctx>,
}

pub struct FuncGen<'ctx> {
	pub function: FunctionValue<'ctx>,
	pub statement_stack: Vec<StatementGen<'ctx>>,
	pub scope_stack: Vec<ScopeGen<'ctx>>,
}

pub struct StatementGen<'ctx> {
	pub name: String,
	pub entry_block_gen: BlockGen<'ctx>,
	pub exit_block_gen: BlockGen<'ctx>,
}

pub struct ScopeGen<'ctx> {
	pub variable_map: HashMap<String, (ValueType, PointerValue<'ctx>)>,
	pub stack_position: BasicValueEnum<'ctx>,
}

pub struct BlockGen<'ctx> {
	pub block: BasicBlock,
	pub builder: Builder<'ctx>,
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
	) -> FunctionValue<'ctx> {
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

		self.function_prototype_table.insert(
			name.to_owned(),
			FuncPrototype {
				return_type: return_type,
				param_type_vec: param_type_vec,
				variadic_param: variadic_param,
				function: function.clone(),
			},
		);

		function
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

		let function =
			self.decl_function(context, name, return_type, param_type_vec, variadic_param);
		let mut func_gen = FuncGen {
			function: function,
			statement_stack: Vec::new(),
			scope_stack: Vec::new(),
		};

		let entry_block = context.append_basic_block(function, "entry");
		let entry_builder = context.create_builder();

		let exit_block = context.append_basic_block(function, "exit");
		let exit_builder = context.create_builder();

		entry_builder.position_at_end(&entry_block);
		exit_builder.position_at_end(&exit_block);

		let statement = StatementGen {
			name: "".to_owned(),
			entry_block_gen: BlockGen {
				block: entry_block,
				builder: entry_builder,
			},
			exit_block_gen: BlockGen {
				block: exit_block,
				builder: exit_builder,
			},
		};
		let block = statement.create_block(context, "body".to_owned());

		func_gen.statement_stack.push(statement);
		func_gen.create_scope(self, &block.builder);
		func_gen
	}
}

impl<'stmt, 'fnc: 'stmt, 'mdl: 'fnc, 'ctx: 'mdl> FuncGen<'ctx> {
	pub fn create_statement(
		&'fnc mut self,
		context: &'ctx Context,
		name: String,
	) -> &'stmt mut StatementGen<'ctx> {
		let parent_statement = self.statement_stack.last_mut().unwrap();

		let entry = parent_statement.create_block(context, "entry".to_owned());
		let exit = parent_statement.create_block(context, "exit".to_owned());

		let statement_gen = StatementGen {
			name: name,
			entry_block_gen: entry,
			exit_block_gen: exit,
		};
		statement_gen.create_block(context, "body".to_owned());

		self.statement_stack.push(statement_gen);
		self.statement_stack.last_mut().unwrap()
	}

	pub fn end_statement(&'fnc mut self, context: &'ctx Context) {
		self.statement_stack.pop().unwrap().end_statement(context);
	}

	pub fn create_scope(
		&'fnc mut self,
		module_gen: &'mdl ModuleGen<'ctx>,
		builder: &'stmt Builder<'ctx>,
	) {
		self.scope_stack.push(ScopeGen {
			variable_map: HashMap::new(),
			stack_position: builder
				.build_call(
					module_gen
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

	pub fn end_scope(
		&'fnc mut self,
		module_gen: &'mdl ModuleGen<'ctx>,
		builder: &'stmt Builder<'ctx>,
	) {
		builder.build_call(
			module_gen
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
		builder: &'stmt Builder<'ctx>,
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

impl<'stmt, 'ctx: 'stmt> StatementGen<'ctx> {
	pub fn end_statement(self, context: &'ctx Context) {
		// Create connection: entry BB -> first body BB
		self.entry_block_gen
			.builder
			.build_unconditional_branch(&self.first_block(context).block);

		// Create connection: last body BB -> exit BB
		self.last_block(context)
			.builder
			.build_unconditional_branch(&self.exit_block_gen.block);
	}

	pub fn create_block(&'stmt self, context: &'ctx Context, name: String) -> BlockGen<'ctx> {
		let block = context.prepend_basic_block(&self.exit_block_gen.block, &name);
		let builder = context.create_builder();

		builder.position_at_end(&block);

		BlockGen {
			block: block,
			builder: builder,
		}
	}

	pub fn first_block(&'stmt self, context: &'ctx Context) -> BlockGen<'ctx> {
		let block = self
			.entry_block_gen
			.block
			.get_next_basic_block()
			.unwrap()
			.clone();
		let builder = context.create_builder();

		builder.position_at_end(&block);

		BlockGen {
			block: block,
			builder: builder,
		}
	}

	pub fn last_block(&'stmt self, context: &'ctx Context) -> BlockGen<'ctx> {
		let block = self
			.exit_block_gen
			.block
			.get_previous_basic_block()
			.unwrap()
			.clone();
		let builder = context.create_builder();

		builder.position_at_end(&block);

		BlockGen {
			block: block,
			builder: builder,
		}
	}
}

impl<'fnc, 'mdl: 'fnc, 'ctx: 'mdl> ModuleGen<'ctx> {
	pub fn generate_code(&'mdl mut self, context: &'ctx Context, ast: &AST) {
		let mut main_function =
			self.create_function(context, "main".to_owned(), ValueType::Void, vec![], true);

		main_function.generate_code(context, self, &ast.children[0]);
	}
}

impl<'stmt, 'fnc: 'stmt, 'mdl: 'fnc, 'ctx: 'mdl> FuncGen<'ctx> {
	pub fn generate_code(
		&'fnc mut self,
		context: &'ctx Context,
		module: &'mdl mut ModuleGen<'ctx>,
		ast: &AST,
	) {
		
	}
}
