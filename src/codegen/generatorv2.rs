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

pub struct FuncGen<'ctx> {
	pub prototype: FuncPrototype<'ctx>,
	pub statement_vec: Vec<StatementGen<'ctx>>,
}

pub struct FuncPrototype<'ctx> {
	pub return_type: ValueType,
	pub param_type_vec: Vec<ValueType>,
	pub variadic_param: bool,
	pub function: FunctionValue<'ctx>,
}

pub struct StatementGen<'ctx> {
	pub scope: ScopeGen<'ctx>,
	pub substatement_vec: Vec<StatementGen<'ctx>>,
}

pub struct ScopeGen<'ctx> {
	pub block_vec: Vec<BlockGen<'ctx>>,
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

		self.module_map.get_mut(&name).unwrap()
	}
}

impl<'fnc, 'mdl: 'fnc, 'ctx: 'mdl> ModuleGen<'ctx> {
	pub fn create_function(
		&'mdl mut self,
		name: String,
		return_type: ValueType,
		param_type_vec: Vec<ValueType>,
		variadic_param: bool,
	) -> &'fnc mut FuncGen<'ctx> {
		
	}
}
