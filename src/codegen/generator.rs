extern crate inkwell;

use super::value::{Value, ValueType, ValueTypeGroup};
use crate::parser::AST;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::AnyValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FloatValue;
use inkwell::values::FunctionValue;
use inkwell::values::IntValue;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use std::cmp::max;
use std::vec::Vec;

pub struct Generator {
	pub context: Context,
}

impl<'a> Generator {
	pub fn new() -> Generator {
		let context = Context::create();

		Generator { context: context }
	}

	pub fn new_module(&'a self, name: &str) -> InModuleGenerator<'a> {
		let module = self.context.create_module(name);

		InModuleGenerator {
			generator: self,
			name: name.to_owned(),
			module: module,
		}
	}

	pub fn generate_code(&'a self, ast: &AST, name: &str) {
		if ast.name != "module" {
			panic!("module AST expected, got {}.", ast.name);
		}

		self.new_module(name).generate_code(ast);
	}
}

pub struct InModuleGenerator<'a> {
	pub generator: &'a Generator,
	pub name: String,
	pub module: Module,
}

impl<'a> InModuleGenerator<'a> {
	pub fn new_function(
		&'a self,
		name: &str,
		function_type: (Option<ValueType>, Vec<ValueType>),
	) -> InFunctionGenerator<'a> {
		let function_parameter_type: Vec<BasicTypeEnum> = function_type
			.1
			.into_iter()
			.map(|param_type| self.generator.to_basic_type(param_type))
			.collect();

		let function_type = match function_type.0 {
			Some(value_type) => match self.generator.to_basic_type(value_type) {
				BasicTypeEnum::IntType(int_type) => {
					int_type.fn_type(function_parameter_type.as_slice(), false)
				}
				BasicTypeEnum::FloatType(float_type) => {
					float_type.fn_type(function_parameter_type.as_slice(), false)
				}
				_ => unreachable!(),
			},
			None => self
				.generator
				.context
				.void_type()
				.fn_type(function_parameter_type.as_slice(), false),
		};

		let function = self.module.add_function(name, function_type, None);
		let function_basic_block = self
			.generator
			.context
			.append_basic_block(&function, "entry");
		let function_builder = self.generator.context.create_builder();

		function_builder.position_at_end(&function_basic_block);

		InFunctionGenerator {
			name: name.to_owned(),
			function: function,
			builder: function_builder,
			in_module_generator: self,
		}
	}

	pub fn generate_code(&'a self, ast: &AST) {
		self.new_function("main", (None, vec![]))
			.generate_code(&ast.children[0]);
	}
}

pub struct InFunctionGenerator<'a> {
	pub name: String,
	pub function: FunctionValue,
	pub builder: Builder,
	pub in_module_generator: &'a InModuleGenerator<'a>,
}

impl<'a> InFunctionGenerator<'a> {
	pub fn new_basic_block(&'a self, name: &str) -> InBasicBlockGenerator<'a> {
		let basic_block = self
			.in_module_generator
			.generator
			.context
			.append_basic_block(&self.function, name);
		let builder = self.in_module_generator.generator.context.create_builder();

		builder.position_at_end(&basic_block);

		InBasicBlockGenerator {
			name: name.to_owned(),
			basic_block: basic_block,
			builder: builder,
			in_function_generator: self,
		}
	}

	pub fn generate_code(&'a self, ast: &AST) {
		if ast.name != "statement-list" {
			panic!("statement-list AST expected, got {}.", ast.name);
		}

		if ast.children.is_empty() {
			return;
		}

		let mut statement_stack: Vec<&AST> = ast.children.iter().rev().collect();

		while statement_stack.last().unwrap().name == "statement-list" {
			let mut statement_vec: Vec<&AST> = statement_stack
				.pop()
				.unwrap()
				.children
				.iter()
				.rev()
				.collect();

			statement_stack.append(&mut statement_vec);
		}

		for ast in statement_stack.into_iter().rev() {
			self.generate_code(ast);
		}
	}

	fn generate_statement_code(&'a self, ast: &AST) {
		if ast.name != "statement" {
			panic!("statement AST expected, got {}.", ast.name);
		}

		match ast.children[0].name.as_ref() {
			"expression" => {
				self.generate_expression_code(&ast.children[0]);
			}
			_ => unreachable!(),
		}
	}

	fn generate_expression_code(&'a self, ast: &AST) -> Value {
		match ast.name.as_ref() {
			"assignment" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_assignment(ast)
				}
			}
			"op-or" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_assignment(ast)
				}
			}
			_ => unreachable!(),
		}
	}

	fn generate_expression_code_assignment(&'a self, ast: &AST) -> Value {
		unreachable!();
	}

	fn generate_expression_code_op_or(&'a self, ast: &AST) -> Value {
		if ast.name != "op-or" {
			panic!("op-or AST expected, got {}.", ast.name);
		}

		let lhs_block = self.new_basic_block("OR lhs");
		let rhs_block = self.new_basic_block("OR rhs");
		let end_block = self.new_basic_block("OR end");

		let lhs = 

		if let AnyValueEnum::IntValue(int_value) = self
			.cast(
				self.generate_expression_code(&ast.children[0]),
				ValueType::Bool,
			)
			.llvm_value
		{
			lhs_block.builder.build_conditional_branch(
				int_value,
				&end_block.basic_block,
				&rhs_block.basic_block,
			);
		} else {
			unreachable!();
		}

		// TODO : Remove the builder from all generators except for the bb generator.

		// Value {

		// }

		unreachable!();
	}
}

pub struct InBasicBlockGenerator<'a> {
	pub name: String,
	pub basic_block: BasicBlock,
	pub builder: Builder,
	pub in_function_generator: &'a InFunctionGenerator<'a>,
}
