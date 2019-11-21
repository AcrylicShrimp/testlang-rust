extern crate either;
extern crate inkwell;

use super::value::{Value, ValueType, ValueTypeGroup};
use crate::lexer::TokenType;
use crate::parser::AST;

use either::Either;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::AnyValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FloatValue;
use inkwell::values::FunctionValue;
use inkwell::values::IntValue;
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

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

		// Declare printf prototype.
		let printf_function_type = self.context.i32_type().fn_type(
			&[BasicTypeEnum::PointerType(
				self.context.i8_type().ptr_type(AddressSpace::Generic),
			)],
			true,
		);

		module.add_function("printf", printf_function_type, None);

		InModuleGenerator {
			generator: self,
			name: name.to_owned(),
			module: module,
		}
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

		InFunctionGenerator {
			name: name.to_owned(),
			function: self.module.add_function(name, function_type, None),
			in_module_generator: self,
		}
	}

	pub fn generate_code(&'a self, ast: &AST) {
		if ast.name != "module" {
			panic!("module AST expected, got {}.", ast.name);
		}

		let main_function = self.new_function("main", (None, vec![]));

		// DELETEME: Adding printf call instruction.
		let basic_block = main_function.new_basic_block("printf call");
		let first_parameter = basic_block
			.builder
			.build_global_string_ptr("%d", "printf format string");

		basic_block.builder.build_call(
			self.module.get_function("printf").unwrap(),
			&[
				BasicValueEnum::PointerValue(first_parameter.as_pointer_value()),
				BasicValueEnum::IntValue(self.generator.context.i32_type().const_int(124, false)),
			],
			"call printf",
		);

		main_function.generate_code(&ast.children[0]);
		main_function.last_basic_block().builder.build_return(None);

		match self.module.verify() {
			Ok(_) => (),
			Err(err) => panic!("module verification failed; {}", err),
		};

		println!("Executing...");

		// DELETEME: Execute the main function using JIT execution engine.
		let execution_engine = self
			.module
			.create_jit_execution_engine(OptimizationLevel::None)
			.unwrap();

		type MainFunc = unsafe extern "C" fn();

		unsafe {
			let jit_main_function = execution_engine.get_function::<MainFunc>("main").unwrap();

			jit_main_function.call();
		}
	}
}

pub struct InFunctionGenerator<'a> {
	pub name: String,
	pub function: FunctionValue,
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

	pub fn last_basic_block(&'a self) -> InBasicBlockGenerator<'a> {
		let basic_block = self.function.get_last_basic_block().unwrap();
		let builder = self.in_module_generator.generator.context.create_builder();

		builder.position_at_end(&basic_block);

		InBasicBlockGenerator {
			name: basic_block.get_name().to_str().unwrap().to_owned(),
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
			self.generate_statement_code(ast);
		}
	}

	fn generate_statement_code(&'a self, ast: &AST) {
		if ast.name != "statement" {
			panic!("statement AST expected, got {}.", ast.name);
		}

		self.generate_expression_code(&ast.children[0]);
	}

	fn generate_expression_code(&'a self, ast: &AST) -> Value {
		match ast.name.as_ref() {
			"expression" => self.generate_expression_code(&ast.children[0]),
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
					self.generate_expression_code_op_or(ast)
				}
			}
			"op-and" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_and(ast)
				}
			}
			"op-not" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_not(ast)
				}
			}
			"op-cmp" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_cmp(ast)
				}
			}
			"op-addsub" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_addsub(ast)
				}
			}
			"op-muldivmod" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_muldivmod(ast)
				}
			}
			"op-shift" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_shift(ast)
				}
			}
			"op-bit-or" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_bit_or(ast)
				}
			}
			"op-bit-and" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_bit_and(ast)
				}
			}
			"op-bit-xor" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_bit_xor(ast)
				}
			}
			"op-bit-not" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_bit_not(ast)
				}
			}
			"op-single" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_single(ast)
				}
			}
			"function-call" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_function_call(ast)
				}
			}
			"left-value" => self.generate_expression_code_left_value(ast),
			"literal" => self.generate_expression_code_literal(ast),
			_ => panic!("not implemented AST; {} received.", ast.name),
		}
	}

	fn generate_expression_code_assignment(&'a self, ast: &AST) -> Value {
		unimplemented!();
	}

	fn generate_expression_code_op_or(&'a self, ast: &AST) -> Value {
		if ast.name != "op-or" {
			panic!("op-or AST expected, got {}.", ast.name);
		}

		// We don't need to create a new basic block for the lhs here,
		// because always there's an basic block that is not terminated yet.
		let lhs = self.generate_expression_code(&ast.children[0]);
		let lhs_block = self.last_basic_block();

		// Create a new basic block and generates rhs.
		// It may create some basic blocks,
		// so we have to obtain the last block after it.
		self.new_basic_block("OR rhs");
		let rhs = self.generate_expression_code(&ast.children[2]);
		let rhs_block = self.last_basic_block();

		if lhs.value_type != ValueType::Bool || rhs.value_type != ValueType::Bool {
			panic!("type error; boolean operation is only allowed between bool.");
		}

		let end_block = self.new_basic_block("OR end");

		lhs_block.builder.build_conditional_branch(
			lhs.unwrap_int_value(),
			&end_block.basic_block,
			&rhs_block.basic_block,
		);
		rhs_block
			.builder
			.build_unconditional_branch(&end_block.basic_block);

		let result = end_block
			.builder
			.build_phi(self.in_module_generator.generator.context.bool_type(), "OR");

		result.add_incoming(&[(
			&self
				.in_module_generator
				.generator
				.context
				.bool_type()
				.const_int(1, false),
			&lhs_block.basic_block,
		)]);
		result.add_incoming(&[(&rhs.unwrap_int_value(), &rhs_block.basic_block)]);

		Value {
			value_type: ValueType::Bool,
			llvm_type: AnyTypeEnum::IntType(self.in_module_generator.generator.context.bool_type()),
			llvm_value: AnyValueEnum::PhiValue(result),
		}
	}

	fn generate_expression_code_op_and(&'a self, ast: &AST) -> Value {
		if ast.name != "op-and" {
			panic!("op-and AST expected, got {}.", ast.name);
		}

		let lhs = self.generate_expression_code(&ast.children[0]);
		let lhs_block = self.last_basic_block();

		self.new_basic_block("AND rhs");
		let rhs = self.generate_expression_code(&ast.children[2]);
		let rhs_block = self.last_basic_block();

		if lhs.value_type != ValueType::Bool || rhs.value_type != ValueType::Bool {
			panic!("type error; boolean operation is only allowed between bool.");
		}

		let end_block = self.new_basic_block("AND end");

		lhs_block.builder.build_conditional_branch(
			lhs.unwrap_int_value(),
			&rhs_block.basic_block,
			&end_block.basic_block,
		);
		rhs_block
			.builder
			.build_unconditional_branch(&end_block.basic_block);

		let result = end_block.builder.build_phi(
			self.in_module_generator.generator.context.bool_type(),
			"AND",
		);

		result.add_incoming(&[(
			&self
				.in_module_generator
				.generator
				.context
				.bool_type()
				.const_int(0, false),
			&lhs_block.basic_block,
		)]);
		result.add_incoming(&[(&rhs.unwrap_int_value(), &rhs_block.basic_block)]);

		Value {
			value_type: ValueType::Bool,
			llvm_type: AnyTypeEnum::IntType(self.in_module_generator.generator.context.bool_type()),
			llvm_value: AnyValueEnum::PhiValue(result),
		}
	}

	fn generate_expression_code_op_not(&'a self, ast: &AST) -> Value {
		if ast.name != "op-not" {
			panic!("op-not AST expected, got {}.", ast.name);
		}

		let lhs = self.generate_expression_code(&ast.children[1]);
		let lhs_block = self.last_basic_block();

		if lhs.value_type != ValueType::Bool {
			panic!("type error; boolean operation is only allowed between bool.");
		}

		let result = lhs_block.builder.build_int_compare(
			IntPredicate::EQ,
			lhs.unwrap_int_value(),
			self.in_module_generator
				.generator
				.context
				.bool_type()
				.const_int(0, false),
			"NOT",
		);

		Value {
			value_type: ValueType::Bool,
			llvm_type: AnyTypeEnum::IntType(self.in_module_generator.generator.context.bool_type()),
			llvm_value: AnyValueEnum::IntValue(result),
		}
	}

	fn generate_expression_code_op_cmp(&'a self, ast: &AST) -> Value {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();

		match ValueType::merge(lhs.value_type, rhs.value_type) {
			Some(merged_value_type) => {
				lhs = block.cast(lhs, merged_value_type);
				rhs = block.cast(rhs, merged_value_type);
			}
			None => panic!(
				"type error; unable to compare {} and {}.",
				lhs.value_type, rhs.value_type
			),
		};

		let result = match lhs.value_type.to_group().0 {
			ValueTypeGroup::Bool | ValueTypeGroup::U => {
				let predicate = match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpEq => IntPredicate::EQ,
					TokenType::OpNeq => IntPredicate::NE,
					TokenType::OpLs => IntPredicate::ULT,
					TokenType::OpLsEq => IntPredicate::ULE,
					TokenType::OpGt => IntPredicate::UGT,
					TokenType::OpGtEq => IntPredicate::UGE,
					_ => unreachable!(),
				};

				block.builder.build_int_compare(
					predicate,
					lhs.unwrap_int_value(),
					rhs.unwrap_int_value(),
					"CMP",
				)
			}
			ValueTypeGroup::I => {
				let predicate = match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpEq => IntPredicate::EQ,
					TokenType::OpNeq => IntPredicate::NE,
					TokenType::OpLs => IntPredicate::SLT,
					TokenType::OpLsEq => IntPredicate::SLE,
					TokenType::OpGt => IntPredicate::SGT,
					TokenType::OpGtEq => IntPredicate::SGE,
					_ => unreachable!(),
				};

				block.builder.build_int_compare(
					predicate,
					lhs.unwrap_int_value(),
					rhs.unwrap_int_value(),
					"CMP",
				)
			}
			ValueTypeGroup::F => {
				let predicate = match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpEq => FloatPredicate::OEQ,
					TokenType::OpNeq => FloatPredicate::ONE,
					TokenType::OpLs => FloatPredicate::OLT,
					TokenType::OpLsEq => FloatPredicate::OLE,
					TokenType::OpGt => FloatPredicate::OGT,
					TokenType::OpGtEq => FloatPredicate::OGE,
					_ => unreachable!(),
				};

				block.builder.build_float_compare(
					predicate,
					lhs.unwrap_float_value(),
					rhs.unwrap_float_value(),
					"CMP",
				)
			}
			ValueTypeGroup::Str => unimplemented!(),
			ValueTypeGroup::Void => panic!("type error; void type is not valid."),
		};

		Value {
			value_type: ValueType::Bool,
			llvm_type: AnyTypeEnum::IntType(self.in_module_generator.generator.context.bool_type()),
			llvm_value: AnyValueEnum::IntValue(result),
		}
	}

	fn generate_expression_code_op_addsub(&'a self, ast: &AST) -> Value {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();

		match ValueType::merge(lhs.value_type, rhs.value_type) {
			Some(merged_value_type) => {
				lhs = block.cast(lhs, merged_value_type);
				rhs = block.cast(rhs, merged_value_type);
			}
			None => panic!(
				"type error; unable to operate on between {} and {}.",
				lhs.value_type, rhs.value_type
			),
		};

		let result = match lhs.value_type.to_group().0 {
			ValueTypeGroup::Bool => {
				panic!("type error; arithmetic operation between bool is not allowed.");
			}
			ValueTypeGroup::I | ValueTypeGroup::U => {
				AnyValueEnum::IntValue(match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpAdd => block.builder.build_int_add(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"ADD",
					),
					TokenType::OpSub => block.builder.build_int_sub(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"SUB",
					),
					_ => unreachable!(),
				})
			}
			ValueTypeGroup::F => {
				AnyValueEnum::FloatValue(match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpAdd => block.builder.build_float_add(
						lhs.unwrap_float_value(),
						rhs.unwrap_float_value(),
						"ADD",
					),
					TokenType::OpSub => block.builder.build_float_sub(
						lhs.unwrap_float_value(),
						rhs.unwrap_float_value(),
						"SUB",
					),
					_ => unreachable!(),
				})
			}
			ValueTypeGroup::Str => {
				panic!("type error; arithmetic operation between string is not allowed.");
			}
			ValueTypeGroup::Void => panic!("type error; void type is not valid."),
		};

		Value {
			value_type: lhs.value_type,
			llvm_type: self
				.in_module_generator
				.generator
				.to_any_type(lhs.value_type),
			llvm_value: result,
		}
	}

	fn generate_expression_code_op_muldivmod(&'a self, ast: &AST) -> Value {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();

		match ValueType::merge(lhs.value_type, rhs.value_type) {
			Some(merged_value_type) => {
				lhs = block.cast(lhs, merged_value_type);
				rhs = block.cast(rhs, merged_value_type);
			}
			None => panic!(
				"type error; unable to operate on between {} and {}.",
				lhs.value_type, rhs.value_type
			),
		};

		let result = match lhs.value_type.to_group().0 {
			ValueTypeGroup::Bool => {
				panic!("type error; arithmetic operation between bool is not allowed.");
			}
			ValueTypeGroup::I => {
				AnyValueEnum::IntValue(match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpMul => block.builder.build_int_mul(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"MUL",
					),
					TokenType::OpDiv => block.builder.build_int_signed_div(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"DIV",
					),
					TokenType::OpMod => block.builder.build_int_signed_rem(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"MOD",
					),
					_ => unreachable!(),
				})
			}
			ValueTypeGroup::U => {
				AnyValueEnum::IntValue(match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpMul => block.builder.build_int_mul(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"MUL",
					),
					TokenType::OpDiv => block.builder.build_int_unsigned_div(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"DIV",
					),
					TokenType::OpMod => block.builder.build_int_unsigned_rem(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"MOD",
					),
					_ => unreachable!(),
				})
			}
			ValueTypeGroup::F => {
				AnyValueEnum::FloatValue(match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpMul => block.builder.build_float_mul(
						lhs.unwrap_float_value(),
						rhs.unwrap_float_value(),
						"MUL",
					),
					TokenType::OpDiv => block.builder.build_float_div(
						lhs.unwrap_float_value(),
						rhs.unwrap_float_value(),
						"DIV",
					),
					TokenType::OpMod => block.builder.build_float_rem(
						lhs.unwrap_float_value(),
						rhs.unwrap_float_value(),
						"MOD",
					),
					_ => unreachable!(),
				})
			}
			ValueTypeGroup::Str => {
				panic!("type error; arithmetic operation between string is not allowed.");
			}
			ValueTypeGroup::Void => panic!("type error; void type is not valid."),
		};

		Value {
			value_type: lhs.value_type,
			llvm_type: self
				.in_module_generator
				.generator
				.to_any_type(lhs.value_type),
			llvm_value: result,
		}
	}

	fn generate_expression_code_op_shift(&'a self, ast: &AST) -> Value {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();

		match ValueType::merge(lhs.value_type, rhs.value_type) {
			Some(merged_value_type) => {
				lhs = block.cast(lhs, merged_value_type);
				rhs = block.cast(rhs, merged_value_type);
			}
			None => panic!(
				"type error; unable to operate on between {} and {}.",
				lhs.value_type, rhs.value_type
			),
		};

		let result = match lhs.value_type.to_group().0 {
			ValueTypeGroup::Bool => {
				panic!("type error; bitwise operation between bool is not allowed.");
			}
			ValueTypeGroup::I => {
				AnyValueEnum::IntValue(match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpShiftL => block.builder.build_left_shift(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"LSHIFT",
					),
					TokenType::OpShiftR => block.builder.build_right_shift(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						true,
						"RSHIFT",
					),
					_ => unreachable!(),
				})
			}
			ValueTypeGroup::U => {
				AnyValueEnum::IntValue(match ast.children[1].child.as_ref().unwrap().token_type {
					TokenType::OpShiftL => block.builder.build_left_shift(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						"LSHIFT",
					),
					TokenType::OpShiftR => block.builder.build_right_shift(
						lhs.unwrap_int_value(),
						rhs.unwrap_int_value(),
						false,
						"RSHIFT",
					),
					_ => unreachable!(),
				})
			}
			ValueTypeGroup::F => {
				panic!("type error; bitwise operation between float is not allowed.");
			}
			ValueTypeGroup::Str => {
				panic!("type error; bitwise operation between string is not allowed.");
			}
			ValueTypeGroup::Void => panic!("type error; void type is not valid."),
		};

		Value {
			value_type: lhs.value_type,
			llvm_type: self
				.in_module_generator
				.generator
				.to_any_type(lhs.value_type),
			llvm_value: result,
		}
	}

	fn generate_expression_code_op_bit_or(&'a self, ast: &AST) -> Value {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();

		match ValueType::merge(lhs.value_type, rhs.value_type) {
			Some(merged_value_type) => {
				lhs = block.cast(lhs, merged_value_type);
				rhs = block.cast(rhs, merged_value_type);
			}
			None => panic!(
				"type error; unable to operate on between {} and {}.",
				lhs.value_type, rhs.value_type
			),
		};

		let result = match lhs.value_type.to_group().0 {
			ValueTypeGroup::Bool => {
				panic!("type error; bitwise operation between bool is not allowed.");
			}
			ValueTypeGroup::I | ValueTypeGroup::U => {
				block
					.builder
					.build_or(lhs.unwrap_int_value(), rhs.unwrap_int_value(), "BIT_OR")
			}
			ValueTypeGroup::F => {
				panic!("type error; bitwise operation between float is not allowed.");
			}
			ValueTypeGroup::Str => {
				panic!("type error; bitwise operation between string is not allowed.");
			}
			ValueTypeGroup::Void => panic!("type error; void type is not valid."),
		};

		Value {
			value_type: lhs.value_type,
			llvm_type: self
				.in_module_generator
				.generator
				.to_any_type(lhs.value_type),
			llvm_value: AnyValueEnum::IntValue(result),
		}
	}

	fn generate_expression_code_op_bit_and(&'a self, ast: &AST) -> Value {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();

		match ValueType::merge(lhs.value_type, rhs.value_type) {
			Some(merged_value_type) => {
				lhs = block.cast(lhs, merged_value_type);
				rhs = block.cast(rhs, merged_value_type);
			}
			None => panic!(
				"type error; unable to operate on between {} and {}.",
				lhs.value_type, rhs.value_type
			),
		};

		let result = match lhs.value_type.to_group().0 {
			ValueTypeGroup::Bool => {
				panic!("type error; bitwise operation between bool is not allowed.");
			}
			ValueTypeGroup::I | ValueTypeGroup::U => {
				block
					.builder
					.build_and(lhs.unwrap_int_value(), rhs.unwrap_int_value(), "BIT_AND")
			}
			ValueTypeGroup::F => {
				panic!("type error; bitwise operation between float is not allowed.");
			}
			ValueTypeGroup::Str => {
				panic!("type error; bitwise operation between string is not allowed.");
			}
			ValueTypeGroup::Void => panic!("type error; void type is not valid."),
		};

		Value {
			value_type: lhs.value_type,
			llvm_type: self
				.in_module_generator
				.generator
				.to_any_type(lhs.value_type),
			llvm_value: AnyValueEnum::IntValue(result),
		}
	}

	fn generate_expression_code_op_bit_xor(&'a self, ast: &AST) -> Value {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();

		match ValueType::merge(lhs.value_type, rhs.value_type) {
			Some(merged_value_type) => {
				lhs = block.cast(lhs, merged_value_type);
				rhs = block.cast(rhs, merged_value_type);
			}
			None => panic!(
				"type error; unable to operate on between {} and {}.",
				lhs.value_type, rhs.value_type
			),
		};

		let result = match lhs.value_type.to_group().0 {
			ValueTypeGroup::Bool => {
				panic!("type error; bitwise operation between bool is not allowed.");
			}
			ValueTypeGroup::I | ValueTypeGroup::U => {
				block
					.builder
					.build_xor(lhs.unwrap_int_value(), rhs.unwrap_int_value(), "BIT_XOR")
			}
			ValueTypeGroup::F => {
				panic!("type error; bitwise operation between float is not allowed.");
			}
			ValueTypeGroup::Str => {
				panic!("type error; bitwise operation between string is not allowed.");
			}
			ValueTypeGroup::Void => panic!("type error; void type is not valid."),
		};

		Value {
			value_type: lhs.value_type,
			llvm_type: self
				.in_module_generator
				.generator
				.to_any_type(lhs.value_type),
			llvm_value: AnyValueEnum::IntValue(result),
		}
	}

	fn generate_expression_code_op_bit_not(&'a self, ast: &AST) -> Value {
		unimplemented!();
	}

	fn generate_expression_code_op_single(&'a self, ast: &AST) -> Value {
		match ast.children[0].child.as_ref().unwrap().token_type {
			TokenType::KeywordFrom => unimplemented!(),
			TokenType::ParenL => self.generate_expression_code(&ast.children[1]),
			TokenType::OpAdd => self.generate_expression_code(&ast.children[1]),
			TokenType::OpSub => unimplemented!(),
			_ => unreachable!(),
		}
	}

	fn generate_expression_code_function_call(&'a self, ast: &AST) -> Value {
		let mut parameter_value_vec: Vec<BasicValueEnum> = Vec::new();

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
				.filter(|ast| ast.name == "expression")
				.map(|ast| match self.generate_expression_code(ast).llvm_value {
					AnyValueEnum::IntValue(int_value) => BasicValueEnum::IntValue(int_value),
					AnyValueEnum::FloatValue(float_value) => {
						BasicValueEnum::FloatValue(float_value)
					}
					AnyValueEnum::PhiValue(phi_value) => phi_value.as_basic_value(),
					AnyValueEnum::PointerValue(pointer_value) => {
						BasicValueEnum::PointerValue(pointer_value)
					}
					_ => unreachable!(),
				})
				.collect();
		}

		let function_name = &ast.children[0].child.as_ref().unwrap().token_content;
		let function = match self.in_module_generator.module.get_function(function_name) {
			Some(function) => function,
			None => panic!(
				"unknown function detected; unable to find {} function.",
				function_name
			),
		};

		let basic_block = self.last_basic_block();

		let result =
			basic_block
				.builder
				.build_call(function, &parameter_value_vec, "function call");

		match result.try_as_basic_value() {
			Either::Left(basic_value) => Value {
				value_type: ValueType::Void,
				llvm_type: AnyTypeEnum::VoidType(
					self.in_module_generator.generator.context.void_type(),
				),
				llvm_value: AnyValueEnum::from(basic_value),
			},
			Either::Right(instruction_value) => Value {
				value_type: ValueType::Void,
				llvm_type: AnyTypeEnum::VoidType(
					self.in_module_generator.generator.context.void_type(),
				),
				llvm_value: AnyValueEnum::InstructionValue(instruction_value),
			},
		}
	}

	fn generate_expression_code_left_value(&'a self, ast: &AST) -> Value {
		unimplemented!();
	}

	fn generate_expression_code_literal(&'a self, ast: &AST) -> Value {
		let content = &ast.children[0].child.as_ref().unwrap().token_content;

		match ast.children[0].name.as_ref() {
			"LiteralBool" => Value {
				value_type: ValueType::Bool,
				llvm_type: AnyTypeEnum::IntType(
					self.in_module_generator.generator.context.bool_type(),
				),
				llvm_value: AnyValueEnum::IntValue(
					self.in_module_generator
						.generator
						.context
						.bool_type()
						.const_int(
							if content.parse::<bool>().unwrap() {
								1
							} else {
								0
							},
							false,
						),
				),
			},
			"LiteralInteger" => Value {
				value_type: ValueType::I32,
				llvm_type: AnyTypeEnum::IntType(
					self.in_module_generator.generator.context.i32_type(),
				),
				llvm_value: AnyValueEnum::IntValue(
					self.in_module_generator
						.generator
						.context
						.i32_type()
						.const_int(
							if content.starts_with("-") {
								content[1..].parse::<u64>().unwrap()
							} else {
								content.parse::<u64>().unwrap()
							},
							true,
						),
				),
			},
			"LiteralDecimal" => Value {
				value_type: ValueType::F32,
				llvm_type: AnyTypeEnum::FloatType(
					self.in_module_generator.generator.context.f32_type(),
				),
				llvm_value: AnyValueEnum::FloatValue(
					self.in_module_generator
						.generator
						.context
						.f32_type()
						.const_float(content.parse::<f64>().unwrap()),
				),
			},
			"LiteralString" => Value {
				value_type: ValueType::Str,
				llvm_type: AnyTypeEnum::PointerType(
					self.in_module_generator
						.generator
						.context
						.i8_type()
						.ptr_type(AddressSpace::Generic),
				),
				llvm_value: AnyValueEnum::PointerValue(
					self.last_basic_block()
						.builder
						.build_global_string_ptr(content, "String")
						.as_pointer_value(),
				),
			},
			_ => unreachable!(),
		}
	}
}

pub struct InBasicBlockGenerator<'a> {
	pub name: String,
	pub basic_block: BasicBlock,
	pub builder: Builder,
	pub in_function_generator: &'a InFunctionGenerator<'a>,
}
