extern crate inkwell;

use crate::parser::AST;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::module::Module;
use inkwell::types::AnyTypeEnum;
use inkwell::values::AnyValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FloatValue;
use inkwell::values::FunctionValue;
use inkwell::values::IntValue;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use std::cmp::max;

pub struct CodeGen {
	context: Context,
	builder: Builder,
}

impl CodeGen {
	pub fn new() -> CodeGen {
		let context = Context::create();
		let builder = context.create_builder();

		CodeGen {
			context: context,
			builder: builder,
		}
	}

	pub fn gen_code(&mut self, ast: &AST) {
		if ast.name != "module" {
			panic!("unexpected AST type; module expected, got {}.", ast.name);
		}

		let mut codegen_module = CodeGenModule::new(&mut self.context, &mut self.builder, "module");
		codegen_module.gen_code(&ast);
	}
}

#[derive(PartialEq)]
enum ValueType {
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
}

#[derive(PartialEq)]
enum ValueTypeGroup {
	Bool,
	I,
	U,
	F,
}

impl ValueType {
	fn from_group((value_type_group, value_bit): (ValueTypeGroup, usize)) -> ValueType {
		match (value_type_group, value_bit) {
			(ValueTypeGroup::Bool, 1) => ValueType::Bool,
			(ValueTypeGroup::I, 8) => ValueType::I8,
			(ValueTypeGroup::I, 16) => ValueType::I16,
			(ValueTypeGroup::I, 32) => ValueType::I32,
			(ValueTypeGroup::I, 64) => ValueType::I64,
			(ValueTypeGroup::I, 128) => ValueType::I128,
			(ValueTypeGroup::U, 8) => ValueType::U8,
			(ValueTypeGroup::U, 16) => ValueType::U16,
			(ValueTypeGroup::U, 32) => ValueType::U32,
			(ValueTypeGroup::U, 64) => ValueType::U64,
			(ValueTypeGroup::U, 128) => ValueType::U128,
			(ValueTypeGroup::F, 32) => ValueType::F32,
			(ValueTypeGroup::F, 64) => ValueType::F64,
		}
	}

	fn to_group(&self) -> (ValueTypeGroup, usize) {
		match self {
			ValueType::Bool => (ValueTypeGroup::Bool, 1),
			ValueType::I8 => (ValueTypeGroup::I, 8),
			ValueType::I16 => (ValueTypeGroup::I, 16),
			ValueType::I32 => (ValueTypeGroup::I, 32),
			ValueType::I64 => (ValueTypeGroup::I, 64),
			ValueType::I128 => (ValueTypeGroup::I, 128),
			ValueType::U8 => (ValueTypeGroup::U, 8),
			ValueType::U16 => (ValueTypeGroup::U, 16),
			ValueType::U32 => (ValueTypeGroup::U, 32),
			ValueType::U64 => (ValueTypeGroup::U, 64),
			ValueType::U128 => (ValueTypeGroup::U, 128),
			ValueType::F32 => (ValueTypeGroup::F, 32),
			ValueType::F64 => (ValueTypeGroup::F, 64),
		}
	}

	fn merge(lhs: ValueType, rhs: ValueType) -> Option<ValueType> {
		let lhs_group = lhs.to_group();
		let rhs_group = rhs.to_group();

		if lhs_group.0 == rhs_group.0 {
			Some(ValueType::from_group((
				lhs_group.0,
				max(lhs_group.1, rhs_group.1),
			)))
		} else {
			None
		}
	}
}

struct Value {
	value_type: ValueType,
	llvm_type: AnyTypeEnum,
	llvm_value: AnyValueEnum,
}

impl Value {
	fn merge(lhs: &Value, rhs: &Value) -> Option<Value> {
		match ValueType::merge(lhs.value_type, rhs.value_type) {
			Some(value_type) => {}
			None => None,
		}
	}
}

struct CodeGenModule<'a> {
	pub context: &'a mut Context,
	pub builder: &'a mut Builder,
	pub module: Module,
	pub main_function: FunctionValue,
}

impl<'a> CodeGenModule<'a> {
	pub fn new(
		context: &'a mut Context,
		builder: &'a mut Builder,
		module_name: &str,
	) -> CodeGenModule<'a> {
		let module = context.create_module(module_name);
		let main_function =
			module.add_function("main", context.void_type().fn_type(&[], true), None);
		let main_function_block = context.append_basic_block(&main_function, "entry");

		builder.position_at_end(&main_function_block);

		CodeGenModule::<'a> {
			context: context,
			builder: builder,
			module: module,
			main_function: main_function,
		}
	}

	pub fn gen_code(&mut self, ast: &AST) {
		self.gen_code_statement(ast);
		println!("{}", self.main_function.print_to_string());
	}

	// pub fn gen_code(&mut self, ast: &AST) {
	// 	let integer_type = self.context.i64_type();
	// 	let function = self
	// 		.module
	// 		.add_function("exp", integer_type.fn_type(&[], false), None);
	// 	let base_block = self.context.append_basic_block(&function, "entry");

	// 	self.builder.position_at_end(&base_block);

	// // 	let sum = self.builder.build_int_add(
	// // 		integer_type.const_int(72, false),
	// // 		integer_type.const_int(10, false),
	// // 		"add",
	// // 	);

	// // 	self.builder.build_return(Some(&sum));
	// }

	// pub fn gen_code(&mut self, ast: &AST) -> Value {
	// 	match ast.name.as_ref() {
	// 		"literal" => {
	// 			self.gen_code(&ast.children[0]);
	// 			self.builder.build_int_add()
	// 			self.context.
	// 		}
	// 		"LiteralBool" => self.builder.build_float_add(),
	// 	}
	// }

	fn gen_code_statement(&mut self, ast: &AST) {
		match ast.name.as_ref() {
			"module" => {
				self.gen_code_statement(&ast.children[0]);
			}
			"statement-list" => {
				for ast in &ast.children {
					self.gen_code_statement(ast);
				}
			}
			"statement" => {
				self.gen_code_statement(&ast.children[0]);
			}
			"expression" => {
				self.gen_code_expression(ast);
			}
			_ => {
				unreachable!();
			}
		}
	}

	fn gen_code_expression(&mut self, ast: &AST) -> Value {
		match ast.name.as_ref() {
			"expression" => self.gen_code_expression(&ast.children[0]),
			"assignment" => {
				if ast.children.len() == 1 {
					self.gen_code_expression(&ast.children[0])
				} else {
					unreachable!();
				}
			}
			"op-or" => {
				if ast.children.len() == 1 {
					self.gen_code_expression(&ast.children[0])
				} else {
					let lhs_raw = self.gen_code_expression(&ast.children[0]);
					let lhs = self.cast_to_bool(lhs_raw);

					let lhs_block = self.context.append_basic_block(
						&self
							.builder
							.get_insert_block()
							.unwrap()
							.get_parent()
							.unwrap(),
						"OR lhs",
					);
					let rhs_block = self.context.append_basic_block(
						&self
							.builder
							.get_insert_block()
							.unwrap()
							.get_parent()
							.unwrap(),
						"OR rhs",
					);
					let end_block = self.context.append_basic_block(
						&self
							.builder
							.get_insert_block()
							.unwrap()
							.get_parent()
							.unwrap(),
						"OR END",
					);

					self.builder
						.build_conditional_branch(lhs, &lhs_block, &rhs_block);

					self.builder.position_at_end(&lhs_block);
					self.builder.build_unconditional_branch(&end_block);
					self.builder.position_at_end(&rhs_block);
					let rhs_raw = self.gen_code_expression(&ast.children[2]);
					let rhs = self.cast_to_bool(rhs_raw);
					self.builder.build_unconditional_branch(&end_block);

					self.builder.position_at_end(&end_block);
					let result = self.builder.build_phi(self.context.bool_type(), "OR");
					result.add_incoming(&[(
						&self.context.bool_type().const_int(1, false),
						&lhs_block,
					)]);
					result.add_incoming(&[(&rhs, &rhs_block)]);

					Value {
						llvm_value: AnyValueEnum::PhiValue(result),
						llvm_type: AnyTypeEnum::IntType(self.context.bool_type()),
						value_type: ValueType::Bool,
					}
				}
			}
			"op-and" => {
				if ast.children.len() == 1 {
					self.gen_code_expression(&ast.children[0])
				} else {
					let lhs_raw = self.gen_code_expression(&ast.children[0]);
					let lhs = self.cast_to_bool(lhs_raw);

					let lhs_block = self.context.append_basic_block(
						&self
							.builder
							.get_insert_block()
							.unwrap()
							.get_parent()
							.unwrap(),
						"AND lhs",
					);
					let rhs_block = self.context.append_basic_block(
						&self
							.builder
							.get_insert_block()
							.unwrap()
							.get_parent()
							.unwrap(),
						"AND rhs",
					);
					let end_block = self.context.append_basic_block(
						&self
							.builder
							.get_insert_block()
							.unwrap()
							.get_parent()
							.unwrap(),
						"AND END",
					);

					self.builder
						.build_conditional_branch(lhs, &rhs_block, &lhs_block);

					self.builder.position_at_end(&lhs_block);
					self.builder.build_unconditional_branch(&end_block);
					self.builder.position_at_end(&rhs_block);
					let rhs_raw = self.gen_code_expression(&ast.children[2]);
					let rhs = self.cast_to_bool(rhs_raw);
					self.builder.build_unconditional_branch(&end_block);

					self.builder.position_at_end(&end_block);
					let result = self.builder.build_phi(self.context.bool_type(), "AND");
					result.add_incoming(&[(
						&self.context.bool_type().const_int(0, false),
						&lhs_block,
					)]);
					result.add_incoming(&[(&rhs, &rhs_block)]);

					Value {
						llvm_value: AnyValueEnum::PhiValue(result),
						llvm_type: AnyTypeEnum::IntType(self.context.bool_type()),
						value_type: ValueType::Bool,
					}
				}
			}
			"op-not" => {
				if ast.children.len() == 1 {
					self.gen_code_expression(&ast.children[0])
				} else {
					let lhs_raw = self.gen_code_expression(&ast.children[1]);
					let lhs = self.cast_to_bool(lhs_raw);

					let result = self.builder.build_int_compare(
						IntPredicate::EQ,
						lhs,
						self.context.bool_type().const_int(0, false),
						"NOT",
					);

					Value {
						llvm_value: AnyValueEnum::IntValue(result),
						llvm_type: AnyTypeEnum::IntType(self.context.bool_type()),
						value_type: ValueType::Bool,
					}
				}
			}
			"op-comp" => {
				if ast.children.len() == 1 {
					self.gen_code_expression(&ast.children[0])
				} else {
					let lhs_raw = self.gen_code_expression(&ast.children[0]);

					let result = self.builder.build_int_compare(
						IntPredicate::EQ,
						lhs,
						self.context.bool_type().const_int(0, false),
						"NOT",
					);

					Value {
						llvm_value: AnyValueEnum::IntValue(result),
						llvm_type: AnyTypeEnum::IntType(self.context.bool_type()),
						value_type: ValueType::Bool,
					}
				}
			}
			"LiteralInteger" => Value {
				llvm_value: AnyValueEnum::IntValue(
					self.context.i32_type().const_int(
						ast.child
							.as_ref()
							.unwrap()
							.token_content
							.parse::<u64>()
							.unwrap(),
						ast.child.as_ref().unwrap().token_content.starts_with("-"),
					),
				),
				llvm_type: AnyTypeEnum::IntType(self.context.bool_type()),
				value_type: ValueType::I32,
			},
			_ => self.gen_code_expression(&ast.children[0]),
		}
	}

	fn cast_to_bool(&mut self, value: Value) -> IntValue {
		match value.llvm_value {
			AnyValueEnum::IntValue(int_value) => {
				if value.value_type == ValueType::Bool {
					int_value
				} else {
					let from_type = if let AnyTypeEnum::IntType(int_type) = value.llvm_type {
						int_type
					} else {
						unreachable!();
					};

					self.builder.build_int_compare(
						IntPredicate::NE,
						int_value,
						from_type.const_int(0, false),
						"CAST i2b",
					)
				}
			}
			AnyValueEnum::FloatValue(float_value) => {
				let from_type = if let AnyTypeEnum::FloatType(float_type) = value.llvm_type {
					float_type
				} else {
					unreachable!();
				};

				self.builder.build_float_compare(
					FloatPredicate::ONE,
					float_value,
					from_type.const_float(0.0),
					"CAST f2b",
				)
			}
			AnyValueEnum::PhiValue(phi_value) => match phi_value.as_basic_value() {
				BasicValueEnum::IntValue(int_value) => {
					if value.value_type == ValueType::Bool {
						int_value
					} else {
						let from_type = if let AnyTypeEnum::IntType(int_type) = value.llvm_type {
							int_type
						} else {
							unreachable!();
						};
						self.builder.build_int_compare(
							IntPredicate::NE,
							int_value,
							from_type.const_int(0, false),
							"CAST i2b",
						)
					}
				}
				BasicValueEnum::FloatValue(float_value) => {
					let from_type = if let AnyTypeEnum::FloatType(float_type) = value.llvm_type {
						float_type
					} else {
						unreachable!();
					};

					self.builder.build_float_compare(
						FloatPredicate::ONE,
						float_value,
						from_type.const_float(0.0),
						"CAST f2b",
					)
				}
				_ => unreachable!(),
			},
			_ => unreachable!(),
		}
	}
}
