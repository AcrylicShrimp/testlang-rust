extern crate inkwell;
extern crate strum;
extern crate strum_macros;

use super::generator::Generator;
use super::generator::InBasicBlockGenerator;

use inkwell::types::AnyTypeEnum;
use inkwell::types::BasicTypeEnum;
use inkwell::values::AnyValueEnum;
use inkwell::values::BasicValueEnum;
use inkwell::values::FloatValue;
use inkwell::values::IntValue;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;

use std::cmp::max;

use strum_macros::Display;

#[derive(Display, PartialEq, Copy, Clone)]
pub enum ValueType {
	Void,
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
	Str,
}

#[derive(Display, PartialEq, Copy, Clone)]
pub enum ValueTypeGroup {
	Void,
	Bool,
	I,
	U,
	F,
	Str,
}

impl ValueType {
	pub fn from_group((value_type_group, value_bit): (ValueTypeGroup, usize)) -> ValueType {
		match (value_type_group, value_bit) {
			(ValueTypeGroup::Void, 0) => ValueType::Void,
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
			(ValueTypeGroup::F, 16) => ValueType::F16,
			(ValueTypeGroup::F, 32) => ValueType::F32,
			(ValueTypeGroup::F, 64) => ValueType::F64,
			(ValueTypeGroup::Str, 0) => ValueType::Str,
			_ => unreachable!(),
		}
	}

	pub fn to_group(&self) -> (ValueTypeGroup, usize) {
		match self {
			ValueType::Void => (ValueTypeGroup::Void, 0),
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
			ValueType::F16 => (ValueTypeGroup::F, 16),
			ValueType::F32 => (ValueTypeGroup::F, 32),
			ValueType::F64 => (ValueTypeGroup::F, 64),
			ValueType::Str => (ValueTypeGroup::Str, 0),
		}
	}

	pub fn merge(lhs: ValueType, rhs: ValueType) -> Option<ValueType> {
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

pub struct Value {
	pub value_type: ValueType,
	pub llvm_type: AnyTypeEnum,
	pub llvm_value: AnyValueEnum,
}

impl Value {
	pub fn unwrap_int_value(&self) -> IntValue {
		match self.llvm_value {
			AnyValueEnum::IntValue(int_value) => int_value,
			AnyValueEnum::PhiValue(phi_value) => match phi_value.as_basic_value() {
				BasicValueEnum::IntValue(int_value) => int_value,
				_ => unreachable!(),
			},
			_ => unreachable!(),
		}
	}

	pub fn unwrap_float_value(&self) -> FloatValue {
		match self.llvm_value {
			AnyValueEnum::FloatValue(float_value) => float_value,
			AnyValueEnum::PhiValue(phi_value) => match phi_value.as_basic_value() {
				BasicValueEnum::FloatValue(float_value) => float_value,
				_ => unreachable!(),
			},
			_ => unreachable!(),
		}
	}

	pub fn unwrap_string_value(&self) -> PointerValue {
		match self.llvm_value {
			AnyValueEnum::PointerValue(pointer_value) => pointer_value,
			AnyValueEnum::PhiValue(phi_value) => match phi_value.as_basic_value() {
				BasicValueEnum::PointerValue(pointer_value) => pointer_value,
				_ => unreachable!(),
			},
			_ => unreachable!(),
		}
	}
}

impl<'a> Generator {
	pub fn to_any_type(&'a self, value_type: ValueType) -> AnyTypeEnum {
		match value_type {
			ValueType::Void => AnyTypeEnum::VoidType(self.context.void_type()),
			ValueType::Bool => AnyTypeEnum::IntType(self.context.bool_type()),
			ValueType::I8 => AnyTypeEnum::IntType(self.context.i8_type()),
			ValueType::I16 => AnyTypeEnum::IntType(self.context.i16_type()),
			ValueType::I32 => AnyTypeEnum::IntType(self.context.i32_type()),
			ValueType::I64 => AnyTypeEnum::IntType(self.context.i64_type()),
			ValueType::I128 => AnyTypeEnum::IntType(self.context.i128_type()),
			ValueType::U8 => AnyTypeEnum::IntType(self.context.i8_type()),
			ValueType::U16 => AnyTypeEnum::IntType(self.context.i16_type()),
			ValueType::U32 => AnyTypeEnum::IntType(self.context.i32_type()),
			ValueType::U64 => AnyTypeEnum::IntType(self.context.i64_type()),
			ValueType::U128 => AnyTypeEnum::IntType(self.context.i128_type()),
			ValueType::F16 => AnyTypeEnum::FloatType(self.context.f16_type()),
			ValueType::F32 => AnyTypeEnum::FloatType(self.context.f32_type()),
			ValueType::F64 => AnyTypeEnum::FloatType(self.context.f64_type()),
			ValueType::Str => {
				AnyTypeEnum::PointerType(self.context.i8_type().ptr_type(AddressSpace::Generic))
			}
		}
	}

	pub fn to_basic_type(&'a self, value_type: ValueType) -> BasicTypeEnum {
		match value_type {
			ValueType::Void => panic!("type error; unable to cast to basic type."),
			ValueType::Bool => BasicTypeEnum::IntType(self.context.bool_type()),
			ValueType::I8 => BasicTypeEnum::IntType(self.context.i8_type()),
			ValueType::I16 => BasicTypeEnum::IntType(self.context.i16_type()),
			ValueType::I32 => BasicTypeEnum::IntType(self.context.i32_type()),
			ValueType::I64 => BasicTypeEnum::IntType(self.context.i64_type()),
			ValueType::I128 => BasicTypeEnum::IntType(self.context.i128_type()),
			ValueType::U8 => BasicTypeEnum::IntType(self.context.i8_type()),
			ValueType::U16 => BasicTypeEnum::IntType(self.context.i16_type()),
			ValueType::U32 => BasicTypeEnum::IntType(self.context.i32_type()),
			ValueType::U64 => BasicTypeEnum::IntType(self.context.i64_type()),
			ValueType::U128 => BasicTypeEnum::IntType(self.context.i128_type()),
			ValueType::F16 => BasicTypeEnum::FloatType(self.context.f16_type()),
			ValueType::F32 => BasicTypeEnum::FloatType(self.context.f32_type()),
			ValueType::F64 => BasicTypeEnum::FloatType(self.context.f64_type()),
			ValueType::Str => {
				BasicTypeEnum::PointerType(self.context.i8_type().ptr_type(AddressSpace::Generic))
			}
		}
	}
}

impl<'a, 'b, 'c> InBasicBlockGenerator<'a, 'b, 'c> {
	pub fn in_group_cast(&'c self, from: Value, to: ValueType) -> Value {
		if from.value_type == to {
			return from;
		}

		let from_group = from.value_type.to_group();
		let to_group = to.to_group();

		if from_group.0 != to_group.0 {
			unreachable!();
		}

		let to_value = match from.llvm_value {
			AnyValueEnum::IntValue(int_value) => match to {
				ValueType::I8 | ValueType::U8 => AnyValueEnum::IntValue(
					self.builder.build_int_cast(
						int_value,
						self.in_function_generator
							.in_module_generator
							.generator
							.context
							.i8_type(),
						"CAST i2i",
					),
				),
				ValueType::I16 | ValueType::U16 => AnyValueEnum::IntValue(
					self.builder.build_int_cast(
						int_value,
						self.in_function_generator
							.in_module_generator
							.generator
							.context
							.i16_type(),
						"CAST i2i",
					),
				),
				ValueType::I32 | ValueType::U32 => AnyValueEnum::IntValue(
					self.builder.build_int_cast(
						int_value,
						self.in_function_generator
							.in_module_generator
							.generator
							.context
							.i32_type(),
						"CAST i2i",
					),
				),
				ValueType::I64 | ValueType::U64 => AnyValueEnum::IntValue(
					self.builder.build_int_cast(
						int_value,
						self.in_function_generator
							.in_module_generator
							.generator
							.context
							.i64_type(),
						"CAST i2i",
					),
				),
				ValueType::I128 | ValueType::U128 => AnyValueEnum::IntValue(
					self.builder.build_int_cast(
						int_value,
						self.in_function_generator
							.in_module_generator
							.generator
							.context
							.i128_type(),
						"CAST i2i",
					),
				),
				_ => unreachable!(),
			},
			AnyValueEnum::FloatValue(float_value) => match to {
				ValueType::F16 => AnyValueEnum::FloatValue(
					self.builder.build_float_cast(
						float_value,
						self.in_function_generator
							.in_module_generator
							.generator
							.context
							.f16_type(),
						"CAST f2f",
					),
				),
				ValueType::F32 => AnyValueEnum::FloatValue(
					self.builder.build_float_cast(
						float_value,
						self.in_function_generator
							.in_module_generator
							.generator
							.context
							.f32_type(),
						"CAST f2f",
					),
				),
				ValueType::F64 => AnyValueEnum::FloatValue(
					self.builder.build_float_cast(
						float_value,
						self.in_function_generator
							.in_module_generator
							.generator
							.context
							.f64_type(),
						"CAST f2f",
					),
				),
				_ => unreachable!(),
			},
			AnyValueEnum::PhiValue(phi_value) => match phi_value.as_basic_value() {
				BasicValueEnum::IntValue(int_value) => match to {
					ValueType::I8 | ValueType::U8 => AnyValueEnum::IntValue(
						self.builder.build_int_cast(
							int_value,
							self.in_function_generator
								.in_module_generator
								.generator
								.context
								.i8_type(),
							"CAST i2i",
						),
					),
					ValueType::I16 | ValueType::U16 => AnyValueEnum::IntValue(
						self.builder.build_int_cast(
							int_value,
							self.in_function_generator
								.in_module_generator
								.generator
								.context
								.i16_type(),
							"CAST i2i",
						),
					),
					ValueType::I32 | ValueType::U32 => AnyValueEnum::IntValue(
						self.builder.build_int_cast(
							int_value,
							self.in_function_generator
								.in_module_generator
								.generator
								.context
								.i32_type(),
							"CAST i2i",
						),
					),
					ValueType::I64 | ValueType::U64 => AnyValueEnum::IntValue(
						self.builder.build_int_cast(
							int_value,
							self.in_function_generator
								.in_module_generator
								.generator
								.context
								.i64_type(),
							"CAST i2i",
						),
					),
					ValueType::I128 | ValueType::U128 => AnyValueEnum::IntValue(
						self.builder.build_int_cast(
							int_value,
							self.in_function_generator
								.in_module_generator
								.generator
								.context
								.i128_type(),
							"CAST i2i",
						),
					),
					_ => unreachable!(),
				},
				BasicValueEnum::FloatValue(float_value) => match to {
					ValueType::F16 => AnyValueEnum::FloatValue(
						self.builder.build_float_cast(
							float_value,
							self.in_function_generator
								.in_module_generator
								.generator
								.context
								.f16_type(),
							"CAST f2f",
						),
					),
					ValueType::F32 => AnyValueEnum::FloatValue(
						self.builder.build_float_cast(
							float_value,
							self.in_function_generator
								.in_module_generator
								.generator
								.context
								.f32_type(),
							"CAST f2f",
						),
					),
					ValueType::F64 => AnyValueEnum::FloatValue(
						self.builder.build_float_cast(
							float_value,
							self.in_function_generator
								.in_module_generator
								.generator
								.context
								.f64_type(),
							"CAST f2f",
						),
					),
					_ => unreachable!(),
				},
				_ => unreachable!(),
			},
			_ => unreachable!(),
		};

		Value {
			value_type: to,
			llvm_type: self
				.in_function_generator
				.in_module_generator
				.generator
				.to_any_type(to),
			llvm_value: to_value,
		}
	}

	pub fn cast(&'c self, from: Value, to: ValueType) -> Value {
		if from.value_type == to {
			return from;
		}

		let from_group = from.value_type.to_group();
		let to_group = to.to_group();

		if from_group.0 == to_group.0 {
			return self.in_group_cast(from, to);
		}

		let to_value = match to_group.0 {
			ValueTypeGroup::Void => unreachable!(),
			ValueTypeGroup::Bool => match from.llvm_value {
				AnyValueEnum::IntValue(int_value) => {
					if let AnyTypeEnum::IntType(int_type) = from.llvm_type {
						AnyValueEnum::IntValue(self.builder.build_int_compare(
							IntPredicate::NE,
							int_value,
							int_type.const_int(0, false),
							"CAST i2b",
						))
					} else {
						unreachable!();
					}
				}
				AnyValueEnum::FloatValue(float_value) => {
					if let AnyTypeEnum::FloatType(float_type) = from.llvm_type {
						AnyValueEnum::IntValue(self.builder.build_float_compare(
							FloatPredicate::ONE,
							float_value,
							float_type.const_float(0.0),
							"CAST f2b",
						))
					} else {
						unreachable!();
					}
				}
				_ => unreachable!(),
			},
			ValueTypeGroup::I => match from.llvm_value {
				AnyValueEnum::IntValue(int_value) => {
					if let BasicTypeEnum::IntType(int_type) = self
						.in_function_generator
						.in_module_generator
						.generator
						.to_basic_type(to)
					{
						AnyValueEnum::IntValue(
							self.builder.build_int_cast(int_value, int_type, "CAST i2i"),
						)
					} else {
						unreachable!();
					}
				}
				AnyValueEnum::FloatValue(float_value) => {
					if let BasicTypeEnum::IntType(int_type) = self
						.in_function_generator
						.in_module_generator
						.generator
						.to_basic_type(to)
					{
						AnyValueEnum::IntValue(self.builder.build_float_to_signed_int(
							float_value,
							int_type,
							"CAST f2i",
						))
					} else {
						unreachable!();
					}
				}
				_ => unreachable!(),
			},
			ValueTypeGroup::U => match from.llvm_value {
				AnyValueEnum::IntValue(int_value) => {
					if let BasicTypeEnum::IntType(int_type) = self
						.in_function_generator
						.in_module_generator
						.generator
						.to_basic_type(to)
					{
						AnyValueEnum::IntValue(
							self.builder.build_int_cast(int_value, int_type, "CAST i2u"),
						)
					} else {
						unreachable!();
					}
				}
				AnyValueEnum::FloatValue(float_value) => {
					if let BasicTypeEnum::IntType(int_type) = self
						.in_function_generator
						.in_module_generator
						.generator
						.to_basic_type(to)
					{
						AnyValueEnum::IntValue(self.builder.build_float_to_unsigned_int(
							float_value,
							int_type,
							"CAST f2u",
						))
					} else {
						unreachable!();
					}
				}
				_ => unreachable!(),
			},
			ValueTypeGroup::F => match from.llvm_value {
				AnyValueEnum::IntValue(int_value) => {
					if let BasicTypeEnum::FloatType(float_type) = self
						.in_function_generator
						.in_module_generator
						.generator
						.to_basic_type(to)
					{
						if from.value_type.to_group().0 == ValueTypeGroup::I {
							AnyValueEnum::FloatValue(
								self.builder
									.build_signed_int_to_float(int_value, float_type, "CAST i2f"),
							)
						} else {
							AnyValueEnum::FloatValue(
								self.builder
									.build_unsigned_int_to_float(int_value, float_type, "CAST u2f"),
							)
						}
					} else {
						unreachable!();
					}
				}
				_ => unreachable!(),
			},
			ValueTypeGroup::Str => unreachable!(),
		};

		Value {
			value_type: to,
			llvm_type: self
				.in_function_generator
				.in_module_generator
				.generator
				.to_any_type(to),
			llvm_value: to_value,
		}
	}
}
