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
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use std::collections::HashMap;
use std::vec::Vec;

pub struct Generator {
	pub context: Context,
}

impl<'ctx> Generator {
	pub fn new() -> Generator {
		let context = Context::create();

		Generator { context: context }
	}

	pub fn new_module(&'ctx self, name: &str) -> InModuleGenerator<'ctx> {
		let mut in_module_generator = InModuleGenerator {
			generator: self,
			name: name.to_owned(),
			module: self.context.create_module(name),
			function_table: HashMap::new(),
		};

		// DELETEME: Declare some C library function prototypes.
		in_module_generator.new_function(
			"printf",
			(ValueType::I32, vec![ValueType::Str]),
			true,
			true,
		);
		in_module_generator.new_function("rand", (ValueType::I32, vec![]), false, true);
		in_module_generator.new_function(
			"srand",
			(ValueType::Void, vec![ValueType::U64]),
			false,
			true,
		);
		in_module_generator.new_function(
			"time",
			(ValueType::U64, vec![ValueType::U64]),
			false,
			true,
		);

		in_module_generator
	}
}

pub struct InModuleGenerator<'ctx> {
	pub generator: &'ctx Generator,
	pub name: String,
	pub module: Module<'ctx>,
	pub function_table: HashMap<String, (ValueType, Vec<ValueType>, bool, FunctionValue<'ctx>)>,
}

impl<'a, 'ctx: 'a> InModuleGenerator<'ctx> {
	pub fn new_function(
		&'a mut self,
		name: &str,
		function_type: (ValueType, Vec<ValueType>),
		has_variadic_parameter: bool,
		is_declaration: bool,
	) -> InFunctionGenerator<'a, 'ctx> {
		let function_parameter_type: Vec<BasicTypeEnum<'ctx>> = function_type
			.1
			.clone()
			.into_iter()
			.map(|param_type| param_type.to_basic_type(&self.generator.context))
			.collect();

		let llvm_function_type = function_type.0.invoke_handler(
			&self.generator.context,
			ValueTypeHandler::new()
				.handle_void(&|_, ty| {
					ty.fn_type(function_parameter_type.as_slice(), has_variadic_parameter)
				})
				.handle_bool(&|_, ty| {
					ty.fn_type(function_parameter_type.as_slice(), has_variadic_parameter)
				})
				.handle_int(&|_, ty| {
					ty.fn_type(function_parameter_type.as_slice(), has_variadic_parameter)
				})
				.handle_unsigned_int(&|_, ty| {
					ty.fn_type(function_parameter_type.as_slice(), has_variadic_parameter)
				})
				.handle_float(&|_, ty| {
					ty.fn_type(function_parameter_type.as_slice(), has_variadic_parameter)
				})
				.handle_str(&|_, ty| {
					ty.fn_type(function_parameter_type.as_slice(), has_variadic_parameter)
				}),
		);

		let function = self.module.add_function(name, llvm_function_type, None);

		if !is_declaration {
			self.generator.context.append_basic_block(function, "entry");
		}

		self.function_table.insert(
			name.to_owned(),
			(
				function_type.0,
				function_type.1,
				has_variadic_parameter,
				function,
			),
		);

		InFunctionGenerator {
			name: name.to_owned(),
			function: function,
			in_module_generator: self,
			variable_table: HashMap::new(),
		}
	}

	pub fn generate_code(&'a mut self, ast: &AST) {
		if ast.name != "module" {
			panic!("module AST expected, got {}.", ast.name);
		}

		let mut main_function = self.new_function("main", (ValueType::Void, vec![]), true, false);

		main_function.generate_code(&ast.children[0]);
		main_function.last_basic_block().builder.build_return(None);

		match self.module.verify() {
			Ok(_) => println!("{}\n\n", self.module.print_to_string()),
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

pub struct InFunctionGenerator<'mdl, 'ctx> {
	pub name: String,
	pub function: FunctionValue<'ctx>,
	pub in_module_generator: &'mdl InModuleGenerator<'ctx>,
	// TODO: Create a scope generator and move below variable table to that.
	pub variable_table: HashMap<String, (ValueType, PointerValue<'ctx>)>,
}

impl<'a, 'mdl: 'a, 'ctx: 'mdl> InFunctionGenerator<'mdl, 'ctx> {
	pub fn new_basic_block(&'a self, name: &str) -> InBasicBlockGenerator<'ctx> {
		let basic_block = self
			.in_module_generator
			.generator
			.context
			.append_basic_block(self.function, name);
		let builder = self.in_module_generator.generator.context.create_builder();

		builder.position_at_end(&basic_block);

		InBasicBlockGenerator {
			name: name.to_owned(),
			basic_block: basic_block,
			builder: builder,
		}
	}

	pub fn last_basic_block(&'a self) -> InBasicBlockGenerator<'ctx> {
		let basic_block = self.function.get_last_basic_block().unwrap();
		let builder = self.in_module_generator.generator.context.create_builder();

		builder.position_at_end(&basic_block);

		InBasicBlockGenerator {
			name: basic_block.get_name().to_str().unwrap().to_owned(),
			basic_block: basic_block,
			builder: builder,
		}
	}

	pub fn allocate_variable(
		&'a mut self,
		name: String,
		value_type: ValueType,
	) -> PointerValue<'ctx> {
		if self.variable_table.contains_key(&name) {
			panic!(
				"multiple variable definition detected; {} is already defined.",
				name
			);
		}

		let variable_address = self.last_basic_block().builder.build_alloca(
			value_type.to_basic_type(&self.in_module_generator.generator.context),
			&name,
		);

		self.variable_table
			.insert(name, (value_type, variable_address));

		variable_address
	}

	pub fn generate_code(&'a mut self, ast: &AST) {
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

	fn generate_statement_code(&'a mut self, ast: &AST) {
		match ast.children[0].name.as_ref() {
			"scope-statement" => self.generate_statement_code_scope(&ast.children[0]),
			"if-statement" => self.generate_statement_code_if(&ast.children[0]),
			"for-statement" => unimplemented!(),
			"with-statement" => unimplemented!(),
			"ret-statement" => unimplemented!(),
			"let-statement" => self.generate_statement_code_let(&ast.children[0]),
			"expression" => {
				self.generate_expression_code(&ast.children[0]);
			}
			_ => unreachable!(),
		};
	}

	fn generate_statement_code_scope(&'a mut self, ast: &AST) {
		if ast.children.len() == 2 {
			return;
		}

		// TODO: Add the local variable control here.

		self.generate_code(&ast.children[1]);
	}

	fn generate_statement_code_if(&'a mut self, ast: &AST) {
		let criteria = self.generate_expression_code(&ast.children[1]);

		if criteria.get_type() != ValueType::Bool {
			panic!(
				"type error; {} type extected, got {} type.",
				ValueType::Bool,
				criteria.get_type()
			);
		}

		let block_criteria = self.last_basic_block();
		let block_then_begin = self.new_basic_block("THEN block");

		self.generate_statement_code_scope(&ast.children[2]);

		let block_then = self.last_basic_block();
		let block_else_begin = self.new_basic_block("ELSE block");

		if ast.children.len() == 5 {
			if ast.children[4].name == "scope-statement" {
				self.generate_statement_code_scope(&ast.children[4]);
			} else {
				self.generate_statement_code_if(&ast.children[4]);
			}
		}

		let block_else = self.last_basic_block();
		let block_end = self.new_basic_block("ENDIF");

		criteria.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
			block_criteria.builder.build_conditional_branch(
				value,
				&block_then_begin.basic_block,
				&block_else_begin.basic_block,
			);
		}));

		block_then
			.builder
			.build_unconditional_branch(&block_end.basic_block);
		block_else
			.builder
			.build_unconditional_branch(&block_end.basic_block);
	}

	fn generate_statement_code_let(&'a mut self, ast: &AST) {
		if ast.children.len() == 4 {
			let initial_value = self.generate_expression_code(&ast.children[3]);

			if initial_value.get_type() == ValueType::Void {
				panic!("type error; {} type is not allowed.", ValueType::Void);
			}

			let variable_address = self.allocate_variable(
				ast.children[1]
					.child
					.as_ref()
					.unwrap()
					.token_content
					.clone(),
				initial_value.get_type(),
			);
			let block = self.last_basic_block();

			initial_value.invoke_handler(
				ValueHandler::new()
					.handle_bool(&|_, value| {
						block.builder.build_store(variable_address, value);
					})
					.handle_int(&|_, value| {
						block.builder.build_store(variable_address, value);
					})
					.handle_unsigned_int(&|_, value| {
						block.builder.build_store(variable_address, value);
					})
					.handle_float(&|_, value| {
						block.builder.build_store(variable_address, value);
					})
					.handle_str(&|_, value| {
						block.builder.build_store(variable_address, value);
					}),
			);
		} else {
			unimplemented!();
		}
	}

	fn generate_expression_code(&'a self, ast: &AST) -> Value<'ctx> {
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
			"op-cast" => {
				if ast.children.len() == 1 {
					self.generate_expression_code(&ast.children[0])
				} else {
					self.generate_expression_code_op_cast(ast)
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

	fn generate_expression_code_assignment(&'a self, _ast: &AST) -> Value<'ctx> {
		unimplemented!();
	}

	fn generate_expression_code_op_or(&'a self, ast: &AST) -> Value<'ctx> {
		// We don't need to create a new basic block for the lhs here,
		// because always there's an basic block that is not terminated yet.
		let lhs = self.generate_expression_code(&ast.children[0]);
		let lhs_block = self.last_basic_block();

		// Create a new basic block and generates rhs.
		// It may create some new basic blocks,
		// so we have to obtain both the first block and the last block.

		let rhs_block_begin = self.new_basic_block("OR rhs");
		let rhs = self.generate_expression_code(&ast.children[2]);
		let rhs_block = self.last_basic_block();

		if lhs.get_type() != ValueType::Bool || rhs.get_type() != ValueType::Bool {
			panic!(
				"type error; boolean operations are only allowed between {}s.",
				ValueType::Bool
			);
		}

		let end_block = self.new_basic_block("OR end");

		lhs.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
			lhs_block.builder.build_conditional_branch(
				value,
				&end_block.basic_block,
				&rhs_block_begin.basic_block,
			);
		}));
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
		result.add_incoming(&[(&rhs.to_basic_value(), &rhs_block.basic_block)]);

		Value::from_basic_value(ValueType::Bool, result.as_basic_value())
	}

	fn generate_expression_code_op_and(&'a self, ast: &AST) -> Value<'ctx> {
		let lhs = self.generate_expression_code(&ast.children[0]);
		let lhs_block = self.last_basic_block();

		let rhs_block_begin = self.new_basic_block("OR rhs");
		let rhs = self.generate_expression_code(&ast.children[2]);
		let rhs_block = self.last_basic_block();

		if lhs.get_type() != ValueType::Bool || rhs.get_type() != ValueType::Bool {
			panic!(
				"type error; boolean operations are only allowed between {}s.",
				ValueType::Bool
			);
		}

		let end_block = self.new_basic_block("AND end");

		lhs.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
			lhs_block.builder.build_conditional_branch(
				value,
				&rhs_block_begin.basic_block,
				&end_block.basic_block,
			);
		}));
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
		result.add_incoming(&[(&rhs.to_basic_value(), &rhs_block.basic_block)]);

		Value::from_basic_value(ValueType::Bool, result.as_basic_value())
	}

	fn generate_expression_code_op_cmp(&'a self, ast: &AST) -> Value<'ctx> {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();
		let matched = self.match_type(&block, lhs, rhs);

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
							value: block.builder.build_int_compare(
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
							value: block.builder.build_int_compare(
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
							value: block.builder.build_int_compare(
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
							value: block.builder.build_float_compare(
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

	fn generate_expression_code_op_addsub(&'a self, ast: &AST) -> Value<'ctx> {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();
		let matched = self.match_type(&block, lhs, rhs);

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
									block.builder.build_int_nsw_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_nsw_add(lhs_value, rhs_value, "ADD")
								},
							},
							16 => Value::I16 {
								value: if subtraction {
									block.builder.build_int_nsw_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_nsw_add(lhs_value, rhs_value, "ADD")
								},
							},
							32 => Value::I32 {
								value: if subtraction {
									block.builder.build_int_nsw_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_nsw_add(lhs_value, rhs_value, "ADD")
								},
							},
							64 => Value::I64 {
								value: if subtraction {
									block.builder.build_int_nsw_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_nsw_add(lhs_value, rhs_value, "ADD")
								},
							},
							128 => Value::I128 {
								value: if subtraction {
									block.builder.build_int_nsw_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_nsw_add(lhs_value, rhs_value, "ADD")
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
									block.builder.build_int_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_add(lhs_value, rhs_value, "ADD")
								},
							},
							16 => Value::U16 {
								value: if subtraction {
									block.builder.build_int_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_add(lhs_value, rhs_value, "ADD")
								},
							},
							32 => Value::U32 {
								value: if subtraction {
									block.builder.build_int_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_add(lhs_value, rhs_value, "ADD")
								},
							},
							64 => Value::U64 {
								value: if subtraction {
									block.builder.build_int_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_add(lhs_value, rhs_value, "ADD")
								},
							},
							128 => Value::U128 {
								value: if subtraction {
									block.builder.build_int_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_int_add(lhs_value, rhs_value, "ADD")
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
									block.builder.build_float_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_float_add(lhs_value, rhs_value, "ADD")
								},
							},
							32 => Value::F32 {
								value: if subtraction {
									block.builder.build_float_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_float_add(lhs_value, rhs_value, "ADD")
								},
							},
							64 => Value::F64 {
								value: if subtraction {
									block.builder.build_float_sub(lhs_value, rhs_value, "SUB")
								} else {
									block.builder.build_float_add(lhs_value, rhs_value, "ADD")
								},
							},
							_ => unreachable!(),
						}
					}))
				}),
		)
	}

	fn generate_expression_code_op_muldivmod(&'a self, ast: &AST) -> Value<'ctx> {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();
		let matched = self.match_type(&block, lhs, rhs);

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
									TokenType::OpMul => {
										block.builder.build_int_nsw_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_signed_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
										.builder
										.build_int_signed_rem(lhs_value, rhs_value, "MOD"),
									_ => unreachable!(),
								},
							},
							16 => Value::I16 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_int_nsw_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_signed_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
										.builder
										.build_int_signed_rem(lhs_value, rhs_value, "MOD"),
									_ => unreachable!(),
								},
							},
							32 => Value::I32 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_int_nsw_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_signed_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
										.builder
										.build_int_signed_rem(lhs_value, rhs_value, "MOD"),
									_ => unreachable!(),
								},
							},
							64 => Value::I64 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_int_nsw_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_signed_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
										.builder
										.build_int_signed_rem(lhs_value, rhs_value, "MOD"),
									_ => unreachable!(),
								},
							},
							128 => Value::I128 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_int_nsw_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_signed_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
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
									TokenType::OpMul => {
										block.builder.build_int_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
										.builder
										.build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
									_ => unreachable!(),
								},
							},
							16 => Value::U16 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_int_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
										.builder
										.build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
									_ => unreachable!(),
								},
							},
							32 => Value::U32 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_int_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
										.builder
										.build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
									_ => unreachable!(),
								},
							},
							64 => Value::U64 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_int_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
										.builder
										.build_int_unsigned_rem(lhs_value, rhs_value, "MOD"),
									_ => unreachable!(),
								},
							},
							128 => Value::U128 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_int_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => block
										.builder
										.build_int_unsigned_div(lhs_value, rhs_value, "DIV"),
									TokenType::OpMod => block
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
									TokenType::OpMul => {
										block.builder.build_float_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => {
										block.builder.build_float_div(lhs_value, rhs_value, "DIV")
									}
									TokenType::OpMod => {
										block.builder.build_float_rem(lhs_value, rhs_value, "MOD")
									}
									_ => unreachable!(),
								},
							},
							32 => Value::F32 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_float_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => {
										block.builder.build_float_div(lhs_value, rhs_value, "DIV")
									}
									TokenType::OpMod => {
										block.builder.build_float_rem(lhs_value, rhs_value, "MOD")
									}
									_ => unreachable!(),
								},
							},
							64 => Value::F64 {
								value: match op_token_type {
									TokenType::OpMul => {
										block.builder.build_float_mul(lhs_value, rhs_value, "MUL")
									}
									TokenType::OpDiv => {
										block.builder.build_float_div(lhs_value, rhs_value, "DIV")
									}
									TokenType::OpMod => {
										block.builder.build_float_rem(lhs_value, rhs_value, "MOD")
									}
									_ => unreachable!(),
								},
							},
							_ => unreachable!(),
						}
					}))
				}),
		)
	}

	fn generate_expression_code_op_shift(&'a self, ast: &AST) -> Value<'ctx> {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();
		let matched = self.match_type(&block, lhs, rhs);

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
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, true, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							16 => Value::I16 {
								value: if right {
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, true, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							32 => Value::I32 {
								value: if right {
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, true, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							64 => Value::I64 {
								value: if right {
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, true, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							128 => Value::I128 {
								value: if right {
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, true, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
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
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, false, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							16 => Value::U16 {
								value: if right {
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, false, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							32 => Value::U32 {
								value: if right {
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, false, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							64 => Value::U64 {
								value: if right {
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, false, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							128 => Value::U128 {
								value: if right {
									block
										.builder
										.build_right_shift(lhs_value, rhs_value, false, "RSH")
								} else {
									block.builder.build_left_shift(lhs_value, rhs_value, "LSH")
								},
							},
							_ => unreachable!(),
						}
					}))
				}),
		)
	}

	fn generate_expression_code_op_bit_or(&'a self, ast: &AST) -> Value<'ctx> {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();
		let matched = self.match_type(&block, lhs, rhs);

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
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							16 => Value::I16 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							32 => Value::I32 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							64 => Value::I64 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							128 => Value::I128 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							_ => unreachable!(),
						}
					}))
				})
				.handle_unsigned_int(&|_, lhs_value| {
					rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
						match rhs.get_type().get_bitwidth() {
							8 => Value::U8 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							16 => Value::U16 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							32 => Value::U32 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							64 => Value::U64 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							128 => Value::U128 {
								value: block.builder.build_or(lhs_value, rhs_value, "BITWISE OR"),
							},
							_ => unreachable!(),
						}
					}))
				}),
		)
	}

	fn generate_expression_code_op_bit_and(&'a self, ast: &AST) -> Value<'ctx> {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();
		let matched = self.match_type(&block, lhs, rhs);

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
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							16 => Value::I16 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							32 => Value::I32 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							64 => Value::I64 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							128 => Value::I128 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							_ => unreachable!(),
						}
					}))
				})
				.handle_unsigned_int(&|_, lhs_value| {
					rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
						match rhs.get_type().get_bitwidth() {
							8 => Value::U8 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							16 => Value::U16 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							32 => Value::U32 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							64 => Value::U64 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							128 => Value::U128 {
								value: block.builder.build_and(lhs_value, rhs_value, "BITWISE AND"),
							},
							_ => unreachable!(),
						}
					}))
				}),
		)
	}

	fn generate_expression_code_op_bit_xor(&'a self, ast: &AST) -> Value<'ctx> {
		let mut lhs = self.generate_expression_code(&ast.children[0]);
		let mut rhs = self.generate_expression_code(&ast.children[2]);

		let block = self.last_basic_block();
		let matched = self.match_type(&block, lhs, rhs);

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
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							16 => Value::I16 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							32 => Value::I32 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							64 => Value::I64 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							128 => Value::I128 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							_ => unreachable!(),
						}
					}))
				})
				.handle_unsigned_int(&|_, lhs_value| {
					rhs.invoke_handler(ValueHandler::new().handle_unsigned_int(&|_, rhs_value| {
						match rhs.get_type().get_bitwidth() {
							8 => Value::U8 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							16 => Value::U16 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							32 => Value::U32 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							64 => Value::U64 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							128 => Value::U128 {
								value: block.builder.build_xor(lhs_value, rhs_value, "BITWISE XOR"),
							},
							_ => unreachable!(),
						}
					}))
				}),
		)
	}

	fn generate_expression_code_op_cast(&'a self, ast: &AST) -> Value<'ctx> {
		let cast_type = ast.children[2].children[0]
			.child
			.as_ref()
			.unwrap()
			.token_type;

		if cast_type == TokenType::KeywordVoid {
			panic!(
				"type error; unable to perform cast to {}s.",
				ValueType::Void
			);
		}

		let operand = self.generate_expression_code(&ast.children[0]);
		let block = self.last_basic_block();

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
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i8_type(),
							"CAST -> i8",
						),
					},
					TokenType::KeywordI16 => Value::I16 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i16_type(),
							"CAST -> i16",
						),
					},
					TokenType::KeywordI32 => Value::I32 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i32_type(),
							"CAST -> i32",
						),
					},
					TokenType::KeywordI64 => Value::I64 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i64_type(),
							"CAST -> i64",
						),
					},
					TokenType::KeywordI128 => Value::I128 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i128_type(),
							"CAST -> i128",
						),
					},
					TokenType::KeywordU8 => Value::U8 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i8_type(),
							"CAST -> u8",
						),
					},
					TokenType::KeywordU16 => Value::U16 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i16_type(),
							"CAST -> u16",
						),
					},
					TokenType::KeywordU32 => Value::U32 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i32_type(),
							"CAST -> u32",
						),
					},
					TokenType::KeywordU64 => Value::U64 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i64_type(),
							"CAST -> u64",
						),
					},
					TokenType::KeywordU128 => Value::U128 {
						value: block.builder.build_int_z_extend(
							value,
							self.in_module_generator.generator.context.i128_type(),
							"CAST -> u128",
						),
					},
					TokenType::KeywordF16 => Value::F16 {
						value: block.builder.build_unsigned_int_to_float(
							value,
							self.in_module_generator.generator.context.f16_type(),
							"CAST -> f16",
						),
					},
					TokenType::KeywordF32 => Value::F32 {
						value: block.builder.build_unsigned_int_to_float(
							value,
							self.in_module_generator.generator.context.f32_type(),
							"CAST -> f32",
						),
					},
					TokenType::KeywordF64 => Value::F64 {
						value: block.builder.build_unsigned_int_to_float(
							value,
							self.in_module_generator.generator.context.f64_type(),
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
						value: block.builder.build_int_compare(
							IntPredicate::NE,
							value,
							match operand.get_type().get_bitwidth() {
								8 => self
									.in_module_generator
									.generator
									.context
									.i8_type()
									.const_int(0, false),
								16 => self
									.in_module_generator
									.generator
									.context
									.i16_type()
									.const_int(0, false),
								32 => self
									.in_module_generator
									.generator
									.context
									.i32_type()
									.const_int(0, false),
								64 => self
									.in_module_generator
									.generator
									.context
									.i64_type()
									.const_int(0, false),
								128 => self
									.in_module_generator
									.generator
									.context
									.i128_type()
									.const_int(0, false),
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
								value: block.builder.build_int_truncate(
									value,
									self.in_module_generator.generator.context.i8_type(),
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
								value: block.builder.build_int_s_extend(
									value,
									self.in_module_generator.generator.context.i16_type(),
									"CAST -> i16",
								),
							}
						} else {
							Value::I16 {
								value: block.builder.build_int_truncate(
									value,
									self.in_module_generator.generator.context.i16_type(),
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
								value: block.builder.build_int_s_extend(
									value,
									self.in_module_generator.generator.context.i32_type(),
									"CAST -> i32",
								),
							}
						} else {
							Value::I32 {
								value: block.builder.build_int_truncate(
									value,
									self.in_module_generator.generator.context.i32_type(),
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
								value: block.builder.build_int_s_extend(
									value,
									self.in_module_generator.generator.context.i64_type(),
									"CAST -> i64",
								),
							}
						} else {
							Value::I64 {
								value: block.builder.build_int_truncate(
									value,
									self.in_module_generator.generator.context.i64_type(),
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
								value: block.builder.build_int_s_extend(
									value,
									self.in_module_generator.generator.context.i128_type(),
									"CAST -> i128",
								),
							}
						}
					}
					TokenType::KeywordU8 => Value::U8 {
						value: if operand.get_type().get_bitwidth() == 8 {
							value
						} else {
							block.builder.build_int_truncate(
								value,
								self.in_module_generator.generator.context.i8_type(),
								"CAST -> u8",
							)
						},
					},
					TokenType::KeywordU16 => Value::U16 {
						value: if operand.get_type().get_bitwidth() == 16 {
							value
						} else if operand.get_type().get_bitwidth() < 16 {
							block.builder.build_int_s_extend(
								value,
								self.in_module_generator.generator.context.i16_type(),
								"CAST -> u16",
							)
						} else {
							block.builder.build_int_truncate(
								value,
								self.in_module_generator.generator.context.i16_type(),
								"CAST -> u16",
							)
						},
					},
					TokenType::KeywordU32 => Value::U32 {
						value: if operand.get_type().get_bitwidth() == 32 {
							value
						} else if operand.get_type().get_bitwidth() < 32 {
							block.builder.build_int_s_extend(
								value,
								self.in_module_generator.generator.context.i32_type(),
								"CAST -> u32",
							)
						} else {
							block.builder.build_int_truncate(
								value,
								self.in_module_generator.generator.context.i32_type(),
								"CAST -> u32",
							)
						},
					},
					TokenType::KeywordU64 => Value::U64 {
						value: if operand.get_type().get_bitwidth() == 64 {
							value
						} else if operand.get_type().get_bitwidth() < 64 {
							block.builder.build_int_s_extend(
								value,
								self.in_module_generator.generator.context.i64_type(),
								"CAST -> u64",
							)
						} else {
							block.builder.build_int_truncate(
								value,
								self.in_module_generator.generator.context.i64_type(),
								"CAST -> u64",
							)
						},
					},
					TokenType::KeywordU128 => Value::U128 {
						value: if operand.get_type().get_bitwidth() == 128 {
							value
						} else {
							block.builder.build_int_s_extend(
								value,
								self.in_module_generator.generator.context.i128_type(),
								"CAST -> u128",
							)
						},
					},
					TokenType::KeywordF16 => Value::F16 {
						value: block.builder.build_signed_int_to_float(
							value,
							self.in_module_generator.generator.context.f16_type(),
							"CAST -> f16",
						),
					},
					TokenType::KeywordF32 => Value::F32 {
						value: block.builder.build_signed_int_to_float(
							value,
							self.in_module_generator.generator.context.f32_type(),
							"CAST -> f32",
						),
					},
					TokenType::KeywordF64 => Value::F64 {
						value: block.builder.build_signed_int_to_float(
							value,
							self.in_module_generator.generator.context.f64_type(),
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
						value: block.builder.build_int_compare(
							IntPredicate::NE,
							value,
							match operand.get_type().get_bitwidth() {
								8 => self
									.in_module_generator
									.generator
									.context
									.i8_type()
									.const_int(0, false),
								16 => self
									.in_module_generator
									.generator
									.context
									.i16_type()
									.const_int(0, false),
								32 => self
									.in_module_generator
									.generator
									.context
									.i32_type()
									.const_int(0, false),
								64 => self
									.in_module_generator
									.generator
									.context
									.i64_type()
									.const_int(0, false),
								128 => self
									.in_module_generator
									.generator
									.context
									.i128_type()
									.const_int(0, false),
								_ => unreachable!(),
							},
							"CAST -> bool",
						),
					},
					TokenType::KeywordI8 => Value::I8 {
						value: if operand.get_type().get_bitwidth() == 8 {
							value
						} else {
							block.builder.build_int_truncate(
								value,
								self.in_module_generator.generator.context.i8_type(),
								"CAST -> u8",
							)
						},
					},
					TokenType::KeywordI16 => Value::I16 {
						value: if operand.get_type().get_bitwidth() == 16 {
							value
						} else if operand.get_type().get_bitwidth() < 16 {
							block.builder.build_int_z_extend(
								value,
								self.in_module_generator.generator.context.i16_type(),
								"CAST -> i16",
							)
						} else {
							block.builder.build_int_truncate(
								value,
								self.in_module_generator.generator.context.i16_type(),
								"CAST -> i16",
							)
						},
					},
					TokenType::KeywordI32 => Value::I32 {
						value: if operand.get_type().get_bitwidth() == 32 {
							value
						} else if operand.get_type().get_bitwidth() < 32 {
							block.builder.build_int_z_extend(
								value,
								self.in_module_generator.generator.context.i32_type(),
								"CAST -> i32",
							)
						} else {
							block.builder.build_int_truncate(
								value,
								self.in_module_generator.generator.context.i32_type(),
								"CAST -> i32",
							)
						},
					},
					TokenType::KeywordI64 => Value::I64 {
						value: if operand.get_type().get_bitwidth() == 64 {
							value
						} else if operand.get_type().get_bitwidth() < 64 {
							block.builder.build_int_z_extend(
								value,
								self.in_module_generator.generator.context.i64_type(),
								"CAST -> i64",
							)
						} else {
							block.builder.build_int_truncate(
								value,
								self.in_module_generator.generator.context.i64_type(),
								"CAST -> i64",
							)
						},
					},
					TokenType::KeywordI128 => Value::I128 {
						value: if operand.get_type().get_bitwidth() == 128 {
							value
						} else {
							block.builder.build_int_z_extend(
								value,
								self.in_module_generator.generator.context.i128_type(),
								"CAST -> i128",
							)
						},
					},
					TokenType::KeywordU8 => {
						if operand.get_type().get_bitwidth() == 8 {
							operand
						} else {
							Value::U8 {
								value: block.builder.build_int_truncate(
									value,
									self.in_module_generator.generator.context.i8_type(),
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
								value: block.builder.build_int_s_extend(
									value,
									self.in_module_generator.generator.context.i16_type(),
									"CAST -> u16",
								),
							}
						} else {
							Value::U16 {
								value: block.builder.build_int_truncate(
									value,
									self.in_module_generator.generator.context.i16_type(),
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
								value: block.builder.build_int_s_extend(
									value,
									self.in_module_generator.generator.context.i32_type(),
									"CAST -> u32",
								),
							}
						} else {
							Value::U32 {
								value: block.builder.build_int_truncate(
									value,
									self.in_module_generator.generator.context.i32_type(),
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
								value: block.builder.build_int_s_extend(
									value,
									self.in_module_generator.generator.context.i64_type(),
									"CAST -> u64",
								),
							}
						} else {
							Value::U64 {
								value: block.builder.build_int_truncate(
									value,
									self.in_module_generator.generator.context.i64_type(),
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
								value: block.builder.build_int_s_extend(
									value,
									self.in_module_generator.generator.context.i128_type(),
									"CAST -> u128",
								),
							}
						}
					}
					TokenType::KeywordF16 => Value::F16 {
						value: block.builder.build_unsigned_int_to_float(
							value,
							self.in_module_generator.generator.context.f16_type(),
							"CAST -> f16",
						),
					},
					TokenType::KeywordF32 => Value::F32 {
						value: block.builder.build_unsigned_int_to_float(
							value,
							self.in_module_generator.generator.context.f32_type(),
							"CAST -> f32",
						),
					},
					TokenType::KeywordF64 => Value::F64 {
						value: block.builder.build_unsigned_int_to_float(
							value,
							self.in_module_generator.generator.context.f64_type(),
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
						value: block.builder.build_float_compare(
							FloatPredicate::ONE,
							value,
							match operand.get_type().get_bitwidth() {
								16 => self
									.in_module_generator
									.generator
									.context
									.f16_type()
									.const_float(0.0),
								32 => self
									.in_module_generator
									.generator
									.context
									.f32_type()
									.const_float(0.0),
								64 => self
									.in_module_generator
									.generator
									.context
									.f64_type()
									.const_float(0.0),
								_ => unreachable!(),
							},
							"CAST -> bool",
						),
					},
					TokenType::KeywordI8 => Value::I8 {
						value: block.builder.build_float_to_signed_int(
							value,
							self.in_module_generator.generator.context.i8_type(),
							"CAST -> i8",
						),
					},
					TokenType::KeywordI16 => Value::I16 {
						value: block.builder.build_float_to_signed_int(
							value,
							self.in_module_generator.generator.context.i16_type(),
							"CAST -> i16",
						),
					},
					TokenType::KeywordI32 => Value::I32 {
						value: block.builder.build_float_to_signed_int(
							value,
							self.in_module_generator.generator.context.i32_type(),
							"CAST -> i32",
						),
					},
					TokenType::KeywordI64 => Value::I64 {
						value: block.builder.build_float_to_signed_int(
							value,
							self.in_module_generator.generator.context.i64_type(),
							"CAST -> i64",
						),
					},
					TokenType::KeywordI128 => Value::I128 {
						value: block.builder.build_float_to_signed_int(
							value,
							self.in_module_generator.generator.context.i128_type(),
							"CAST -> i128",
						),
					},
					TokenType::KeywordU8 => Value::U8 {
						value: block.builder.build_float_to_unsigned_int(
							value,
							self.in_module_generator.generator.context.i8_type(),
							"CAST -> u8",
						),
					},
					TokenType::KeywordU16 => Value::U16 {
						value: block.builder.build_float_to_unsigned_int(
							value,
							self.in_module_generator.generator.context.i16_type(),
							"CAST -> u16",
						),
					},
					TokenType::KeywordU32 => Value::U32 {
						value: block.builder.build_float_to_unsigned_int(
							value,
							self.in_module_generator.generator.context.i32_type(),
							"CAST -> u32",
						),
					},
					TokenType::KeywordU64 => Value::U64 {
						value: block.builder.build_float_to_unsigned_int(
							value,
							self.in_module_generator.generator.context.i64_type(),
							"CAST -> u64",
						),
					},
					TokenType::KeywordU128 => Value::U128 {
						value: block.builder.build_float_to_unsigned_int(
							value,
							self.in_module_generator.generator.context.i128_type(),
							"CAST -> u128",
						),
					},
					TokenType::KeywordF16 => {
						if operand.get_type().get_bitwidth() == 16 {
							operand
						} else {
							Value::F16 {
								value: block.builder.build_float_trunc(
									value,
									self.in_module_generator.generator.context.f16_type(),
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
								value: block.builder.build_float_ext(
									value,
									self.in_module_generator.generator.context.f32_type(),
									"CAST -> f32",
								),
							}
						} else {
							Value::F32 {
								value: block.builder.build_float_trunc(
									value,
									self.in_module_generator.generator.context.f32_type(),
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
								value: block.builder.build_float_ext(
									value,
									self.in_module_generator.generator.context.f64_type(),
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

	fn generate_expression_code_op_single(&'a self, ast: &AST) -> Value<'ctx> {
		match ast.children[0].child.as_ref().unwrap().token_type {
			TokenType::KeywordFrom => unimplemented!(), // TODO: Implement the from syntax.
			TokenType::ParenL => self.generate_expression_code(&ast.children[1]),
			TokenType::OpAdd => self.generate_expression_code(&ast.children[1]),
			TokenType::OpSub => self.generate_expression_code_op_neg(&ast),
			TokenType::OpNot => self.generate_expression_code_op_not(&ast),
			TokenType::OpBitNot => self.generate_expression_code_op_bit_not(&ast),
			_ => unreachable!(),
		}
	}

	fn generate_expression_code_op_neg(&'a self, ast: &AST) -> Value<'ctx> {
		let lhs = self.generate_expression_code(&ast.children[1]);

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

		let block = self.last_basic_block();

		lhs.invoke_handler(
			ValueHandler::new()
				.handle_int(&|_, lhs_value| match lhs.get_type().get_bitwidth() {
					8 => Value::I8 {
						value: block.builder.build_int_nsw_sub(
							self.in_module_generator
								.generator
								.context
								.i8_type()
								.const_int(0, false),
							lhs_value,
							"NEG",
						),
					},
					16 => Value::I16 {
						value: block.builder.build_int_nsw_sub(
							self.in_module_generator
								.generator
								.context
								.i16_type()
								.const_int(0, false),
							lhs_value,
							"NEG",
						),
					},
					32 => Value::I32 {
						value: block.builder.build_int_nsw_sub(
							self.in_module_generator
								.generator
								.context
								.i32_type()
								.const_int(0, false),
							lhs_value,
							"NEG",
						),
					},
					64 => Value::I64 {
						value: block.builder.build_int_nsw_sub(
							self.in_module_generator
								.generator
								.context
								.i64_type()
								.const_int(0, false),
							lhs_value,
							"NEG",
						),
					},
					128 => Value::I128 {
						value: block.builder.build_int_nsw_sub(
							self.in_module_generator
								.generator
								.context
								.i128_type()
								.const_int(0, false),
							lhs_value,
							"NEG",
						),
					},
					_ => unreachable!(),
				})
				.handle_float(&|_, lhs_value| match lhs.get_type().get_bitwidth() {
					16 => Value::F16 {
						value: block.builder.build_float_neg(lhs_value, "NEG"),
					},
					32 => Value::F32 {
						value: block.builder.build_float_neg(lhs_value, "NEG"),
					},
					64 => Value::F64 {
						value: block.builder.build_float_neg(lhs_value, "NEG"),
					},
					_ => unreachable!(),
				}),
		)
	}

	fn generate_expression_code_op_not(&'a self, ast: &AST) -> Value<'ctx> {
		let lhs = self.generate_expression_code(&ast.children[1]);

		if lhs.get_type() != ValueType::Bool {
			panic!(
				"type error; boolean operations are only allowed between {}s.",
				ValueType::Bool
			);
		}

		let lhs_block = self.last_basic_block();

		let result = lhs.invoke_handler(ValueHandler::new().handle_bool(&|_, value| {
			lhs_block.builder.build_int_compare(
				IntPredicate::EQ,
				value,
				self.in_module_generator
					.generator
					.context
					.bool_type()
					.const_int(0, false),
				"NOT",
			)
		}));

		Value::Bool { value: result }
	}

	fn generate_expression_code_op_bit_not(&'a self, ast: &AST) -> Value<'ctx> {
		let lhs = self.generate_expression_code(&ast.children[1]);

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

		let block = self.last_basic_block();

		lhs.invoke_handler(
			ValueHandler::new()
				.handle_int(&|_, lhs_value| match lhs.get_type().get_bitwidth() {
					8 => Value::I8 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					16 => Value::I16 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					32 => Value::I32 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					64 => Value::I64 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					128 => Value::I128 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					_ => unreachable!(),
				})
				.handle_unsigned_int(&|_, lhs_value| match lhs.get_type().get_bitwidth() {
					8 => Value::U8 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					16 => Value::U16 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					32 => Value::U32 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					64 => Value::U64 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					128 => Value::U128 {
						value: block.builder.build_not(lhs_value, "BITWISE NOT"),
					},
					_ => unreachable!(),
				}),
		)
	}

	fn generate_expression_code_function_call(&'a self, ast: &AST) -> Value<'ctx> {
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
				.filter(|ast| ast.name == "expression")
				.map(|ast| self.generate_expression_code(ast))
				.collect();
		}

		let function_name = &ast.children[0].child.as_ref().unwrap().token_content;
		let function = match self.in_module_generator.function_table.get(function_name) {
			Some(function) => function,
			None => {
				panic!(
					"unknown function detected; unable to find {} function.",
					function_name
				);
			}
		};

		if function.2 && parameter_value_vec.len() < function.1.len() {
			panic!(
					"wrong argument passed; {} function takes at least {} arguments, but {} was supplied.",
					function_name,
					function.1.len(),
					parameter_value_vec.len()
				);
		} else if !function.2 && parameter_value_vec.len() != function.1.len() {
			panic!(
				"wrong argument passed; {} function takes {} arguments, but {} was supplied.",
				function_name,
				function.1.len(),
				parameter_value_vec.len()
			);
		}

		for param_index in 0..function.1.len() {
			if parameter_value_vec[param_index].get_type() != function.1[param_index] {
				panic!(
					"wrong argument passed; {} function takes {} type as {}th argument, but {} type was supplied.",
					function_name,
					function.1[param_index],
					param_index + 1,
					parameter_value_vec[param_index].get_type()
				);
			}
		}

		let result = self.last_basic_block().builder.build_call(
			function.3,
			&parameter_value_vec
				.iter()
				.map(|value| value.to_basic_value())
				.collect::<Vec<BasicValueEnum<'ctx>>>(),
			"CALL",
		);

		match result.try_as_basic_value() {
			Either::Left(basic_value) => Value::from_basic_value(function.0, basic_value),
			Either::Right(_) => Value::Void,
		}
	}

	fn generate_expression_code_left_value(&'a self, ast: &AST) -> Value<'ctx> {
		let variable_name = &ast.children[0].child.as_ref().unwrap().token_content;
		let basic_block = self.last_basic_block();

		match self.variable_table.get(variable_name) {
			Some((value_type, address)) => Value::from_basic_value(
				*value_type,
				basic_block.builder.build_load(*address, "LOAD"),
			),
			None => {
				panic!(
					"undefined variable detected; {} is not defined in current scope.",
					variable_name
				);
			}
		}
	}

	fn generate_expression_code_literal(&'a self, ast: &AST) -> Value<'ctx> {
		let content = &ast.children[0].child.as_ref().unwrap().token_content;

		match ast.children[0].name.as_ref() {
			"LiteralBool" => Value::Bool {
				value: self
					.in_module_generator
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
			},
			"LiteralInteger" => Value::I32 {
				value: self
					.in_module_generator
					.generator
					.context
					.i32_type()
					.const_int(
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
				value: self
					.in_module_generator
					.generator
					.context
					.f32_type()
					.const_float(content.parse::<f64>().unwrap()),
			},
			"LiteralString" => Value::Str {
				value: self
					.last_basic_block()
					.builder
					.build_global_string_ptr(content, "str literal")
					.as_pointer_value(),
			},
			_ => unreachable!(),
		}
	}

	pub fn match_type(
		&'a self,
		in_basic_block_generator: &InBasicBlockGenerator<'ctx>,
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
							value: in_basic_block_generator.builder.build_int_s_extend(
								value,
								self.in_module_generator.generator.context.i16_type(),
								"CAST -> i16",
							),
						},
						ValueType::I32 => Value::I32 {
							value: in_basic_block_generator.builder.build_int_s_extend(
								value,
								self.in_module_generator.generator.context.i32_type(),
								"CAST -> i32",
							),
						},
						ValueType::I64 => Value::I64 {
							value: in_basic_block_generator.builder.build_int_s_extend(
								value,
								self.in_module_generator.generator.context.i64_type(),
								"CAST -> i64",
							),
						},
						ValueType::I128 => Value::I128 {
							value: in_basic_block_generator.builder.build_int_s_extend(
								value,
								self.in_module_generator.generator.context.i128_type(),
								"CAST -> i128",
							),
						},
						_ => unreachable!(),
					})
					.handle_unsigned_int(&|_, value| match to {
						ValueType::U16 => Value::U16 {
							value: in_basic_block_generator.builder.build_int_z_extend(
								value,
								self.in_module_generator.generator.context.i16_type(),
								"CAST -> u16",
							),
						},
						ValueType::U32 => Value::U32 {
							value: in_basic_block_generator.builder.build_int_z_extend(
								value,
								self.in_module_generator.generator.context.i32_type(),
								"CAST -> u32",
							),
						},
						ValueType::U64 => Value::U64 {
							value: in_basic_block_generator.builder.build_int_z_extend(
								value,
								self.in_module_generator.generator.context.i64_type(),
								"CAST -> u64",
							),
						},
						ValueType::U128 => Value::U128 {
							value: in_basic_block_generator.builder.build_int_z_extend(
								value,
								self.in_module_generator.generator.context.i128_type(),
								"CAST -> u128",
							),
						},
						_ => unreachable!(),
					})
					.handle_float(&|_, value| match to {
						ValueType::F32 => Value::F32 {
							value: in_basic_block_generator.builder.build_float_ext(
								value,
								self.in_module_generator.generator.context.f32_type(),
								"CAST -> f32",
							),
						},
						ValueType::F64 => Value::F64 {
							value: in_basic_block_generator.builder.build_float_ext(
								value,
								self.in_module_generator.generator.context.f64_type(),
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
}

pub struct InBasicBlockGenerator<'ctx> {
	pub name: String,
	pub basic_block: BasicBlock,
	pub builder: Builder<'ctx>,
}
