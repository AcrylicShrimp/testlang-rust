use super::astElemStmtScope::ASTElemStmtScope;
use super::astType::Type;
use super::gen::{BlockGen, FuncGen, ModuleGen};
use super::value::{Value, ValueType};
use crate::lexer::Token;
use crate::parser::AST;
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::values::{BasicValueEnum, PointerValue};
use std::collections::HashMap;

pub struct Scope<'parent, 'ctx> {
	pub parent: Option<&'parent Scope<'parent, 'ctx>>,
	pub variable_map: HashMap<String, (ValueType, PointerValue<'ctx>)>,
	pub stack_position: Option<BasicValueEnum<'ctx>>,
}

impl<'parent, 'ctx: 'parent> Scope<'parent, 'ctx> {
	pub fn new() -> Scope<'parent, 'ctx> {
		Scope {
			parent: None,
			variable_map: HashMap::new(),
			stack_position: None,
		}
	}

	pub fn end<'fnc, 'mdl: 'fnc>(
		self,
		module: &'mdl mut ModuleGen<'ctx>,
		last_block: &'fnc mut BlockGen<'ctx>,
	) {
		match self.stack_position {
			Some(stack_position) => {
				last_block
					.builder
					.build_call(
						module
							.function_prototype_table
							.get("llvm.stackrestore")
							.expect("intrinsic function 'llvm.stackrestore' not found.")
							.function,
						vec![stack_position].as_slice(),
						"stackrestore",
					)
					.try_as_basic_value()
					.left()
					.unwrap();
			}
			None => {}
		}
	}

	pub fn subscope<'this, 'fnc: 'this, 'mdl: 'fnc>(
		&'this self,
		module: &'mdl mut ModuleGen<'ctx>,
		last_block: &'fnc mut BlockGen<'ctx>,
	) -> Scope<'this, 'ctx> {
		Scope {
			parent: Some(self),
			variable_map: HashMap::new(),
			stack_position: Some(
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
			),
		}
	}

	pub fn resolve_variable(&self, variable_name: &str) -> Option<(ValueType, PointerValue<'ctx>)> {
		match self.variable_map.get(variable_name) {
			Some(&(value_type, pointer_value)) => Some((value_type, pointer_value)),
			None => match self.parent {
				Some(parent) => parent.resolve_variable(variable_name),
				None => None,
			},
		}
	}
}

pub trait ASTElemStmt {
	fn get_name(&self) -> &str;
	fn begin_token(&self) -> Token;
	fn end_token(&self) -> Token;
	fn gen_code<'fnc, 'mdl: 'fnc, 'ctx: 'mdl>(
		&self,
		ctx: &'ctx Context,
		mdl: &'mdl mut ModuleGen<'ctx>,
		fnc: &'fnc mut FuncBlockGen<'ctx>,
	);
	// Scope? (for static analysis)
}

pub trait ASTElemStmtLoop {
	fn get_begin<'ctx>(&self) -> BasicBlock<'ctx>;
	fn get_end<'ctx>(&self) -> BasicBlock<'ctx>;
}

pub trait ASTElemStmtInLoop {
	fn gen_code<'fnc, 'mdl: 'fnc, 'ctx: 'mdl>(
		&self,
		ctx: &'ctx Context,
		mdl: &'mdl mut ModuleGen<'ctx>,
		fnc: &'fnc mut FuncBlockGen<'ctx>,
		closest_loop: &dyn ASTElemStmtLoop,
	);
}

pub trait ASTElemExp {
	fn get_name(&self) -> &str;
	fn get_type(&self) -> Type;
	fn begin_token(&self) -> Token;
	fn end_token(&self) -> Token;
	fn obtain_address(&self) -> Option<PointerValue>; // Obtain address to be assigned if any.
	fn gen_code<'fnc, 'mdl: 'fnc, 'ctx: 'mdl>(
		&self,
		ctx: &'ctx Context,
		mdl: &'mdl mut ModuleGen<'ctx>,
		fnc: &'fnc mut FuncBlockGen<'ctx>,
	) -> Value;
}

pub fn new_statement(ast: &AST) -> Box<dyn ASTElemStmt> {
	if ast.name != "statement" {
		panic!("statement AST expected, got {}.", ast.name);
	}

	match ast.children[0].name.as_ref() {
		"scope-statement" => Box::new(ASTElemStmtScope::new(&ast.children[0])),
		_ => unreachable!(),
	}
}

pub fn new_statement_list(ast: &AST) -> Vec<Box<dyn ASTElemStmt>> {
	if ast.name != "statement-list" {
		panic!("statement-list AST expected, got {}.", ast.name);
	}

	if ast.children.is_empty() {
		panic!("statement-list cannot be empty.");
	}

	let mut statement_ast_stack = ast.children.iter().rev().collect::<Vec<&AST>>();

	while statement_ast_stack.last().unwrap().name == "statement-list" {
		let mut statement_vec = statement_ast_stack
			.pop()
			.unwrap()
			.children
			.iter()
			.rev()
			.collect::<Vec<&AST>>();

		statement_ast_stack.append(&mut statement_vec);
	}

	statement_ast_stack
		.iter()
		.rev()
		.map(|ast| new_statement(ast))
		.collect()
}

pub fn new_expression(ast: &AST) -> Box<dyn ASTElemExp> {
	if ast.name != "expression" {
		panic!("expression AST expected, got {}.", ast.name);
	}

	unimplemented!();
}
