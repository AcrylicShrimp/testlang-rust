use inkwell::context::Context;
use inkwell::values::PointerValue;

use super::astElemStmtScope::ASTElemStmtScope;
use super::astType::Type;
use super::gen::{FuncBlockGen, ModuleGen};
use super::value::{Value, ValueType};
use crate::lexer::Token;
use crate::parser::AST;

use std::boxed::Box;
use std::vec::Vec;

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
