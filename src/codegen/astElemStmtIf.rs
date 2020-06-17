use either::Either;
use either::Either::{Left, Right};
use inkwell::context::Context;
use inkwell::values::PointerValue;

use super::astElem::{new_expression, ASTElemExp, ASTElemStmt};
use super::astElemStmtScope::ASTElemStmtScope;
use super::astType::Type;
use super::gen::{FuncBlockGen, FuncPrototype, ModuleGen};
use super::value::{Value, ValueHandler, ValueType, ValueTypeHandler};
use crate::lexer::Token;
use crate::parser::AST;

use std::boxed::Box;

pub struct ASTElemStmtIf {
	begin: Token,
	criteria: Box<dyn ASTElemExp>,
	then_block: ASTElemStmtScope,
	else_block: Option<Either<ASTElemStmtScope, Box<ASTElemStmtIf>>>,
}

impl ASTElemStmtIf {
	pub fn new(ast: &AST) -> ASTElemStmtIf {
		if ast.name != "if-statement" {
			panic!("if-statement AST expected, got {}.", ast.name);
		}

		if ast.children.len() == 3 {
			ASTElemStmtIf {
				begin: ast.children[0].child.as_ref().unwrap().clone(),
				criteria: new_expression(&ast.children[1]),
				then_block: ASTElemStmtScope::new(&ast.children[2]),
				else_block: None,
			}
		} else if ast.children[4].name == "scope-statement" {
			ASTElemStmtIf {
				begin: ast.children[0].child.as_ref().unwrap().clone(),
				criteria: new_expression(&ast.children[1]),
				then_block: ASTElemStmtScope::new(&ast.children[2]),
				else_block: Some(Left(ASTElemStmtScope::new(&ast.children[4]))),
			}
		} else {
			ASTElemStmtIf {
				begin: ast.children[0].child.as_ref().unwrap().clone(),
				criteria: new_expression(&ast.children[1]),
				then_block: ASTElemStmtScope::new(&ast.children[2]),
				else_block: Some(Right(Box::new(ASTElemStmtIf::new(&ast.children[4])))),
			}
		}
	}
}

impl ASTElemStmt for ASTElemStmtIf {
	fn get_name(&self) -> &str {
		"if"
	}

	fn begin_token(&self) -> Token {
		self.begin.clone()
	}

	fn end_token(&self) -> Token {
		if self.else_block.is_none() {
			self.then_block.end_token()
		} else {
			self.else_block.as_ref().unwrap().as_ref().either(
				|scope_statement| scope_statement.end_token(),
				|if_statement| if_statement.end_token(),
			)
		}
	}

	fn gen_code<'fnc, 'mdl: 'fnc, 'ctx: 'mdl>(
		&self,
		ctx: &'ctx Context,
		mdl: &'mdl mut ModuleGen<'ctx>,
		fnc: &'fnc mut FuncBlockGen<'ctx>,
	) {
		if self.criteria.get_type() != Type::Bool {
			panic!(
				"{} type expected, got {}.",
				Type::Bool,
				self.criteria.get_type()
			);
		}

		unimplemented!();
	}
}
