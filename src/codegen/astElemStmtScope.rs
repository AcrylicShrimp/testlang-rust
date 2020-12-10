use inkwell::context::Context;

use super::astElem::{new_statement_list, ASTElemStmt};
use super::gen::{FuncBlockGen, ModuleGen};
use crate::lexer::Token;
use crate::parser::AST;

use std::boxed::Box;
use std::vec::Vec;

pub struct ASTElemStmtScope {
	begin: Token,
	end: Token,
	statement_list: Vec<Box<dyn ASTElemStmt>>,
}

impl ASTElemStmtScope {
	pub fn new(ast: &AST, need_cleanup: bool) -> ASTElemStmtScope {
		if ast.name != "scope-statement" {
			panic!("scope-statement AST expected, got {}.", ast.name);
		}

		if ast.children.len() == 2 {
			ASTElemStmtScope {
				begin: ast.children[0].child.as_ref().unwrap().clone(),
				end: ast.children[1].child.as_ref().unwrap().clone(),
				statement_list: vec![],
			}
		} else {
			ASTElemStmtScope {
				begin: ast.children[0].child.as_ref().unwrap().clone(),
				end: ast.children[2].child.as_ref().unwrap().clone(),
				statement_list: new_statement_list(&ast.children[1]),
			}
		}
	}
}

impl ASTElemStmt for ASTElemStmtScope {
	fn get_name(&self) -> &str {
		"scope"
	}

	fn begin_token(&self) -> Token {
		self.begin.clone()
	}

	fn end_token(&self) -> Token {
		self.end.clone()
	}

	fn gen_code<'fnc, 'mdl: 'fnc, 'ctx: 'mdl>(
		&self,
		ctx: &'ctx Context,
		mdl: &'mdl mut ModuleGen<'ctx>,
		fnc: &'fnc mut FuncBlockGen<'ctx>,
	) {
		// Do we really need to call llvm.stacksave and llvm.stackrestore here?
		// Yes, but maybe we can reduce them somehow.
		fnc.func.create_scope(mdl, &mut fnc.last_block, true);

		for statement in self.statement_list.iter() {
			statement.gen_code(ctx, mdl, fnc);
		}

		fnc.func.end_scope(mdl, &mut fnc.last_block);
	}
}
