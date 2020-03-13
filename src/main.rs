mod codegen;
mod lexer;
mod parser;
mod rule;

extern crate serde;
extern crate serde_json;

use codegen::generator::Generator as CodeGenerator;
use codegen::value::ValueType;
use inkwell::OptimizationLevel;
use parser::Parser;
use rule::generator::action_table::ActionTable;
use rule::generator::generator::Generator;
use rule::parser::Parser as RuleParser;

use serde::{Deserialize, Serialize};

use std::fs;

#[derive(Serialize, Deserialize)]
struct ActionTableWrapper {
    action_table: ActionTable,
}

fn main() {
    // generate_action_table();
    match fs::read_to_string("sources/test.tl") {
        Ok(content) => do_compile(content),
        Err(err) => println!("{}", err),
    }
}

fn generate_action_table() {
    match fs::read_to_string("rules/main.rule") {
        Ok(content) => {
            let mut rule_parser = RuleParser::new(content);
            let mut generator = Generator::new(rule_parser.parse());

            generator.generate_first();
            generator.generate_dag();
            generator.generate_action_table();

            let action_table = ActionTableWrapper {
                action_table: generator.action_table,
            };

            fs::write(
                "rules/main.rule.json",
                serde_json::to_string(&action_table).unwrap(),
            )
            .expect("failed to write json");
        }
        Err(err) => println!("Error: {}", err),
    }
}

fn do_compile(content: String) {
    match fs::read_to_string("rules/main.rule.json") {
        Ok(json) => {
            let action_table: ActionTableWrapper =
                serde_json::from_str(&json).expect("failed to read json");

            let parser = Parser::new(action_table.action_table);
            let ast = parser.parse(content);

            match ast {
                Ok(ast) => {
                    let mut codegen = CodeGenerator::new();
                    let mut module = codegen.create_module("test");

                    // DELETEME: Addings some c-std functions.
                    module.decl_function("printf", ValueType::I32, vec![ValueType::Str], true);
                    module.decl_function("rand", ValueType::I32, vec![], false);
                    module.decl_function("srand", ValueType::Void, vec![ValueType::U64], false);
                    module.decl_function("time", ValueType::U64, vec![ValueType::U64], false);

                    module.generate_code(&ast);

                    // DELETEME: JIT execution.
                    let engine = module
                        .module
                        .create_jit_execution_engine(OptimizationLevel::None)
                        .expect("no jit execution engine found.");

                    type MainFunction = unsafe extern "C" fn();

                    unsafe {
                        let jit_main_function = engine
                            .get_function::<MainFunction>("main")
                            .expect("no main function found.");

                        jit_main_function.call();
                    }
                }
                Err(err) => println!("{}", err),
            };
        }
        Err(err) => println!("Error: {}", err),
    }
}
