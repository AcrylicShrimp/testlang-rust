mod codegen;
mod lexer;
mod parser;
mod rule;

use codegen::generator::Generator as CodeGenerator;
use parser::Parser;
use rule::generator::generator::Generator;
use rule::parser::Parser as RuleParser;

use std::fs;

fn main() {
    match fs::read_to_string("rules/main.rule") {
        Ok(content) => do_compile(content),
        Err(err) => println!("{}", err),
    }
}

fn do_compile(content: String) {
    let mut rule_parser = RuleParser::new(content);
    let mut generator = Generator::new(rule_parser.parse());

    generator.generate_first();
    generator.generate_dag();
    generator.generate_action_table();

    println!("{}", generator.rule_table.rule_vec.len());
    println!("{}", generator.state_graph.state_map.len());
    println!("{}", generator.state_graph.state_transition_map.len());

    let parser = Parser::new(generator.action_table);
    let ast = parser.parse(fs::read_to_string("sources/test.tl").unwrap());

    match ast {
        Ok(ast) => {
            let codegen = CodeGenerator::new();
            let mut module = codegen.new_module("test");

            module.generate_code(&ast);
        }
        Err(err) => println!("{}", err),
    };
}
