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

    let ast = parser.parse(
        "
        let lhs = rand();
        let rhs = rand();

        printf('%d, %d\\n', lhs, rhs);
        printf('%d\\n', lhs | rhs);
        printf('%d\\n', lhs & rhs);
        printf('%d\\n', lhs ^ rhs);
    "
        .to_string(),
    );

    match ast {
        Ok(ast) => {
            let codegen = CodeGenerator::new();
            let mut module = codegen.new_module("test");

            module.generate_code(&ast);
        }
        Err(err) => println!("{}", err),
    };

    // println!("=============================================");

    // let mut state_num = 0;

    // for state in &generator.state_graph.state_vec {
    //     println!("!!! State #{} !!!", state_num);

    //     for rule in state.iter() {
    //         let src_rule = &generator.rule_table.rule_vec[rule.rule_index];
    //         let mut rule_item_index = 0;

    //         print!("{} ->", src_rule.name);

    //         for rule_item in &src_rule.item_vec {
    //             if rule_item_index == rule.seen_index {
    //                 print!(" .");
    //             }

    //             print!(" {}", rule_item.item_content);

    //             rule_item_index += 1;
    //         }

    //         if rule_item_index == rule.seen_index {
    //             print!(" •");
    //         } else {
    //             print!("");
    //         }

    //         if rule.lookahead.is_empty() {
    //             println!(" / $");
    //         } else {
    //             println!(" / {}", rule.lookahead);
    //         }
    //     }

    //     state_num += 1;
    // }

    // println!("=============================================");

    // for (transition_src, state_index) in generator.state_graph.state_transition_map {
    //     println!(
    //         "{}, {} -> {}",
    //         transition_src.0, transition_src.1, state_index
    //     );
    // }

    // println!("=============================================");
}
