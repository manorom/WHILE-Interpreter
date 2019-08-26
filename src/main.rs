mod expression;
//mod tokenize;
mod environment;
mod walker_interpreter;
mod console_executor;
mod token;
mod lexer;
mod interpreter_settings;

use std::fs;

extern crate clap;

use clap::Arg;


fn main() {
    let matches = clap::App::new("A WHILE program interpeter")
                    .about("Interprets and executes a WHILE program")
                    .arg(Arg::with_name("INPUT")
                            .help("Text file containing the WHILE program")
                            .required(true)
                            .index(1))
                    .get_matches();
    
    let filename = matches.value_of("INPUT").unwrap();

    println!("Reading source in: {} ...", filename);
    let source_code = fs::read_to_string(filename).expect("Could not read input file");

    println!("Parsing source code...");
    let settings = interpreter_settings::InterpreterSettings::default();
    let lex_result = lexer::Lexer::new(&source_code, settings.clone()).collect::<Vec<_>>();
    for maybe_token in lex_result {
        match maybe_token {
            Ok(token) => println!("{}", token),
            Err(e) => println!("{}", e),
        }
    }
    
    let parse_result = expression::Expression::compile_from_tokens(lexer::Lexer::new(&source_code, settings));
    match parse_result {
        Err(parse_error) => println!("Could not parse source code\n{}", parse_error),
        Ok(expr) => {
            console_executor::ConsoleExecutor::new_from_expr(&source_code, &expr, 100).run();

        }
    }
}
