mod ast;
mod console_executor;
mod environment;
mod expression;
mod lexer;
mod settings;
mod token;
mod walker_interpreter;
mod interpreter;

use clap::Arg;
use std::fs;

fn main() {
    let matches = clap::App::new("A WHILE program interpeter")
        .about("Interprets and executes a WHILE program")
        .arg(
            Arg::with_name("INPUT")
                .help("Text file containing the WHILE program")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("list-tokens")
                .short("l")
                .long("list-tokens")
                .help("Lists recognized tokens in the input file"),
        )
        .arg(
            Arg::with_name("allow-arbitrary-constants-assign")
                .long("allow-arbitrary-constants-assign")
                .help("Allows arbitrary constants in assign expressions"),
        )
        .get_matches();

    let filename = matches.value_of("INPUT").unwrap();

    let mut settings = settings::Settings::default();

    if matches.occurrences_of("allow-arbitrary-constants-assign") > 0 {
        settings.allow_arbitrary_constants_assign = true;
    }

    println!("Reading source in: {} ...", filename);
    let source_code = fs::read_to_string(filename).expect("Could not read input file");

    if matches.occurrences_of("list-tokens") > 0 {
        println!("Lexing source code...");
        for maybe_token in lexer::Lexer::new(&source_code, &settings) {
            match maybe_token {
                Ok(token) => print!("{};  ", token),
                Err(e) => print!("({});  ", e),
            }
        }
        println!("\n")
    }

    println!("Parsing source code...");
    let parse_result = expression::Expression::compile_from_tokens(
        lexer::Lexer::new(&source_code, &settings),
        &settings,
    );
    match parse_result {
        Err(parse_error) => println!("Could not parse source code\n{}", parse_error),
        Ok(expr) => {
            console_executor::ConsoleExecutor::new_from_expr(&source_code, &expr, 100).run();
        }
    }
}
