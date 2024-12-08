mod ast;
mod interpreter;
mod lexer;
mod settings;
mod token;

use clap::Arg;
use interpreter::Interpreter;
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

    let my_lexer = lexer::Lexer::new(&source_code, &settings);

    let lines = source_code.lines().collect::<Vec<&str>>();

    println!("Parsing source code...");

    let my_ast = match ast::parse_program(my_lexer, settings.clone()) {
        Ok(my_ast) => my_ast,
        Err(e) => {
            eprintln!("Could not parse source code: {}", e);
            std::process::exit(1);
        }
    };

    my_ast.print();
    println!("");

    let mut interpreter = Interpreter::new(&my_ast);

    loop {
        if let Some(cur_line) = interpreter.current_line() {
            println!("(L{cur_line}) {}", lines[cur_line -1 ]);
        }
        let stepped = interpreter.step();
        if !stepped {
            break;
        }
    }

}
