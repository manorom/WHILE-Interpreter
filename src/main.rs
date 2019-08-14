mod expression;
mod tokenize;
mod environment;
mod walker_interpreter;
mod console_executor;

static EXAMPLE: &'static str =
"x1 := x1 + 1;
 x1 := x1 + 1;
 WHILE x1 != 0 DO 
    x2:=x2+1;
    x1:=x1-1 
 END;
 x1 := x1 - 1";

fn main() {
    println!("Source Code:\n{}\n\n", EXAMPLE);
    let expr = expression::Expression::compile_from_tokens(tokenize::TokenStream::from_str(EXAMPLE)).unwrap();
    let mut executor = console_executor::ConsoleExecutor::new_from_string(EXAMPLE, &expr, 1000);
    executor.run();
}
