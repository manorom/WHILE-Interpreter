mod expression;
mod tokenize;
mod environment;
mod walker_interpreter;

static EXAMPLE: &'static str =
"x1:=x1+1;x1:=x1+1;WHILE x1!=0 DO x2:=x2+1; x1:=x1-1 END x1 := x1 - 1";

fn main() {
    let token_stream = tokenize::TokenStream::from_str(EXAMPLE);
    //    let a: Vec<_> = token_stream.collect();
    for t in token_stream.clone() {
        if let Ok(token) = t {
            println!("{}", token);
        } else {
            println!("Tokenize error")
        }
    }
    let a = expression::Expression::compile_from_tokens(token_stream);
    match a {
        Ok(expr) => {
            println!("{}", expr);
            let mut interpreter = walker_interpreter::ExpressionWalkerInterpreter::new(&expr);
            while interpreter.step().is_some() {
                println!("{:?}", interpreter.current_code_location());
            }

        },
        Err(e) => println!("{}", e),
    }
}
