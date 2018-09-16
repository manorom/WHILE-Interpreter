mod expression;
mod tokenize;

static EXAMPLE: &'static str = "x1 := x1 + 1; x1:=x1+1;WHILE x1 != 0 DO x2 := x2 + 1; x1 := x1 - 1 END";

fn main() {
    let token_stream = tokenize::TokenStream::from_str(EXAMPLE);
//    let a: Vec<_> = token_stream.collect();
    let a = expression::Expression::compile_from_tokens(token_stream);
    match a {
        Ok(expr) => println!("{:?}", expr),
        Err(e) => println!("{}", e),
    }
}
