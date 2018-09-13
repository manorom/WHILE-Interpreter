mod tokenize;

fn main() {
    let a: std::vec::Vec<Result<tokenize::Token, tokenize::TokenizeError>> = tokenize::TokenStream::from_str("WHILE DO abc !zdf!=x1").collect();
    println!("{:?}", a);
}
