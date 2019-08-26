
#[derive(Clone)]
pub struct InterpreterSettings {
    // Lexer options
    pub delimiter_after_keyword: bool,
    //pub newline_after_do: bool,

    //pub allow_arbitrary_constant: bool,
    //allow_leading_zeros_in_ints: bool
}

impl Default for InterpreterSettings {
    fn default() -> InterpreterSettings {
        InterpreterSettings {
            delimiter_after_keyword: true,
            //newline_after_do: false,
            //allow_arbitrary_constant: true,
        }
    }
}