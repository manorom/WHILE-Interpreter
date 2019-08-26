
#[derive(Clone)]
pub struct InterpreterSettings {
    //pub newline_after_do: bool,
    pub delimiter_after_keyword: bool,
    //pub allow_arbitrary_constant: bool,
    //allow_leading_zeros_in_ints: bool
}

impl Default for InterpreterSettings {
    fn default() -> InterpreterSettings {
        InterpreterSettings {
            //newline_after_do: false,
            delimiter_after_keyword: true,
            //allow_arbitrary_constant: true,
        }
    }
}