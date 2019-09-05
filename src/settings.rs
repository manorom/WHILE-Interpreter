#[derive(Clone)]
pub struct Settings {
    // Lexer options
    pub delimiter_after_keyword: bool,

    // Parser options
    pub allow_arbitrary_constants_while: bool,
    pub allow_arbitrary_constants_assign: bool,
    //pub end_implies_semicolon: bool,
    //pub allow_unncecessary_semicolons: bool
}

impl Default for Settings {
    fn default() -> Settings {
        Settings {
            delimiter_after_keyword: true,
            allow_arbitrary_constants_while: false,
            allow_arbitrary_constants_assign: false,
            //end_implies_semicolon: false,
            //allow_unncessary_semicolons: false,
        }
    }
}
