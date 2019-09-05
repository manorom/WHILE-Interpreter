use expression;
use walker_interpreter;

pub struct ConsoleExecutor<'a, 'b> {
    interp: walker_interpreter::ExpressionWalkerInterpreter<'a, 'b>,
    source_code_lines: Vec<&'a str>,
    num_steps: u32,
    max_num_steps: u32,
}

impl<'a, 'b> ConsoleExecutor<'a, 'b> {
    pub fn new_from_expr(
        source_code: &'a str,
        expr: &'b expression::Expression<'a>,
        max_num_steps: u32,
    ) -> ConsoleExecutor<'a, 'b> {
        let source_code_lines = source_code.lines().collect();
        let interp = walker_interpreter::ExpressionWalkerInterpreter::new(expr);

        ConsoleExecutor {
            interp,
            source_code_lines,
            num_steps: 1,
            max_num_steps,
        }
    }

    pub fn run(&mut self) {
        self.display_current_line();
        while self.interp.step().is_some() {
            if self.num_steps >= self.max_num_steps {
                println!("Aborted after {} steps", self.num_steps);
                return;
            }
            self.display_current_line();
            self.num_steps += 1;
        }
        self.print_environemnt();
    }

    pub fn display_current_line(&self) {
        let code_location = self.interp.current_code_location().unwrap();
        let line_string = code_location.line.to_string();
        println!(
            "(L {}) {}",
            line_string,
            self.source_code_lines[code_location.line - 1]
        );
        println!(
            "      {: <1$}^\n",
            "",
            line_string.len() + code_location.col - 2
        );
    }

    pub fn print_environemnt(&self) {
        print!("[  ");
        for (index, value) in self.interp.environ.vars_iter() {
            print!("x{}={}  ", index, value);
        }
        println!("]");
    }
}
