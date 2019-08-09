use std::convert::From;
use std::iter::Peekable;
use std::slice::Iter;

use environment::Environment;
use expression::{AssignExpr, Expression, LoopExpr, SequenceExpr, WhileExpr};
use tokenize::CodeLocation;

trait Evaluability {
    fn is_evaluable(&self) -> bool;
}

impl<'a> Evaluability for Expression<'a> {
    fn is_evaluable(&self) -> bool {
        match self {
            Expression::Sequence(..) => false,
            _ => true,
        }
    }
}

enum LoopPhase {
    LoopVarUninitialized,
    LoopVarInitialized,
}

enum ProgramStackElement<'a, 'b> {
    Loop(&'b LoopExpr<'a>, LoopPhase),
    While(&'b WhileExpr<'a>),
    Sequence(&'b SequenceExpr<'a>, Peekable<Iter<'b, Expression<'a>>>),
    Assign(&'b AssignExpr<'a>),
}

impl<'a, 'b> Evaluability for ProgramStackElement<'a, 'b> {
    fn is_evaluable(&self) -> bool {
        match self {
            ProgramStackElement::Sequence(..) => false,
            _ => true,
        }
    }
}

impl<'a, 'b> From<&'b Expression<'a>> for ProgramStackElement<'a, 'b> {
    fn from(expr: &'b Expression<'a>) -> Self {
        match expr {
            Expression::While(ref while_expr) => ProgramStackElement::While(while_expr),
            Expression::Loop(ref loop_expr) => {
                ProgramStackElement::Loop(loop_expr, LoopPhase::LoopVarUninitialized)
            }
            Expression::Sequence(ref seq_expr) => {
                ProgramStackElement::Sequence(seq_expr, seq_expr.body.as_slice().iter().peekable())
            }
            Expression::Assign(ref assign_expr) => ProgramStackElement::Assign(assign_expr),
        }
    }
}

pub struct ExpressionWalkerInterpreter<'a, 'b> {
    program_stack: Vec<ProgramStackElement<'a, 'b>>,
    environ: Environment,
}

impl<'a, 'b> ExpressionWalkerInterpreter<'a, 'b> {
    pub fn new(expr: &'b Expression<'a>) -> ExpressionWalkerInterpreter<'a, 'b> {
        let stack_elem = ProgramStackElement::from(expr);

        let program_stack = vec![stack_elem];

        let environ = Environment::new();

        let mut interp = ExpressionWalkerInterpreter {
            program_stack,
            environ,
        };

        if let ProgramStackElement::Sequence(..) = interp.program_stack.last().unwrap() {
            interp.move_to_next();
        }

        return interp;
    }

    fn push_to_stack(&mut self, stack_elem: ProgramStackElement<'a, 'b>) {
        match stack_elem {
            ProgramStackElement::Loop(_, _) => self.environ.increment_counter_register_level(0),
            ProgramStackElement::While(_) => self.environ.increment_comp_register_level(true),
            _ => (),
        }
        self.program_stack.push(stack_elem);
    }

    fn backtrack(&mut self) {
        if let Some(elem) = self.program_stack.last() {
            match elem {
                ProgramStackElement::Loop(_, _) => self.environ.decrement_counter_register_level(),
                ProgramStackElement::While(_) => self.environ.decrement_comp_register_level(),
                _ => (),
            }
        }

        self.program_stack.pop();
    }

    fn eval(&mut self) -> Option<()> {
        match self.program_stack.last_mut()? {
            ProgramStackElement::Loop(ref loop_expr, ref mut loop_phase) => {
                if let LoopPhase::LoopVarUninitialized = *loop_phase {
                    let val = self.environ.load_var(loop_expr.var_idx);
                    self.environ.set_current_counter_register(val as u32);
                    *loop_phase = LoopPhase::LoopVarInitialized;
                } else {
                    let old_loop_val = self.environ.get_current_counter_register();
                    self.environ.set_current_counter_register(old_loop_val - 1);
                }
            }
            ProgramStackElement::While(ref while_expr) => {
                let while_var_value = self.environ.load_var(while_expr.var_idx);
                if while_var_value != 0 {
                    self.environ.set_current_comp_register(true);
                } else {
                    self.environ.set_current_comp_register(false);
                }
            }
            ProgramStackElement::Sequence(_, _) => {
                panic!("Trying to evaluate a Sequence Expression");
            }
            ProgramStackElement::Assign(ref assign_expr) => {
                let source_var_value = self.environ.load_var(assign_expr.source_var_idx);
                let modifier = assign_expr.modifier;
                let target_var_value = source_var_value + modifier;
                self.environ
                    .store_var(assign_expr.target_var_idx, target_var_value);
            }
        };
        Some(())
    }

    fn move_to_next(&mut self) -> Option<()> {
        loop {
            let next_possible_expr: Option<&Expression> = match self.program_stack.last_mut()? {
                ProgramStackElement::Assign(_) => None,
                ProgramStackElement::Sequence(_, ref mut seq_iter) => {
                    if let Some(expr) = seq_iter.next() {
                        Some(expr)
                    } else {
                        None
                    }
                }
                ProgramStackElement::While(while_expr) => {
                    if self.environ.get_current_comp_register() {
                        Some(&*while_expr.body)
                    } else {
                        None
                    }
                }
                ProgramStackElement::Loop(loop_expr, _) => {
                    if self.environ.get_current_counter_register() != 0 {
                        Some(&*loop_expr.body)
                    } else {
                        None
                    }
                }
            };

            if let Some(next_expr) = next_possible_expr {
                let next_stack_element = ProgramStackElement::from(next_expr);
                if next_expr.is_evaluable() {
                    self.push_to_stack(next_stack_element);
                    return Some(());
                } else {
                    self.push_to_stack(next_stack_element);
                }
            } else {
                self.backtrack();

                if self.program_stack.last()?.is_evaluable() {
                    return Some(());
                }
            }
        }
    }

    pub fn step(&mut self) -> Option<()> {
        self.eval()?;
        self.move_to_next()?;
        Some(())
    }

    pub fn current_code_location(&self) -> Option<CodeLocation> {
        let ret = match self.program_stack.last()? {
            ProgramStackElement::While(while_expr) => while_expr.code_location(),
            ProgramStackElement::Loop(loop_expr, _) => loop_expr.code_location(),
            ProgramStackElement::Assign(assign_expr) => assign_expr.code_location(),
            _ => panic!("Sequence Expression does not have a source location"),
        };
        Some(ret)
    }
}
