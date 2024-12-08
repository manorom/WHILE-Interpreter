use core::slice;
use std::collections::HashMap;
use crate::ast;

struct LoopScope<'a> {
    node: &'a ast::Loop,
    loop_var: i32,
}

enum Scope<'a> {
    Sequence(&'a ast::Node, slice::Iter<'a, ast::Node>),
    While(&'a ast::While),
    Loop(LoopScope<'a>),
    Assign(&'a ast::Assign),
}

impl<'a> Scope<'a> {
    fn from_ast(node: &'a ast::Node, vars: &HashMap<u32, i32>) -> Self {
        match node {
            ast::Node::Sequence(vec) => Scope::Sequence(node, vec.iter()),
            ast::Node::Assign(assign) => Scope::Assign(assign),
            ast::Node::While(w) => Scope::While(w),
            ast::Node::Loop(node) => Scope::Loop(LoopScope {
                node,
                loop_var: *vars.get(&node.var_idx).unwrap_or(&0),
            }),
        }
    }
}

pub struct Interpreter<'a> {
    vars: HashMap<u32, i32>,
    stack: Vec<Scope<'a>>,
    num_steps: u32
}

impl<'a> Interpreter<'a> {
    pub fn new(ast: &'a ast::Node) -> Self {
        let vars = HashMap::new();
        let scope = Scope::from_ast(ast, &vars);
        let stack = vec![scope];
        Interpreter {
            vars,
            stack,
            num_steps: 0
        }
    }
    fn execute_assign(&mut self, assign: &'a ast::Assign) {
        let source_val = *self.vars.get(&assign.source_var_idx).unwrap_or(&0);
        self.vars
            .insert(assign.target_var_idx, source_val + assign.modifier);
    }

    fn execute_while(&mut self, while_: &'a ast::While) -> bool {
        if self.vars.get(&while_.var_idx).copied().unwrap_or(0) != while_.comparator {
            true
        } else {
            false
        }
    }

    fn execute_loop(&mut self, loop_scope: &mut LoopScope<'a>) -> bool {
        let exec_body = loop_scope.loop_var > 0;
        loop_scope.loop_var -= 1;
        exec_body
    }

    pub fn current_line(&self) -> Option<usize> {
        Some(match self.stack.last()? {
            Scope::Sequence(seq, _) => seq.location().line,
            Scope::While(w) => w.location.line,
            Scope::Loop(scope) => scope.node.location.line,
            Scope::Assign(a) => a.location.line,
        })
    }

    pub fn step(&mut self) -> bool {
        if let Some(scope) = self.stack.pop() {
            match scope {
                Scope::Sequence(_, iter) => {
                    for scope in iter.rev() {
                        self.stack.push(Scope::from_ast(scope, &self.vars));
                    }
                    self.step()
                }
                Scope::While(ref node) => {
                    let enter = self.execute_while(node);
                    if enter {
                        self.stack.push(Scope::While(node));
                        self.stack
                            .push(Scope::from_ast(node.body_ref(), &self.vars));
                    }
                    true
                }
                Scope::Loop(mut loop_scope) => {
                    let enter = self.execute_loop(&mut loop_scope);
                    if enter {
                        let body = loop_scope.node.body_ref();
                        let next = Scope::from_ast(body, &self.vars);
                        self.stack.push(Scope::Loop(loop_scope));
                        self.stack.push(next);
                    }
                    true
                }
                Scope::Assign(assign) => {
                    self.execute_assign(assign);
                    true
                }
            }
        } else {
            false
        }
    }
}