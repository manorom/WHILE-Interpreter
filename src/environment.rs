use std::collections::hash_map::Iter;
use std::collections::HashMap;

pub struct Environment {
    pub variables: HashMap<u32, i32>,
    comp_registers: Vec<bool>,
    counter_registers: Vec<u32>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            variables: HashMap::new(),
            comp_registers: Vec::new(),
            counter_registers: Vec::new(),
        }
    }

    pub fn decrement_counter_register_level(&mut self) {
        self.counter_registers.pop().unwrap();
    }

    pub fn increment_counter_register_level(&mut self, value: u32) {
        self.counter_registers.push(value);
    }

    pub fn counter_register_level(&self) -> usize {
        self.counter_registers.len()
    }

    pub fn get_current_counter_register(&self) -> u32 {
        *self.counter_registers.last().unwrap()
    }

    pub fn set_current_counter_register(&mut self, value: u32) {
        *self.counter_registers.last_mut().unwrap() = value;
    }

    pub fn increment_comp_register_level(&mut self, value: bool) {
        self.comp_registers.push(value);
    }

    pub fn decrement_comp_register_level(&mut self) {
        self.comp_registers.pop().unwrap();
    }

    pub fn set_current_comp_register(&mut self, value: bool) {
        *self.comp_registers.last_mut().unwrap() = value;
    }

    pub fn get_current_comp_register(&self) -> bool {
        *self.comp_registers.last().unwrap()
    }

    pub fn load_var(&mut self, i: u32) -> i32 {
        *self.variables.entry(i).or_insert(0)
    }

    pub fn store_var(&mut self, i: u32, val: i32) {
        self.variables.insert(i, val);
    }

    pub fn vars_iter(&self) -> Iter<u32, i32> {
        self.variables.iter()
    }
}
