use std::collections::HashMap;

pub type SymbolScope = &'static str;

pub const GLOBAL_SCOPE: SymbolScope = "GLOBAL";

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }
    pub fn define(&mut self, name: String) -> &Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            index: self.num_definitions,
            scope: GLOBAL_SCOPE,
        };
        self.store.insert(name.to_string(), symbol);
        self.num_definitions += 1;
        &self.store[&name]
    }
    pub fn resolve(&self, name: &String) -> Option<&Symbol> {
        self.store.get(name)
    }
}
