use std::collections::HashMap;

pub type SymbolScope = String;

pub struct Symbol {
    name: String,
    scope: SymbolScope,
    index: usize,
}

pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    fn new() -> Self {
        SymbolTable {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }
}
