use std::collections::HashMap;

pub type SymbolScope = &'static str;

pub const GLOBAL_SCOPE: SymbolScope = "GLOBAL";
pub const LOCAL_SCOPE: SymbolScope = "LOCAL";
pub const BUILTIN_SCOPE: SymbolScope = "BUILTIN";
pub const FREE_SCOPE: SymbolScope = "FREE";

#[derive(Clone, Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    pub outer: Option<Box<SymbolTable>>,
    pub store: HashMap<String, Symbol>,
    pub num_definitions: usize,
    pub free_symbols: Vec<Symbol>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
        }
    }

    pub fn new_enclosed(outer: SymbolTable) -> Self {
        SymbolTable {
            outer: Some(Box::new(outer)),
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
        }
    }

    pub fn define(&mut self, name: &str) -> &Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            index: self.num_definitions,
            scope: if self.outer.is_some() {
                LOCAL_SCOPE
            } else {
                GLOBAL_SCOPE
            },
        };
        self.store.insert(name.to_string(), symbol);
        self.num_definitions += 1;
        &self.store[name]
    }

    pub fn define_builtin(&mut self, index: usize, name: &str) -> &Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            index,
            scope: BUILTIN_SCOPE,
        };
        self.store.insert(name.to_string(), symbol);
        &self.store[name]
    }

    pub fn define_free(&mut self, original: Symbol) -> &Symbol {
        let name = &original.name.clone();
        self.free_symbols.push(original);
        let symbol = Symbol {
            name: name.clone(),
            index: self.free_symbols.len() - 1,
            scope: FREE_SCOPE,
        };
        self.store.insert(name.clone(), symbol);
        &self.store[name]
    }

    pub fn resolve(&mut self, name: &String) -> Option<Symbol> {
        if let Some(symbol) = self.store.get(name) {
            return Some(symbol.clone());
        }

        if let Some(outer) = &mut self.outer {
            if let Some(outer_symbol) = outer.resolve(name) {
                if outer_symbol.scope == GLOBAL_SCOPE || outer_symbol.scope == BUILTIN_SCOPE {
                    return Some(outer_symbol);
                }

                let free_symbol = self.define_free(outer_symbol);
                return Some(free_symbol.clone());
            }
        }
        None
    }
}
