#![cfg(test)]
use compiler::symbol_table::{GLOBAL_SCOPE, Symbol, SymbolTable};
use std::collections::HashMap;

#[test]
fn test_define() {
    let expected: HashMap<String, Symbol> = HashMap::from([
        (
            "a".to_string(),
            Symbol {
                name: "a".to_string(),
                scope: GLOBAL_SCOPE,
                index: 0,
            },
        ),
        (
            "b".to_string(),
            Symbol {
                name: "b".to_string(),
                scope: GLOBAL_SCOPE,
                index: 1,
            },
        ),
    ]);
    let mut global = SymbolTable::new();

    let a = global.define("a".to_string());
    assert_eq!(*a, expected["a"]);

    let b = global.define("b".to_string());
    assert_eq!(*b, expected["b"]);
}

#[test]
fn test_resolve() {
    let mut global = SymbolTable::new();
    global.define("a".to_string());
    global.define("b".to_string());

    let expected: HashMap<String, Symbol> = HashMap::from([
        (
            "a".to_string(),
            Symbol {
                name: "a".to_string(),
                scope: GLOBAL_SCOPE,
                index: 0,
            },
        ),
        (
            "b".to_string(),
            Symbol {
                name: "b".to_string(),
                scope: GLOBAL_SCOPE,
                index: 1,
            },
        ),
    ]);

    for (_, sym) in expected.iter().enumerate() {
        let result = global.resolve(sym.0).unwrap();
        assert_eq!(result, sym.1);
    }
}
