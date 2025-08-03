#![cfg(test)]
use compiler::symbol_table::{GLOBAL_SCOPE, LOCAL_SCOPE, Symbol, SymbolTable};
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
        (
            "c".to_string(),
            Symbol {
                name: "c".to_string(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
        ),
        (
            "d".to_string(),
            Symbol {
                name: "d".to_string(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
        ),
        (
            "e".to_string(),
            Symbol {
                name: "c".to_string(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
        ),
        (
            "f".to_string(),
            Symbol {
                name: "d".to_string(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
        ),
    ]);
    let mut global = SymbolTable::new();
    let a = global.define("a".to_string());
    assert_eq!(a, &expected["a"]);
    let b = global.define("b".to_string());
    assert_eq!(b, &expected["b"]);

    let mut first_local = SymbolTable::new_enclosed(&global);
    let c = first_local.define("c".to_string());
    assert_eq!(c, &expected["c"]);
    let d = first_local.define("d".to_string());
    assert_eq!(d, &expected["d"]);

    let mut second_local = SymbolTable::new_enclosed(&first_local);
    let e = second_local.define("d".to_string());
    assert_eq!(e, &expected["d"]);
    let f = second_local.define("e".to_string());
    assert_eq!(f, &expected["f"]);
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

    for sym in expected.iter() {
        let result = global.resolve(sym.0).unwrap();
        assert_eq!(result, sym.1);
    }
}

#[test]
fn test_resolve_local() {
    let mut global = SymbolTable::new();
    global.define("a".to_string());
    global.define("b".to_string());

    let mut local = SymbolTable::new_enclosed(&global);
    local.define("c".to_string());
    local.define("d".to_string());

    let expected = HashMap::from([
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
        (
            "c".to_string(),
            Symbol {
                name: "c".to_string(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
        ),
        (
            "d".to_string(),
            Symbol {
                name: "d".to_string(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
        ),
    ]);

    for sym in expected.iter() {
        let result = local.resolve(sym.0).unwrap();
        assert_eq!(result, sym.1);
    }
}

#[test]
fn test_resolve_nested_local() {
    struct SymbolTableTest {
        table: SymbolTable,
        expected_symbols: HashMap<String, Symbol>,
    }
    let mut global = SymbolTable::new();
    global.define("a".to_string());
    global.define("b".to_string());

    let mut first_local = SymbolTable::new_enclosed(&global);
    first_local.define("c".to_string());
    first_local.define("d".to_string());

    let mut second_local = SymbolTable::new_enclosed(&first_local);
    second_local.define("e".to_string());
    second_local.define("f".to_string());

    let tests = vec![
        SymbolTableTest {
            table: first_local,
            expected_symbols: HashMap::from([
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
                (
                    "c".to_string(),
                    Symbol {
                        name: "c".to_string(),
                        scope: LOCAL_SCOPE,
                        index: 0,
                    },
                ),
                (
                    "d".to_string(),
                    Symbol {
                        name: "d".to_string(),
                        scope: LOCAL_SCOPE,
                        index: 1,
                    },
                ),
            ]),
        },
        SymbolTableTest {
            table: second_local,
            expected_symbols: HashMap::from([
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
                (
                    "e".to_string(),
                    Symbol {
                        name: "c".to_string(),
                        scope: LOCAL_SCOPE,
                        index: 0,
                    },
                ),
                (
                    "f".to_string(),
                    Symbol {
                        name: "d".to_string(),
                        scope: LOCAL_SCOPE,
                        index: 1,
                    },
                ),
            ]),
        },
    ];

    for tt in tests.iter() {
        for sym in tt.expected_symbols.iter() {
            let result = tt.table.resolve(sym.0).unwrap();
            assert_eq!(result, sym.1);
        }
    }
}
