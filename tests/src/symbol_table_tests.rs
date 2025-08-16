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
                name: "e".to_string(),
                scope: LOCAL_SCOPE,
                index: 0,
            },
        ),
        (
            "f".to_string(),
            Symbol {
                name: "f".to_string(),
                scope: LOCAL_SCOPE,
                index: 1,
            },
        ),
    ]);
    let mut global = SymbolTable::new();
    let a = global.define("a");
    assert_eq!(a, &expected["a"]);
    let b = global.define("b");
    assert_eq!(b, &expected["b"]);

    let mut first_local = SymbolTable::new_enclosed(global);
    let c = first_local.define("c");
    assert_eq!(c, &expected["c"]);
    let d = first_local.define("d");
    assert_eq!(d, &expected["d"]);

    let mut second_local = SymbolTable::new_enclosed(first_local);
    let e = second_local.define("e");
    assert_eq!(e, &expected["e"]);
    let f = second_local.define("f");
    assert_eq!(f, &expected["f"]);
}

#[test]
fn test_resolve() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

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
    global.define("a");
    global.define("b");

    let mut local = SymbolTable::new_enclosed(global);
    local.define("c");
    local.define("d");

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
    global.define("a");
    global.define("b");

    let mut first_local = SymbolTable::new_enclosed(global);
    first_local.define("c");
    first_local.define("d");

    let mut second_local = SymbolTable::new_enclosed(first_local.clone());
    second_local.define("e");
    second_local.define("f");

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
                        name: "e".to_string(),
                        scope: LOCAL_SCOPE,
                        index: 0,
                    },
                ),
                (
                    "f".to_string(),
                    Symbol {
                        name: "f".to_string(),
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

#[test]
fn test_define_resolve_builtins() {
    let expected = vec![
        Symbol {
            name: "a".to_string(),
            scope: compiler::symbol_table::BUILTIN_SCOPE,
            index: 0,
        },
        Symbol {
            name: "c".to_string(),
            scope: compiler::symbol_table::BUILTIN_SCOPE,
            index: 1,
        },
        Symbol {
            name: "e".to_string(),
            scope: compiler::symbol_table::BUILTIN_SCOPE,
            index: 2,
        },
        Symbol {
            name: "f".to_string(),
            scope: compiler::symbol_table::BUILTIN_SCOPE,
            index: 3,
        },
    ];

    let mut global = SymbolTable::new();
    for (i, v) in expected.iter().enumerate() {
        global.define_builtin(i, &v.name);
    }

    let first_local = SymbolTable::new_enclosed(global.clone());
    let second_local = SymbolTable::new_enclosed(first_local.clone());

    for table in [&global, &first_local, &second_local] {
        for sym in &expected {
            let result = table.resolve(&sym.name);
            if result.is_none() {
                panic!("name {} not resolvable", sym.name);
            }
            if result.unwrap() != sym {
                panic!(
                    "expected {} to resolve to {:?}, got={:?}",
                    sym.name,
                    sym,
                    result.unwrap()
                );
            }
        }
    }
}
