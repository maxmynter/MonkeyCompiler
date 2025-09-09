#![cfg(test)]
use compiler::symbol_table::{FREE_SCOPE, GLOBAL_SCOPE, LOCAL_SCOPE, Symbol, SymbolTable};
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
        assert_eq!(result, *sym.1);
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
        assert_eq!(result, *sym.1);
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

    let mut tests = vec![
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

    for tt in tests.iter_mut() {
        for sym in tt.expected_symbols.iter() {
            let result = tt.table.resolve(sym.0).unwrap();
            assert_eq!(result, *sym.1);
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

    let mut first_local = SymbolTable::new_enclosed(global.clone());
    let mut second_local = SymbolTable::new_enclosed(first_local.clone());

    for table in [&mut global, &mut first_local, &mut second_local] {
        for sym in &expected {
            let result = table.resolve(&sym.name);
            match result {
                None => panic!("name {} not resolvable", sym.name),
                Some(resolved_sym) => {
                    if resolved_sym != *sym {
                        panic!(
                            "expected {} to resolve to {:?}, got={:?}",
                            sym.name,
                            sym,
                            resolved_sym
                        );
                    }
                }
            }
        }
    }
}

#[test]
fn test_resolve_unresolvable_free() {
    let mut global = SymbolTable::new();
    global.define("a");

    let mut first_local = SymbolTable::new_enclosed(global);
    first_local.define("c");

    let mut second_local = SymbolTable::new_enclosed(first_local);
    second_local.define("e");
    second_local.define("f");

    let expected = vec![
        Symbol {
            name: "a".to_string(),
            scope: GLOBAL_SCOPE,
            index: 0,
        },
        Symbol {
            name: "c".to_string(),
            scope: FREE_SCOPE,
            index: 0,
        },
        Symbol {
            name: "e".to_string(),
            scope: LOCAL_SCOPE,
            index: 0,
        },
        Symbol {
            name: "f".to_string(),
            scope: LOCAL_SCOPE,
            index: 1,
        },
    ];

    for sym in expected {
        let result = second_local.resolve(&sym.name);
        match result {
            None => panic!("name {} not resolvable", sym.name),
            Some(resolved_sym) => {
                if resolved_sym != sym {
                    panic!(
                        "expected {} to resolve to {:?}, got={:?}",
                        sym.name,
                        sym,
                        resolved_sym
                    );
                }
            }
        }
    }

    let expected_unresolvable = vec!["b", "d"];
    for name in expected_unresolvable {
        let result = second_local.resolve(&name.to_string());
        if result.is_some() {
            panic!("name {} resolved, but was expected not to", name);
        }
    }
}

#[test]
fn test_resolve_free() {
    let mut global = SymbolTable::new();
    global.define("a");
    global.define("b");

    let mut first_local = SymbolTable::new_enclosed(global);
    first_local.define("c");
    first_local.define("d");

    let mut second_local = SymbolTable::new_enclosed(first_local.clone());
    second_local.define("e");
    second_local.define("f");

    struct TestCase {
        table: SymbolTable,
        expected_symbols: Vec<Symbol>,
        expected_free_symbols: Vec<Symbol>,
    }

    let tests = vec![
        TestCase {
            table: first_local,
            expected_symbols: vec![
                Symbol {
                    name: "a".to_string(),
                    scope: GLOBAL_SCOPE,
                    index: 0,
                },
                Symbol {
                    name: "b".to_string(),
                    scope: GLOBAL_SCOPE,
                    index: 1,
                },
                Symbol {
                    name: "c".to_string(),
                    scope: LOCAL_SCOPE,
                    index: 0,
                },
                Symbol {
                    name: "d".to_string(),
                    scope: LOCAL_SCOPE,
                    index: 1,
                },
            ],
            expected_free_symbols: vec![],
        },
        TestCase {
            table: second_local,
            expected_symbols: vec![
                Symbol {
                    name: "a".to_string(),
                    scope: GLOBAL_SCOPE,
                    index: 0,
                },
                Symbol {
                    name: "b".to_string(),
                    scope: GLOBAL_SCOPE,
                    index: 1,
                },
                Symbol {
                    name: "c".to_string(),
                    scope: FREE_SCOPE,
                    index: 0,
                },
                Symbol {
                    name: "d".to_string(),
                    scope: FREE_SCOPE,
                    index: 1,
                },
                Symbol {
                    name: "e".to_string(),
                    scope: LOCAL_SCOPE,
                    index: 0,
                },
                Symbol {
                    name: "f".to_string(),
                    scope: LOCAL_SCOPE,
                    index: 1,
                },
            ],
            expected_free_symbols: vec![
                Symbol {
                    name: "c".to_string(),
                    scope: LOCAL_SCOPE,
                    index: 0,
                },
                Symbol {
                    name: "d".to_string(),
                    scope: LOCAL_SCOPE,
                    index: 1,
                },
            ],
        },
    ];

    for mut tt in tests {
        for sym in tt.expected_symbols {
            let result = tt.table.resolve(&sym.name);
            match result {
                None => panic!("name {} not resolvable", sym.name),
                Some(resolved_sym) => {
                    if resolved_sym != sym {
                        panic!(
                            "expected {} to resolve to {:?}, got={:?}",
                            sym.name,
                            sym,
                            resolved_sym
                        );
                    }
                }
            }
        }

        if tt.table.free_symbols.len() != tt.expected_free_symbols.len() {
            panic!(
                "wrong number of free symbols. got={}, want={}",
                tt.table.free_symbols.len(),
                tt.expected_free_symbols.len()
            );
        }

        for (i, sym) in tt.expected_free_symbols.iter().enumerate() {
            let result = &tt.table.free_symbols[i];
            if result != sym {
                panic!("wrong free symbol. got={:?}, want={:?}", result, sym);
            }
        }
    }
}
