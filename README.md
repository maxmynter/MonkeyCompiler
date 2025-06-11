This is a Rust implementation of a [Monkey](https://monkeylang.org/) interpreter. It uses a recursive descent parser with top down operator precedence (Pratt Parser).

After cloning the repo, you can open a REPL with 
```bash
cargo run
```
in the root directory.

Run tests with 
```bash
cargo test
```
Monkey supports Integers, Booleans, Strings, Arrays, Hashmaps, and functions. 
Further, there are the following builtin functions:

```
puts(<arg1>, <arg2>, ...): void   -- print arguments to stdout
len(<arg>): Integer               -- length of argument
first(<arg>): any                 -- First element of array
last(<arg>): any                  -- Last element of array
rest(<arg>): Array                -- All but first element of array
push(<arg1>, <arg2>): Array       -- Append element to array
```



The Monkey language was designed by Thorsten Ball for the books [Building an Interpreter in Go](https://interpreterbook.com/) and [Building a Compiler in Go](https://compilerbook.com/) which I used as reference.
