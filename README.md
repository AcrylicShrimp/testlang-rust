# testlang-rust
A rust implemented compiler front-end project that uses LLVM as back-end.

## Purpose
This project was began for the complete implementation of the front-end of compilers.

## Structure
- `src/rule`: Parses a `rule` file and generates a [CLR parsing table](https://www.javatpoint.com/clr-1-parsing).
- `src/parser.rs`: Parses tokens from a lexer and generates an AST. This parser is [driven by a parsing table](https://www.tutorialspoint.com/compiler_design/compiler_design_bottom_up_parser.htm).
- `src/lexer.rs`: Supplies tokens to a parser by reading given input.
- `src/codegen`: Generates LLVM IR codes.

## Rule file
The `rule` crate requires a valid rule file to generate a parsing table. This rule file is similar to [the BNF notation](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form) except for that it is not supports alternations(`|` symbol).

### Basic form
It is super easy to define a new non-terminal. Don't forget to add an `@` sign as a first letter for each non-terminal.

```
non-terminal-name: terminal @non-terminal terminal @non-terminal ... ;
```

If you want use alternations, define it multiple times.

```
statement: @if-statement;
statement: @for-statement;
statement: @while-statement;
statement: @var-declare-statement semicolon;
...
```

Every terminals should be defined in `lexer.ts` too.

### Root non-terminal
Any valid rule files must define a root non-terminal `__root`(double underscore). This root non-terminal cannot be defined multiple times. After successful parsing, the parser may return an AST named `S` for below rules.

```
__root = @S;

S: ...
```
