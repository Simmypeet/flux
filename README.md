# Flux

Flux is an interpreter for a simple programming language unsurprisingly called Flux. The interpreter
allows user to run a Flux program written in a file and see the output.

- [The Language](#the-language)
- [High-level Overview](#high-level-overview)
  - [1. Lexical Analysis](#1-lexical-analysis)
  - [2. Syntactic Analysis](#2-syntactic-analysis)
  - [3. Lowering](#3-lowering)
  - [4. Interpretation](#4-interpretation)
- [Example](#example)
  - [1. Fibonacci](#1-fibonacci)
  - [2. Factorial](#2-factorial)

<div class="page">

## The Language

The language is a simple imperative language with a C-like syntax. The language supports the
following features:

- Variable declaration and assignment
- Arithmetic expressions
- Conditional statements (`if-else` and `while`)
- Functions
- Output
- Dynamically typed
- Has four primitive types: `int64`, `float64`, `bool`, and `null`

### Synopsis

The following is an example of a Flux program that computes the first 10 Fibonacci numbers:

``` javascript
function fibonacci(i) {
    if (i <= 1) {
        return i;
    } else {
        return fibonacci(i - 1) + fibonacci(i - 2);
    }
}

function main() {
    let i = 0;
    let b = 10;
    while (i < b) {
        print fibonacci(i);
        i += 1;
    }
}
```

<div class="page">

## High-level Overview

When the source code is provided to the interpreter, the input then goes throught the content
processing pipeline which consists of the following steps:

### 1. Lexical Analysis

After the input is read, it is passed to the `flux_lexical` crate. The crate is responsible for
splitting the input into *tokens*. *Tokens* can be thought of as a **word** in a programming
language, they are groups of characters that were categorized by certain properties. For example.
the following `let x = 5` statement would be split into the following tokens: `let`, `x`, `=`, `5`
with whitespaces separating them.

### 2. Syntactic Analysis

After the **tokens** are generated, they are passed to the `flux_syntax` crate. This phase is
responsible for generating an *syntax tree* from the a linear list/string of tokens. The *syntax
tree* is a tree-like structure that closely resembles the structure of the source code and is
used to represent the program in a more convenient way for the interpreter.

### 3. Lowering

After the **syntax tree** is generated, it is passed to the `flux_semantic` crate. The program
represented by the *Control Flow Graph* (CFG) which is a graph-like structure that represents the
control flow of the program.

### 4. Interpretation

After the **CFG** is generated, it is ready to be interpreted. The interpreter then goes through
the CFG and executes the instructions.

<div class="page">

## Example

All the examples Flux programs can be found in the `examples` directory. To run an example,
execute the following command:

```bash
cargo run --release -- example/<name>
```

### 1. Fibonacci

The file [examples/fibonacci.flux](examples/fibonacci.flux) contains a program that prints out first
ten Fibonacci numbers. This program demonstrates the several features of the language such as
recursive functions, conditional statements, and output.

### 2. Factorial

The file [examples/factorial.flux](examples/factorial.flux) contains a program that prints out
factorial of a number.
