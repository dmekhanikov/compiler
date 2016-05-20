# Compiler  [![Build Status](https://travis-ci.org/dmekhanikov/compiler.svg?branch=master)](https://travis-ci.org/dmekhanikov/compiler)

## Features
- `int` and `bool` types
- primitive operators: 
    - `+`, `-`, `*`, `/`, `%`
    - `==`, `!=`, `<`, `<=`, `>`, `>=`
    - `&&`, `||`
- conditional expressions
- `while` statements
- `printInt`, `printBool`, `readInt` and user-defined functions
- functions overloading
- structures with fields and methods
    - private and public members
    - constructors
    - inheritance
- tail recursion elimination
    - implicit tail calls
    - integer recursive computations
- LLVM code generation

## Build
The project can be built with Maven:

    $ mvn package

## Examples
Fast power computation:
```c
int power(int x, int y) {
    int result = 1;
    while (x > 0) {
        r = if (y % 2 == 1) {
                r * x
            } else { r };
        x = x * x;
        y = y / 2;
    }
    result
}
```
Calculation of a sum of numbers in a range:
```c
int sum(int a, int b) {
    if (a < b) {
        b + sum(a, b - 1)
    } else {
        0
    }
}
```
Structure of a rectangle:
```c
struct Rectangle {
    private int w, h;

    constructor(int w, int h) {
        this.w = w;
        this.h = h;
    }

    int square() {
        this.w * this.h
    }

    bool equals(Rectangle other) {
        this.w == other.w &&
        this.h == other.h
    }
}
```
