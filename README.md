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
