# Compiler  [![Build Status](https://travis-ci.org/dmekhanikov/compiler.svg?branch=master)](https://travis-ci.org/dmekhanikov/compiler)

## Features
- `int` and `bool` types
- primitive operators: 
    - `+`, `-`, `*`, `/`, `%`
    - `==`, `!=`, `<`, `<=`, `>`, `>=`
    - `&&`, `||`
- `if` and `while` statements
- `printInt`, `printBool`, `readInt` and user-defined functions
- structures with fields and methods
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
        if (y % 2 == 1) {
            r = r * x;
        } else {}
        x = x * x;
        y = y / 2;
    }
    return result;
}
```
Structure of a rectangle:
```c
struct Rectangle {
    int w, h;

    int square() {
        return this.w * this.h;
    }

    bool equals(Rectangle other) {
        return this.w == other.w && 
                this.h == other.h;
    }
}
```
