# Compiler  [![Build Status](https://travis-ci.org/dmekhanikov/compiler.svg?branch=master)](https://travis-ci.org/dmekhanikov/compiler)

## Features
- `int` and bool types
- primitive operators: 
    - `+`, `-`, `*`, `/`, `%`
    - `==`, `!=`, `<`, `<=`, `>`, `>=`
    - `&&`, `||`
- `if` and `while` statements
- `printInt`, `printBool`, `readInt` and user-defined functions
- LLVM code generation

## Build
The project can be built with Maven:

    $ mvn package

## Example
```c
int power(int x, int y) {
    int result;
    result = 1;
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
