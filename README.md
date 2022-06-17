# proto

A language compiler exploring as many parts of compiler theory as we (the writers) can muster

<br>

## Comments

---

Proto supports single line and multi line comments:

```rs
// this is a single line comment

/*
And this
is a
multi line comment
*/
```

<br>

## Basic types

---

```rs
i64: 1, 2, 3, 4
str: "some string"
char: 'a', 'e', 'i', 'o', 'u'
bool: true, false
unit
```

The **unit** type is the return type of statements (like variable definitions) and functions that return no values (procedures). It cannot be assigned to a variable.

<br>

## Complex types (and their type annotations)

---

- Arrays are type annotated as `[T]`, where `T` is the type of the contained elements. For example:

  ```rs
  [1, 2, 3, 4, 5] is inferred to be or annotated [i64]
  ```

- Hashmaps are written as `{K: V}`, where `K` is the type of the keys and `V` is the type of the values. For example:

  ```rs
  {'a': true, 'b': false, 'c': false} is inferred to be or annotated {char: bool}
  ```

- Tuples are the only heterogenous complex type (i.e they allow the mixing of different types). Once created, new elements cannot be added to extend a tuple. They are type annotated `(T`<sub>`1`</sub>, `T`<sub>`2`</sub>, `...`, `T`<sub>`n`</sub>`)` where each `T` from `1` to `n` can be a different type. For example:
  ```rs
  (1, 'c', "some string", false) is inferred to be or annotated (i64, char, str, bool)
  ```

<br>

## Variables (Mutable and Immutable)

---

Proto allows definition of both mutable and immutable variables using declaration statements. Using `let` to bind a variable makes it immutable for its lifetime in the program.

```rs
let immutable: i64 = 5;
immutable = 7; // this will fail at compile time
```

To allow mutation of a variable, use `mut` instead of `let`:

```rs
mut mutable: i64 = 5;
mutable = 7; // successful
```

When a variable is created, the type annotation can be skipped over if it is initialized (as the type can be inferred). When it is not initialized, it is an error to not type annotate it:

```rs
let init = 3; // init is inferred to be `i64`

mut stuff; // this will cause a compile time error
```

_Note_: In the future, we can infer the type of a variable without annotation or initialization based on the first value they are assigned.

In every scope, there is a variable named `'_'` which allows you to discard values you don't have need for:

```rs
let a = 5;
let b = 10;
_ = 15 - (a + b); // this value is discarded
```

## Code blocks

---

Lines of code can be grouped together using code blocks. These blocks come with their own scope.

```rs
{                    // ++++++++ scope a
    let a = 5;       //                +
                     //                +
    {                // .... scope b   +
        mut b = 6;   //            .   +
        b = b + a;   //            .   +
    }                // ............   +
                     //                +
}                    // ++++++++++++++++
```

Since a new scope is created for each code block, shadowing variables (defining a variable with a name existing in an outer scope) can occur. At all times, the nearest variable will be referenced:

```rs
{                          // ++++++++++ scope a
    let a: i64 = 5;        //                  +
                           //                  +
    {                      // ...... scope b   +
        let a: char = 'e'; //              .   +
        mut b = a + 1;     // type error!  .   +
    }                      // ..............   +
                           //                  +
}                          // ++++++++++++++++++
```

Trying to reference the outer a, which is an `i64` in a scope where a is a `char` leads to a type error in the example above.

Blocks also return values. They return the value of evaluating the last line of the block. If the final line is a statement or results in the `unit` type, then it returns `unit`, else it returns the value of the terminating expression.

```rs
{
    let a = 5;
    a * 5
} // returns 25 which is an i64 value

{
    let b = "12345abcde";
} // returns unit since variable declarations are statements
```

Terminating expressions can also be turned into statements by terminating them with a `;`.

```rs
{
    let a = 5
    a * 5;
} // returns unit, since i64 expression was terminated with ;
```

## Control flow

Pieces of code can be conditionally executed using `if ... else` expressions.

```rs
let a = 3;
let b = 2;

if a > b {
    println("a is greater"); // block returns unit
} else {
    println("b is greater"); // block returns unit
}
```

Since the `if ... else` construct is an expression, it can be used in variable definitions (provided the branch blocks don't return unit like they do in the above example).

```rs
let a = 3;
let b = 2;

let c = if a > b {
    a * a // returns i64
} else {
    b * b // returns i64
};
```

The variable `c` in the above example is inferred to be i64 since that is the type of the `if ... else` expression.

The `else` part of the construct allows further `if` expressions to check more conditional cases:

```rs
let a = 3;
let b = 2;

let c = if a > b {
    "a is greater" // returns str
} else if b > a{
    "b is greater" // returns str
} else {
    "they are equal" // returns str
};
```
