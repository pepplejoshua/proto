# proto

A language compiler exploring as many parts of compiler theory as we (the writers) can muster

<br>

## Basic types

---

- `i64`
- `str`
- `char`
- `bool`
- `unit`, which is the type returned by statements and functions with no return type.

<br>

## Complex types

---

- Arrays, type annotated as `[T]`, where `T` is the type. For example, `[i64]` is the type of the array `[1, 2, 3, 4, 5]`.
- Hashmaps, written as `{K: V}`, where `K` is the type of the keys and `V` is the type of the values For example,`{char: bool}` is the type of the hashmap `{'a': true, 'b': false, 'c': false}`.
- Sets. written as `{I}`, where `I` is the type of the items to be contained in the set. For example. `{str}` is the type of the set `{"proto", "is", "a", "fun", "language"}`.
- Tuples, which are the only heterogenous complex type (i.e they allow the mixing of different types). They are type annotated (`T`<sub>`1`</sub>, `T`<sub>`2`</sub>, ..., `T`<sub>`n`</sub>) where each `T` from `1` to `n` can be a different type. For example. the tuple (`1`, `'c'`, `"some string"`, `false`) will be inferred to be typed (`i64`, `char`, `str`, `bool`).

<br>

## Variables (Mutability and Immutability)

---

Proto allows both mutable and immutable variables.

```rs
let immutable: i64 = 5;
immutable = 7; // this will fail at compile time
```

Using `let` to bind a variable makes it immutable for its lifetime in the program. To allow mutation of a variable, use `mut` instead of `let`:

```rs
mut mutable: i64 = 5;
mutable = 7; // successful
```

When a variable is created, the type annotation can be skipped over if it is initialized (as the type can be inferred). When it is not initialized, it is an error to not type annotate it:

```rs
let init = 3 // its type will be inferred to be `i64`

mut stuff // this will cause a compile time error
```

_Note_: In the future, we can infer the type of a variable without annotation or initialization based on the first value they are assigned.

In every scope, there is a variable named `'_'` which allows you to discard values you don't have need for:

```rs
let a = 5
let b = 10
_ = 15 - (a + b) // this value is discarded
```
