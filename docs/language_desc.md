# Proto Language Description (v0.0.1)

Proto is a typed programming language. The long term goal is to compile to native code through LLVM. To support the REPL, it will have a Virtual Machine written in Rust ([_proton_](../src/proton/README.md)). This VM could eventually power compile time expressions.

Instead of dwelling in the future, let us describe what the language will look like. This document will get updated as more language features get added in future versions.

These docs were inspired by [ziglang](https://ziglang.org/documentation/master/) docs.

# Content

---

1. [Standard Library](#1-standard-library)
2. [Hello World](#2-hello-world)
3. [Comments](#3-comments)
4. [Values/Constants](#4-values--constants)
   1. [Primitive Types](#41-primitive-types)

<br>

## 1. Standard Library

---

The Standard Library for Proto has its own documentation. It can be found [here](./tbd.md).

<br>

## 2. Hello World

---

#### **`hello.pr`**

```rs
use std::io::println;

fn main() {
  println("Hello, {#}!", "world");
}
```

#### **`Shell`**

```bash
$ proto -r hello.pr
Hello, world!
```

<br>

## 3. Comments

---

#### **`comments.pr`**

```rs
use std::io::println;

fn main() {
  // a comment starts with "//" and ends when the line terminates.
  // comments are for the reader. the compiler will not read them,
  // so you are free to write as many as you like.
  println("Hello, {#}!", "world");

  // the line below will not be executed:
  // println("Does not run");
}
```

#### **`Shell`**

```bash
$ proto -r comments.pr
Hello, world!
```

> Documentation comments will be done later when work starts on adding it to proto.

<br>

## 4. Values | Constants

---

#### **`values_constants.pr`**

```rs
use std::io::println;

fn main() {
  // integers
  let one_times_two: i32 = 1 * 2;
  println("1 * 2 = {#}", one_times_two);

  // boolean
  println(
    "true and false => {#}\nfalse or true => {#}\n!true => {#}",
    true and false,
    false or true,
    !true
  );

  // characters
  let a: char = 'a';

  // string literals
  let literal: str = "this is {#} string literal";
  println(literal, a);
}
```

#### **`Shell`**

```bash
$ proto -r values_constants.pr
1 * 2 = 2
false
true
false
this is a string literal
```

<br>

### 4.1 Primitive types

---

<br>

| Type        | Description                                     | Implemented? |
| ----------- | ----------------------------------------------- | ------------ |
| **`i8`**    | signed 8-bit integer                            | ❌           |
| **`u8`**    | unsigned 8-bit integer                          | ❌           |
| **`i16`**   | signed 16-bit integer                           | ❌           |
| **`u16`**   | unsigned 16-bit integer                         | ❌           |
| **`i32`**   | signed 32-bit integer                           | ❌           |
| **`u32`**   | unsigned 32-bit integer                         | ❌           |
| **`i64`**   | signed 64-bit integer                           | ❌           |
| **`u64`**   | unsigned 64-bit integer                         | ❌           |
| **`isize`** | signed pointer sized integer. platform specific | ❌           |
| **`bool`**  | `true` or `false`                               | ❌           |
| **`void`**  | `void` as in, nothing                           | ❌           |
| **`type`**  | the type of types                               | ❌           |

<br>
