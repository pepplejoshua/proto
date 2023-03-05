# Proto Language Description (v0.0.1)

Proto is a typed programming language. The long term goal is to compile to native code through LLVM. To support the REPL, it will have a Virtual Machine written in Rust ([_proton_](../src/proton/README.md)). This VM could eventually power compile time expressions.

Instead of dwelling in the future, let us describe what the language will look like. This document will get updated as more language features get added in future versions.

## Content

---

1. [Standard Library](#standard-library)
2. [Hello World](#hello-world)
3. [Comments](#comments)
4. [Values/Constants](#valuesconstants)

<br>

### Standard Library

---

The Standard Library for Proto has its own documentation. It can be found [here](./tbd.md).

<br>

### Hello World

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

### Comments

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

### Values/Constants

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
  
}
```

#### **`Shell`**

```bash
$ proto -r values_constants.pr
1 * 2 = 2
false
true
false
```
