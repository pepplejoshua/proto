# Proto Language Description

Proto is a typed programming language. The long term goal is to compile to native code through LLVM. To support the REPL, it will have a Virtual Machine written in Rust ([_proton_](../src/proton/README.md)). This VM could also power compile time expressions.

Instead of dwelling in the future, let us describe what the language will look like. This document will get updated as more language features get added in future versions.

## Content

---

1. [Standard Library](#standard-library)
2. [Hello World]()

<br>

### Standard Library

---

The Standard Library for Proto has its own documentation. It can be found [here](./tbd.md).

<br>

### Hello World

---

#### **`hello.pr`**

```rs
use std::[print];

fn main() void {
  println("Hello, {#}!\n", "world");
}
```

#### **`Shell`**

```bash
$ proto -r hello.pr
Hello, world!
```

<br>
