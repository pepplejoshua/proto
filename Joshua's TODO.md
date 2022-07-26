# Joshua's TODO

---

- [ ] Fix early return

  - [ ] between ripping out Halt and making use of implicit behaviour of return opcode and doing some hacky stuff to jump to halt on early return from main(). I prefer the first. 1 less opcode. I'd really just need to fix tests.

- [ ] Update READMe

  - [ ] include structs and initialization
  - [ ] include function definitions and use

- [x] replace make_token() with lexer.make_token()

- [x] write remaining tests for lexer

- [x] work on structs

  - [x] struct initialization expression
  - [ ] struct member function definition

- [x] begin work on parser

  - [x] finish variable declaration
  - [x] work on parsing function calls
  - [x] work on parsing . expressions (e.g a.b, struct.member)
  - [x] work on parsing types

- [x] add double arithmetic operators like +=, -=, \*=, /= and others

- [x] write parser tests

  - [x] use s-exprs to make testing parsing easier

- [x] implement range using `..` and `..=`.

- [ ] make sure all block contents are ; terminated (in Parser) except for the last line (which is allowed to not be terminated)

- [ ] implement traits/interface

  - [x] implement collections based loops. e.g:

  ```rust
    let a = [1, 2, 3, 4]
    for item in a {

    }

    // or

    for index, item in a.enumerate() {

    }
  ```

```rust
let a = 2;

fn stuff() -> i64 {
  1
}

fn main() {
  a = stuff();
}

// TODO

fn accept_i64(a: i64) -> bool {
  if a > 2 {
    false
  } else {
    true
  }
}

fn main() {
  let a = 2;
  let b = accept_i64(a);
}
```
