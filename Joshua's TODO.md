# Joshua's TODO

---

- [x] replace make_token() with lexer.make_token()

- [x] write remaining tests for lexer

- [x] begin work on parser

  - [x] finish variable declaration
  - [x] work on parsing function calls
  - [x] work on parsing . expressions (e.g a.b, struct.member)
  - [x] work on parsing types

- [x] add double arithmetic operators like +=, -=, \*=, /= and others

- [x] write parser tests

  - [x] use s-exprs to make testing parsing easier

- [x] implement range using `..` and `..=`.

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
