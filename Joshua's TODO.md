# Joshua's TODO

---

- [x] replace make_token() with lexer.make_token()

- [x] write remaining tests for lexer

- [x] begin work on parser

  - [x] finish variable declaration
  - [ ] work on parsing hashmaps
  - [ ] work on parsing function calls
  - [ ] work on parsing . expressions (e.g a.b, struct.member)
  - [x] work on parsing types

- [ ] add double arithmetic operators like +=, -=, \*=, /= and others

- [ ] write parser tests

  - [x] use s-exprs to make testing parsing easier

- [ ] implement traits/interface

  - [ ] implement range using `..` and `..=` after adding in traits/interfaces. Will require comparison trait
  - [ ] implement collections based loops. e.g:

  ```rust
    let a = [1, 2, 3, 4]
    for item in a {

    }

    // or

    for index, item in a.enumerate() {

    }
  ```
