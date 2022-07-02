# proto

A language compiler exploring as many parts of compiler theory as we (the writers) can muster.

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
unit: ()
```

The **unit** type is the return type of statements (like variable definitions) and functions that return no values (procedures). It cannot be assigned to a variable.

<br>

## Complex types (and their type annotations)

---

- Arrays are type annotated as `[T]`, where `T` is the type of the contained elements. For example:

  ```rs
  [1, 2, 3, 4, 5] is inferred to be or annotated [i64]
  ```

  When an empty array is used, it is necessary to type annotate the expected type. Without the annotation, the typechecker will throw an error. For example, these will work:

  ```rust
  let a = [i64;]
  let b: [[i64]] = [a, a, [1, 2, 3, 4]];
  ```

  with `a` being set to an array of `i64`.
  These will not work:

  ```rust
  let a = [];
  let b: [char] = [];
  ```

- Tuples are the only heterogenous complex type (i.e they allow the mixing of different types). Once created, new elements cannot be added to extend a tuple. They are type annotated `(T`<sub>`1`</sub>, `T`<sub>`2`</sub>, `...`, `T`<sub>`n`</sub>`)` where each `T` from `1` to `n` can be a different type. For example:

  ```rs
  (1, 'c', "some string", false) is inferred to be or annotated (i64, char, str, bool)
  ```

- Ranges are another complex type. They are annotated as `Range<T>`, where `T` is an ordinal type (i.e can be counted and put in a one-to-one correspondence with positive integers). Both `char` and `i64` types are ordinal by this definition. The elements in each of these sets of types can be compared to one another to attain some form of 'order'. In this way, they are similar to Pascal's subrange type. A range literal can be specified with a `start..end`, where the range terminates at 1 before the `end` provided. You can also specify an end-inclusive range literal using `start..=end`, where the range terminates exactly at the end. For example:

  ```rs
    let a: Range<i64> = 1..10 // runs from 1 to 9
    let b: Range<char> = 'a'..='z' // runs from 'a' to 'z'
  ```

  It is an error to specify the start of a range to be bigger than its end. For example:

  ```rs
    let a = 5;
    mut b = 2;
    let c = a..=b; // this will result in an error
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
    let a = 5;
    a * 5;
} // returns unit, since i64 expression was terminated with ;
```

## Conditionals

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

The `else` part of the construct also allows further `if` expressions to check more conditional cases:

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

## Loops

Sometimes, code needs to repeat a number of times, or infinitely until a termination condition is reached.

`For` loops come in 2 kinds: a generic (C++ like `for` loop) kind and a collection iteration (Python like `for` loop) kind.

Finding the sum of an `i64` array in Proto using the generic kind might look like:

```rs
let arr = [1, 2, 3, 4, 5];
mut sum: i64;
for mut indx = 0; indx < arr.len(); indx += 1 {
    let num = arr[indx]
    sum += num;
}
println(sum); // prints 15
```

It takes an initialized variable (`mut indx = 0;` in the above example), a condition for continuing the looping (`indx < arr.len();` in the above example) and a statement for updating the state of the loop variant (the loop variant in this case is `indx` and the update statement is `indx += 1`).

To make it more tidy for the case of collections (like arrays, hashmaps, ranges and others), we will have a collection-centric `for` loop:

```rs
let arr = [1, 2, 3, 4, 5];
mut sum: i64;
for num in arr {
    sum += num;
}
println(sum); // prints 15
```

Using a range in a `for` loop would look like:

```rs
let arr = [1, 2, 3, 4, 5]
mut sum: i64
for index in 0..arr.len() {
    sum += arr[index];
}
println(sum); // prints 15
```

To loop a piece of code infinitely until a code is met (or not), you use `loop` construct:

```rs
mut num: i64 = 0;
loop {
    println(num);
    num += 1;
}
```

The above loop will run infinitely. To terminate an infinite loop, you use the `break` keyword:

```rs
mut num: i64 = 0;
loop {
    if num >= 5000 {
        break;
    }
    println(num);
    num += 1
}
```

Another keyword for use in a loop is the `continue` keyword. It allows skipping the rest of the body of a loop and jumping to the next iteration of the loop:

```rs
fn is_even(n: i64) -> bool {
    n % 2 == 0
}

mut num: i64 = 0;
loop {
    if num >= 5000 {
        break;
    }

    num += 1;
    if is_even(num-1) {
        continue;
    }
    println(num-1);
}
```

The above code snippet will print all the odd numbers between 0 and 4999.

There is also a `while` loop construct. These three looping constructs allow you write a loop however you see fit. A `while` loop takes a boolean condition and a code block. To print all the odd numbers from 0 to 5000 using all 3 loop constructs for example:

```rs
fn is_even(n: i64) -> bool {
    n % 2 == 0
}

// using a generic for loop
for mut i = 0; i < 5000; i += 1 {
    if is_even(i) {
        println(i);
    }
}

// using collections for loop + range
for i in 0..5000 {
    if is_even(i) {
        println(i);
    }
}

// using an infinite loop
mut num: i64 = 0;
loop {
    if num >= 5000 {
        break;
    }
    if is_even(num) {
        println(num);
    }
    num += 1;
}

// using a while loop
num = 0;
while num < 5000 {
    if is_even(num) {
        println(num)
    }
    num += 1
}
```

## Builtin Operators

There are builtin operators (some of which we have already seen). Below are their type signatures. Operator overloading will be implemented in future updates. Upscaling of types to handle overflow will also be allowed in future updates (like `*: (i64, i64) -> i128`).

They take the form: `operator: (inputs) -> output`.
Binary operators are:

```rust
    // for + operator
    +: (i64, i64) -> i64
    +: (char, char) -> str
    +: (str, str) -> str
    +: (str, char) -> char

    // for -, *, / and % operators
    -: (i64, i64) -> i64
    *: (i64, i64) -> i64
    /: (i64, i64) -> i64 // will allow float output in future updates
    %: (i64, i64) -> i64

    // for >, >=, < and <= operators
    >: (i64, i64) -> bool
    >: (char, char) -> bool
    >=: (i64, i64) -> bool
    >=: (char, char) -> bool
    <: (i64, i64) -> bool
    <: (char, char) -> bool
    <=: (i64, i64) -> bool
    <=: (char, char) -> bool

    // for && and || operators
    &&: (bool, bool) -> bool
    ||: (bool, bool) -> bool

    // for == and != operators
    ==: (i64, i64) -> bool
    ==: (char, char) -> bool
    ==: (str, str) -> bool
    ==: (bool, bool) -> bool
    !=: (i64, i64) -> bool
    !=: (char, char) -> bool
    !=: (str, str) -> bool
    !=: (bool, bool) -> bool
```

Unary operators are:

```rust
    // for - and not operator
    not: (bool) -> bool
    -: (i64) -> i64
```

## Function Definition and Use

Proto allows the definition of functions to encapsulate behaviour. An example of a function that checks if a number is even might look like:

```rust
fn is_even(num: i64) -> bool {
    return num % 2 == 0;
}

fn main() {
    let a = 5;
    if is_even(a) {
        println("even");
    } else {
        println("odd");
    }
}
```

The `is_even` function captures what is germaine about function definitions in proto. It has a single parameter `num` which is of `i64` type and it has a return value of `bool` specified after the arrow (`->`). Function calls in proto are not any different from regular function calls, so its form is `callable`(`arguments`);

Since the body of functions are block expressions, functions that return values can forgo a return statement, as there is an explicit return at the end of the block (if it is an expression). So `is_even` can be rewritten as:

```rust
fn is_even(num: i64) -> bool {
    num % 2 == 0
}
```

This allows using `return` statements for early returns and explicit block returns for late returns. For example:

```rust
fn even_or_odd_msg(num: i64) -> (str, bool) {
    if num % 2 == 0 {
        return ("it is even", true);
    }
    ("it is odd", false)
}

fn main() {
    let tuple = even_or_odd(4); // tuple restructuring will be a future update
    println(tuple.0); // will preint "it is even"
}
```

## Structs
