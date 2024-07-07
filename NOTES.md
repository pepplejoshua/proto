# Improvements for 0.0.1
- Prevent use of array types without size in some receiver contexts (function parameters, struct members) [DONE]
- Add support for method calls on builtin types (Option, Array, Slice, String). [DONE]
- Do type rewrite to stop using the same shape for every damn type. I thought I was cool but I was just an architecture astronaut. Note to self: just do the simple fucking thing until there is concrete proof that a more complex solution is required. [DONE]
- Fix string interpolation involving str literals (which are const char arrays) in C++. Pass them through proto_str. Which would require differentiating between string expressions and the string parts of interpolated strings. [DONE]
- Make members of builtins (Optional, Array, Slice) static instead of per check. [On Hold. I tried to implement this but since Arrays, Slices and Optionals depend on types we don't know at the time the builtins method map is initialized, I cannot define the function types (which themselves require an Optional type sometimes). Unless I make the Optional type take an optional type. Will require too much work to do that and it will affect unrelated code. I am considering prepopulating these types with their methods. Once I have structs built, I'm sure I can figure something out. Using type tables, I can generate 1 version of a type at most with its relevant methods attached to the type. Or once I add generics, I can just return a generic type?]
- Make sure all functions return from the last instruction if they have non-void return type. If the last instruction is an if statement, make sure it covers all cases (i.e. provides an else) with returns. [DONE]
- Work on out of order declarations. [On Hold. Implemented topological sort using weak references to names. Not the best but it works in the top level and in structs.]
- Add support for structs. [DONE. Other small tasks have spawned from this one.]
- Explore how untyped integers can be used (by containing a pointer to the originating expression) in typechecking where we don't have context of use early. so `a :: 1` will be untyped int until `b : u8 : a + 1`, where it can be enforced to be typed u8, with the 1 expression checked again.
- Add defer instruction. [DONE]
- Add loops, continue, break.
- Add compound assignment expressions (+=, -=, *=, /=).
- Add range expression.
- Work on pointers and references.
- Add functional methods (map, filter) to iterables.
- Add Vec<T> type for growable vectors.
- Consider if tuples are valuable to add (if I can implement them myself in C++)
- Reimplement variables and constants within structs differently. Maybe restrict access to fields to only through self? This will help with distinguishing between methods and assoc functions. This can probably also help with mutability rule checking for methods. Since instead of marking all fields as const, we would just mark self as const.
- Decide on mutability rule checking for methods. A const instance of a struct cannot call a non const member function. A non-const function can call const and non-const functions. This will require a way to also specify that a function does not mutate the self instance or any instance variables.
```rs
struct Node {
    n : int;

    fn init(n int) void {
        self.n = n;
    }

    const fn does_nothing() {
        // self will be const. Every field will also be const
        println("does nothing was called.");
    }

    fn update(new_n int) void {
        self.n = new_n;
    }
}

fn main() int {
    a := Node(30);
    a.does_nothing(); // will work just fine
    a.update(1);      // will also work fine. a is not a constant

    b :: Node(500);
    b.does_nothing(); // will work just fine
    b.update(250);    // should error, since b is a const (self is a const)

    return 0;
}
```
- Mutable/const function parameters
- Work on type tables. Ty will be just type information. Which will be tracked by the type table using type IDs generated from hashes. This will restrict the number of types generated in a program to one instance per type. The program will now have type instances which hold a type id for the actual type and the SourceRef of the type instance. They will be heavily used while types themselves will be stored in the type table. This type table can get generated alongside user code. To allow introspection.
- Can SourceRef be made smaller, without affecting error reporting?
- For literals like Strings, Slices, Arrays and Optional, instead of just generating the literal in C++, wrap it with the actual type. This will mean these expression nodes will carry their type with them. So the literals can be used in any instance (like printing a `some a` where a is an array) without a complaint about knowing the type to use.
- Add support for traits.
- Determine how exactly types propagate top down in expr essions. Fix inconsistencies. [DONE. For now seems okay. I trickle types down as required from a parent context (types from variable, constants, function parameters, et.c). In the case of binary statements, I can use a sibling to typecheck an expression node. This is especially useful when one of the siblings has a guaranteed type (a identifier) and the other sibling would have been inferred (to be int for example) without the context of its sibling's type. So we can decide to check one before the other.]
- Work on some type of support for generic types and functions.
- Write something for blog about journey so far (lexer, parser, seman and codegen).
- Improve error reporting. Instead of current style, maybe allow combining multiple string sources from different sources to provide more information in errors.
- Work on some type of support for variadic functions (using slices?). Is it really needed?
- Work on recoverable errors in parser.
- Function overloading?
- Write something for blog about progress since last post.
- Understand the implications of using smart pointers to deal with auto deallocation vs not.
- Work on support for allocators (explicit or implicitly through context passing).
- Can we avoid runtime pointer errors by combining lexical analysis with smart pointers, allocators and Option type.
- Work on function call stack (maybe using context passing?)
- Add panic(), and todo() statements.
- Support for enums.
- Look into supporting functions as first call expressions/types.
- LSP support in zed
- LSP support in vscode
- Look into comments and how they encapsulate code. To be used for doc strings
- Std Lib?
- Tests?
- Modules?
- proto testing framework built in language?
- Notebook support? 👀
- 0.0.1 release?

// infinite loop
i := 0
for {
    if i >= arr.len() {
        break
    }
    n :: arr[i]
    println(`arr[{i}] = {arr[i]}`)
    i = i + 1
}
println("")

// regular c-style loop
for (i := 0; i < arr.len(); i = i + 1) {
    println(`arr[{i}] = {arr[i]}`)
}
println("")

i = 0
// while loop, with post instruction
for i < arr.len() : (i = i + 1) {
    n :: arr[i]
    println(`arr[{i}] = {arr[i]}`)
}
