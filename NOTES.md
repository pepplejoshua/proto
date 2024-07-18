# Improvements for 0.0.1
- Prevent use of array types without size in some receiver contexts (function parameters, struct members) [DONE]
- Add support for method calls on builtin types (Option, Array, Slice, String). [DONE]
- Do type rewrite to stop using the same shape for every damn type. I thought I was cool but I was just an architecture astronaut. Note to self: just do the simple fucking thing until there is concrete proof that a more complex solution is required. [DONE]
- Fix string interpolation involving str literals (which are const char arrays) in C++. Pass them through proto_str. Which would require differentiating between string expressions and the string parts of interpolated strings. [DONE]
- Make members of builtins (Optional, Array, Slice) static instead of per check. [On Hold. I tried to implement this but since Arrays, Slices and Optionals depend on types we don't know at the time the builtins method map is initialized, I cannot define the function types (which themselves require an Optional type sometimes). Unless I make the Optional type take an optional type. Will require too much work to do that and it will affect unrelated code. I am considering prepopulating these types with their methods. Once I have structs built, I'm sure I can figure something out. Using type tables, I can generate 1 version of a type at most with its relevant methods attached to the type. Or once I add generics, I can just return a generic type?]
- Make sure all functions return from the last instruction if they have non-void return type. If the last instruction is an if statement, make sure it covers all cases (i.e. provides an else) with returns. [DONE]
- Work on out of order declarations. [On Hold. Implemented topological sort using weak references to names. Not the best but it works in the top level and in structs.]
- Add support for structs. [DONE. Other small tasks have spawned from this one.]
- Add defer instruction. [DONE]
- Add loops, continue, break. [DONE]
- Add compound assignment expressions (+=, -=, *=, /=, %=). [DONE]
- Add function expressions (lambdas). [DONE]
- Work on pointers. [DONE]
- Make pointers printable (implement proto_str() for it). [DONE]
- Add naive allocation (`new` and `free` essentially). To be removed when allocators are good (and replaced with a NaiveAllocator that uses that). [DONE]
- Add hashmaps. Implement methods and [] indexing. [DONE]
- Add floats and doubles.
- Add range expression (they will need to be restricted for a few use cases).
- Consider if tuples are valuable to add (if I can implement them myself in C++)
- Make self's type to be a pointer to the current type. This will stop C++ from implicitly removing its generated copy constructor.
- Allow access members on pointers of types that have members. Compile to PtrAccessMember expression. It will work well with the self-as-pointer change.
- Remove direct access to fields and methods within structs. All access to internal data should go through self. This will help when I implement const methods since I can make just self const and use it to determine whether fields can be mutated, and also if a non-const function can be called.
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
- Look into how comparator function can be implemented for the types used in hashmaps.
- Add using for struct composing other structs.
- Add option for user to provide deinit() void function on structs to handle clean up of internal resources. How does this interact with allocators?
- Report an error if a struct field refers to itself unwrap by another types
- Should new return a pointer or an optional pointer?
- Add CFG to seman for checking validity of variable references (they have been initialized through all execution paths). Use Claude 3.5 Sonnet as a guide.
- Make functions printable (generate a string in seman to show the type).
- Work on support for allocators (explicit or implicitly through context passing).
- Explore how untyped integers can be used (by containing a pointer to the originating expression) in typechecking where we don't have context of use early. so `a :: 1` will be untyped int until `b : u8 : a + 1`, where it can be enforced to be typed u8, with the 1 expression checked again.
- Add String type for growable strings. Requires an explicit allocator passed into it.
- Improve typechecking of NamedTypes. Use type_is_valid more pervasively and check that NamedTypes exist in the current scope
- Look into implementing my own Char and Str types. Use starting work in t.h
- Allow declaring functions with `fn name() ret_ty {}` syntax within function blocks. Generate lambda assigned to a constant after checking. In the lambda, do not capture the environment. Functions declared this way are self-contained with no reference to outside scope. They are not closures.
- Work on type tables. Ty will be just type information. Which will be tracked by the type table using type IDs generated from hashes. This will restrict the number of types generated in a program to one instance per type. The program will now have type instances which hold a type id for the actual type and the SourceRef of the type instance. They will be heavily used while types themselves will be stored in the type table. This type table can get generated alongside user code. To allow introspection. This will allow the implementation of of typeid as a type.
- Add functional methods (map, filter) to iterables.
- Reimplement variables and constants within structs differently. Maybe restrict access to fields to only through self? This will help with distinguishing between methods and assoc functions. This can probably also help with mutability rule checking for methods. Since instead of marking all fields as const, we would just mark self as const.
- Add do keyword for single statement functions / lambdas (non-block). This will allow forgoing a void type for the function since I can check for `{` or `do` to determine whether to just parse the body, or to parse a type, and then parse a body.
```rs
fn single_explicit() void do println("hello")
fn single_implicit() do println("hello")
fn multi_explicit() void {
    s :: "hello"
    println(s)
}
fn multi_implicit() {
    s :: "hello"
    println(s)
}
```
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
- Can we avoid runtime pointer errors by combining lexical analysis with smart pointers, allocators and using Option type for nullable pointers?
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
