# Improvements for 0.0.1
- Prevent use of array types without size in some receiver contexts (function parameters, struct members) [DONE]
- Add support for method calls on builtin types (Option, Array, Slice, String). [DONE]
- Do type rewrite to stop using the same shape for every damn type. I thought I was cool but I was just an architecture astronaut. Note to self: just do the simple fucking thing until there is concrete proof that a more complex solution is required. [DONE]
- Fix string interpolation involving str literals (which are const char arrays) in C++. Pass them through proto_str. Which would require differentiating between string expressions and the string parts of interpolated strings. [DONE]
- Make members of builtins (Optional, Array, Slice) static instead of per check. [On Hold. I tried to implement this but since Arrays, Slices and Optionals depend on types we don't know at the time the builtins method map is initialized, I cannot define the function types (which themselves require an Optional type sometimes). Unless I make the Optional type take an optional type. Will require too much work to do that and it will affect unrelated code. I am considering prepopulating these types with their methods. Once I have structs built, I'm sure I can figure something out. Using type tables, I can generate 1 version of a type at most with its relevant methods attached to the type.]
- Make sure all functions return from the last instruction if they have non-void return type. If the last instruction is an if statement, make sure it covers all cases (i.e. provides an else) with returns. [DONE]
- Work on out of order declarations.
- Add support for structs.
- Work on type tables. Ty will be just type information. Which will be tracked by the type table using type IDs generated from hashes. This will restrict the number of types generated in a program to one instance per type. Instead the program will have type instances which hold a type id for the actual type and the location of the type instance.
- Can SourceRef be made smaller, without affecting error reporting?
- For literals like Strings, Slices, Arrays and Optional, instead of just generating the literal in C++, wrap it with the actual type. This will mean these expression nodes will carry their type with them. So the literals can be used in any instance (like printing a `some a` where a is an array) without a complaint about knowing the type to use.
- Add support for traits.
- Determine how exactly types propagate top down in expr essions. Fix inconsistencies.
- Mutable/const function parameters
- Work on pointers and references.
- Write something for blog about journey so far (lexer, parser, seman and codegen).
- Improve error reporting. Instead of current style, maybe allow combining multiple string sources from different sources to provide more information in errors.
- Work on some type of support for variadic functions (using slices?). Is it really needed?
- Work on recoverable errors in parser.
- Work on some type of support for generic types and functions.
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
