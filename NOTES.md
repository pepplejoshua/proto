- use other proto files in your proto file with @use("path_to_proto")
  - this will allow you to use the types and functions in the other proto file in your code
  - this is similar to the `use` keyword in rust

- memory management through the use of custom allocators or (new | delete) constructs
  - this will allow us to manage memory in a way that is more efficient than the default memory management in rust
  - we can also use this to manage memory in a way that is more efficient than the default memory management in rust

- Uniform Function Calling Syntax ?
  - stuff like 1.add(2) which is equivalent to add(1, 2)

- compile execution of code based on KNOWN information
  - this can allow us generate code from generic functions and types
  - we can also load things at compile time and generate code based on that

- use c libs with @lib("path_to_lib_mapping_in_proto")
  - we will use this to map the types and function in the c lib to the proto so you can use them in your code
  - @ffi directive is used on prototypes to inform the compiler that the function is implemented in a c lib. jo blow mentions "dyn_call"
  - @ffi("__c_lib_name__") can be used to specify the actual name in the c lib to link against