# Proto Programming Language Design Specification

## 1. Core Philosophy

Proto is designed around these key principles:
- Explicit over implicit
- Simple but powerful
- Clear memory ownership
- Zero-cost abstractions
- No hidden control flow
- Composition over inheritance

## 2. Type System

### Basic Types
```rust
i8, i16, i32, i64     // Signed integers
u8, u16, u32, u64     // Unsigned integers
f32, f64              // Floating point
bool                  // Boolean
char                  // Unicode character
str                   // UTF-8 string slice
```

### Compound Types
```rust
[]T                   // Slice of T
[N]T                  // Array of T with size N
*T                    // Pointer to T
&T                    // Reference to T (immutable)
&mut T                // Mutable reference to T
```

### User-Defined Types
```rust
// Structures
struct Vector2 {
    x: f32,
    y: f32,
}

// Generic structures
struct HashMap(K, V) {
    keys: []K,
    values: []V,
    len: usize,
}

// Enums with payloads
enum Option(T) {
    Some(T),
    None,
}

enum Result(T, E) {
    Ok(T),
    Err(E),
}
```

## 3. Memory and Ownership

### Ownership Rules
- Each value has exactly one owner
- Ownership can be transferred explicitly
- References can be borrowed but must not outlive the owner
- Mutable references are exclusive

### Reference and Pointer Syntax
```rust
fn example(
    value: &Vector2,        // Immutable reference
    mut_value: &mut Vector2,// Mutable reference
    raw_ptr: *Vector2,      // Raw pointer
    owned: Vector2,         // Owned value
) void {
    // ...
}
```

### Memory Management
```rust
// Wrong - frees before return
fn create_buffer(alloc: Allocator, size: usize) ![]u8 {
    mut buf = try alloc.create([]u8, size)
    defer alloc.free(buf)  // BUG: frees immediately before return
    return buf
}

// Correct - caller is responsible for freeing
fn create_buffer(alloc: Allocator, size: usize) ![]u8 {
    return alloc.create([]u8, size)
}

// Usage example
fn example() !void {
    mut buf = try create_buffer(allocator, 1024)
    defer allocator.free(buf)  // Caller handles cleanup
    // Use buf...
}
```

### Reference Rules
```rust
```rust
fn demonstrate_refs() void {
    // Values must be mutable to create &mut references
    let point1 = Point{ x = 10, y = 20 }
    let ref_err = &mut point1        // Error: can't get &mut from immutable value

    mut point2 = Point{ x = 10, y = 20 }
    mut ref1 = &mut point2          // OK: point2 is mutable
    ref1.x = 30                     // OK: can modify through mutable ref

    // Demonstrate exclusivity with different value
    mut point3 = Point{ x = 30, y = 40 }
    mut ref2 = &mut point3          // OK: different value

    // Reference reassignment
    ref1 = &mut point3              // OK: ref1 is mut, can reassign

    // Immutable reference examples
    let ref3 = &point2              // OK: can create & reference
    ref3.x = 50                     // Error: can't modify through & reference
}

// Parameter mutation rules:
fn modify(p: &mut Point) void {
    p.x += 10                       // OK: can modify pointed-to values
    p = &mut other_point           // Error: parameter p is not mut
}
```

Key Rules:
Only mutable values (`mut`) can have `&mut` references taken
1. Mutable references (`&mut T`) allow modifying pointed-to values
2. Only one `&mut T` can exist at a time for a given value
3. No `&T` can exist while `&mut T` exists for the same value
4. Reference reassignment requires `mut` declaration
5. Parameters are immutable by default, including reference parameters

## 4. Syntax

### Functions
```rust
// Basic function
fn add(a: i32, b: i32) i32 {
    return a + b
}

// Function with error handling
fn divide(a: i32, b: i32) !f64 {
    if b == 0 {
        return error.DivideByZero
    }
    return a as f64 / b as f64  // Type casting with 'as'
}
```

### Control Flow
```rust
// Pattern matching
match option {
    Some(value) => value,
    None => default,
}

// Error handling with try/catch
fn process() !void {
    try something_risky() catch |err| {
        log.error("Failed: {}", err)
        return err
    }
}

// Loops
// Iterator loop (v0.1)
for items |item, index| {
    // ...
}

// C style loop (v0.1)
for (mut i = 0; i < 10; i = i + 2) {
    // ...
}

// Whle loop (v0.1)
for condition {
    // ...
}

// While loop with post code per loop (later)
for condition : { condition = update_condition() } {
    // ...
}
```

### Type Casting
```rust
let x = 42 as f64        // Explicit type casting
let ptr = &value as *T   // Reference to raw pointer
```

### Modules
```rust
// In file: point.pr
pub fn new_point(x: f32, y: f32) Point {
    return Point{ x = x, y = y }
}

// In file: main.pr
use point.new_point  // Import specific item
use point.{Point}    // Import multiple items
use point.*         // Import everything public

fn main() void {
    mut p = new_point(10, 20)
    // ...
}
```
Key Module Rules:
1. Each file is a module
2. `pub` keyword makes items visible outside the module
3. Simple path-based imports with `use`
4. No complex module hierarchies for v0.1

## 5. Core Library (v0.1)

### Essential Types
1. `Option(T)` - Optional values
2. `Result(T, E)` - Success or failure
3. `Error` - Error handling
4. `Allocator` - Memory management

### Basic Modules
```rust
std.io      // Basic input/output operations
std.fmt     // Formatting
std.alloc   // Memory allocation
```

## 6. Implementation Phases

### Phase 1: Core Language
1. Lexer and Parser
  - Basic tokens (keywords, operators, literals)
  - Function declarations
  - Variable declarations (mut/let)
  - Basic expressions
  - Control flow structures (if, while, for)
2. Basic Type System
  - Primitive types (i32, f64, bool, etc.)
  - Basic type checking
  - Function signatures
  - Variable type inference
3. Memory management
  - Allocator interface
  - Basic ownership tracking
  - Reference handling (&T, &mut T)
  - Pointer operations (*T)
4. Error handling
  - Error type
  - Try/catch mechanism
  - Error propagation (!)
5. Functions and Control Flow
  - Function calls
  - Return values
  - Basic control structures
  - Defer statement


### Phase 2: Type System
1. Enums and pattern matching
  - Enum declarations with payloads
  - Match expressions
  - Option(T) implementation
  - Result(T, E) implementation
2. Generics
  - Generic type parameters
  - Generic functions
  - Generic structs
  - Type constraints (if needed for v0.1)
3. User-defined Types
  - Struct definitions
  - Field access
  - Struct initialization
  - Struct methods (if needed for v0.1)
4. Type Inference
  - Local variable type inference
  - Function return type inference
  - Generic type inference


### Phase 3: Standard Library
1. Core types implementation
  - Option(T)
  - Result(T, E)
  - Basic collections (if needed)
2. Basic I/O
  - File operations
  - Standard input/output
  - Basic formatting
3. Memory Allocator
  - General purpose allocator
  - Arena allocator
  - Allocation tracking
4. Formatting utilities
  - String formatting
  - Number formatting
  - Error formatting
