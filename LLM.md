This is how I intend to resolve this lazily and by demand. Generate no code. Just be aware of my goal:
1. Create entries for `main`, `addi`, `Int` and `globl_const` showing what instruction has to be processed to resolve a reference to them. This is probably the lazy method:
`main` => Ins(0),
`addi` => Ins(1),
`Int` => Ins(2),
`globl_const` => Ins(3)

2. I can start then with the first item, `main`.
decl_checking_stack = [`main`]
We can also remove it from the unchecked map:
`addi` => Ins(1),
`Int` => Ins(2),
`globl_const` => Ins(3)
It will reference `addi`, which is actually not in the symbol table. So we will check the `unresolved` instruction map for a matching name, which we will find. We can then check the instruction using only the global scope (which is applicable at this point).
decl_checking_stack = [`main`, `addi`]
`Int` => Ins(2),
`globl_const` => Ins(3)

3. `addi` references `Int` in its type heading, which is also in our unresolved instruction map:
decl_checking_stack = [`main`, `addi`, `Int`]
`globl_const` => Ins(3)
We will completely resolve Int at this point as a type alias:
type_table: [ type{T0, is_meta_type}, i32 ]
sym_table: { `Int`: type{1} }
decl_checking_stack = [`main`, `addi`]
`globl_const` => Ins(3)

4. While resolving the body of `addi`, we will also see `globl_const`, which is the final name in the unresolved map:
type_table: [ type{T0, is_meta_type}, i32 ]
sym_table: { `Int`: type{1} }
decl_checking_stack = [`main`, `addi`, `globl_const`]

We can resolve `globl_const`, since `Int` exists and `300` is a valid `i32`:
type_table: [ type{T0, is_meta_type}, i32, type{T1} ]
sym_table: { `Int`: 2, `globl_const`: 1 }
decl_checking_stack = [`main`, `addi`]

We can also completely resolve `addi` since we have all the information needed:
type_table: [ type{T0, is_meta_type}, i32, type{T1}, \(T2, T2) -> T2 ]
sym_table: { `Int`: T2, `globl_const`: T1, `addi`: T3 }
decl_checking_stack = [`main`]

`main` can also be fully resolved at this point since we have resolved `addi` and know what `Int` is as well:
type_table: [ type{T0, is_meta_type}, i32, type{T1}, \(T2, T2) -> T2, int, \() -> T4 ]
sym_table: { `Int`: T2, `globl_const`: T1, `addi`: T3, `main`: T5 }
decl_checking_stack = []

Lazy and Demand Driven using tracking of instructions by name, which takes advantage of how top level instructions are written in proto
