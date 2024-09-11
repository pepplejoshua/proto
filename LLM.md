1. at the start of checking the top level
scopes: [
  [ top level scope
    { main: Ins[0], add: Ins[1], Byte: Ins[2], Pos: Ins[3] },
    gen_ins: []
  ]
]
types: [ type{meta_type} ]
resolving: []

2. check Ins[0] (main). we will resolve the signature of main's expression lambda and
set main's type to it, before continuing to resolve main.
scopes: [
  [ top level scope
    { main: T3, add: Ins[1], Byte: Ins[2], Pos: Ins[3] },
    gen_ins: []
  ],
]
types: [ type{meta_type}, int, \() T1 ]
resolving: [ main ]

3. In main, we will try to bind three to scope, which will require finding an add()
function. We will find add in a parent scope and get to resolving it.
scopes: [
  [ top level scope
    { main: T3, add: Ins[1], Byte: Ins[2], Pos: Ins[3] },
    gen_ins: []
  ],
  [ main scope
    {},
    gen_ins: []
  ]
]
types: [ type{meta_type}, int, \() T1 ]
resolving: [ main ]

4. Since we are diverging from main scope to check something down a different scope path, we will
use the scope add was found in as the starting scope. We will also resolve the signature like we
did main. It however references Byte in its name, which means we will have to resolve Byte before
we continue resolving add.
scopes: [
  [ top level scope
    { main: T3, add: Ins[1], Byte: Ins[2], Pos: Ins[3] },
    gen_ins: []
  ],
]
types: [ type{meta_type}, int, \() T1 ]
resolving: [ main, add ]

5. We will also find Byte in the top level scope, so we can resolve it. It is an alias to the type
u8. We can add u8 to the type table, and then bind
scopes: [
  [ top level scope
    { main: T3, add: Ins[1], Byte: T4, Pos: Ins[3] },
    gen_ins: []
  ],
]
types: [ type{meta_type}, int, \() T1, u8, type{T3} ]
resolving: [ main, add ]


1. Intern all names found into symbol table
symtable: [
  {
    main:

  }
]
