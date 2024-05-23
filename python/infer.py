from rich import print

# this represents unknown types to be solved for like
# T_b for some variable b we have to infer
class TypeVariable:
    def __init__(self, name: str) -> None:
        self.name = name

    def __str__(self) -> str:
        return f"TypeVariable({self.name})"

# this represents known types like i32, bool, etc.
class Type:
    def __init__(self, name: str) -> None:
        self.name = name

    def __str__(self) -> str:
        return f"Type({self.name})"

# this represents a constraint between 2 types
class Constraint:
    def __init__(self, left, right) -> None:
        self.left = left
        self.right = right

    def __str__(self) -> str:
        return f"Constraint[{self.left} = {self.right}]"

FN_AST = [
    ("let", "a", Type("i32")),          # a: i32
    ("let", "b", TypeVariable("T_b")),  # b: unknown
    ("let", "c", ("add", "a", "b")),    # c = a + b
    ("let", "d", ("mul", "c", 2)),      # d = c * 2
    ("let", "e", ("gt", "d", 10)),      # e = d > 10
    ("return", "e"),                    # return e
]

# will contain these after collect_constraints()
# T_c = i32 (a)
# T_c = T_b (b)
# T_d = T_c (c)
# T_d = T_2 (2)
# T_e = bool
# T_d = T_10 (d > 10)
CONSTRAINTS = []

# will contain these after collect_constraints()
# a => i32
# b => T_b
# c => T_c
# d => T_d
# e => T_e
TYPE_MAP = {}

def collect_constraints(ast):
    for stmt in ast:
        # new definition
        if stmt[0] == "let":
            var_name, init_expr = stmt[1], stmt[2]
            if isinstance(init_expr, Type):
                # known type
                TYPE_MAP[var_name] = init_expr
            elif isinstance(init_expr, TypeVariable):
                # unknown type
                TYPE_MAP[var_name] = init_expr
            elif isinstance(init_expr, tuple):
                # complex init expr
                op, left, right = init_expr
                left_type = TYPE_MAP.get(left, TypeVariable(f"T_{left}"))
                right_type = TYPE_MAP.get(right, TypeVariable(f"T_{right}"))
                var_type = TypeVariable(f"T_{var_name}")
                # store the type variable for this variable
                TYPE_MAP[var_name] = var_type

                # generate constraints based on the op type
                match op:
                    case "add" | "mul":
                        CONSTRAINTS.append(Constraint(var_type, left_type))
                        CONSTRAINTS.append(Constraint(var_type, right_type))
                    case "gt":
                        CONSTRAINTS.append(Constraint(var_type, Type("bool")))
                        CONSTRAINTS.append(Constraint(left_type, right_type))

collect_constraints(FN_AST)

print("[TYPES]")
for var_name, var_type in TYPE_MAP.items():
    print(f"  - {var_name} => {var_type}")
print()

print("[CONSTRAINTS]")
for constraint in CONSTRAINTS:
    print(f"  - {constraint}")
