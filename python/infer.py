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

# fn example(a i32, b) {
#   let c = a + b;
#   let d = c * 2;
#   let e = d > 10;
#   return e;
# }
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
        elif stmt[0] == "return":
            return_expr = stmt[1]
            return_type = TYPE_MAP.get(return_expr, TypeVariable(f"T_{return_expr}"))
            CONSTRAINTS.append(Constraint(TypeVariable("T_return"), return_type))
            TYPE_MAP["Inferred_Return_Type"] = return_type

def unify(constraints):
    solved = {}

    # replaces all occurences of Var with Ty
    def substitute(Var, Ty):
        for constraint in constraints:
            if constraint.left == Var:
                constraint.left = Ty
            if constraint.right == Var:
                constraint.right = Ty

    while constraints:
        cons = constraints.pop(0)
        left, right = cons.left, cons.right

        # perform substitutions if required
        if isinstance(left, TypeVariable):
            if left not in solved:
                solved[left] = right
                # replace all occurences of left with right
                substitute(left, right)
        elif isinstance(right, TypeVariable):
            if right not in solved:
                solved[right] = left;
                substitute(right, left)
        elif left != right:
            raise TypeError(f"cannot unify {left} and {right}")

    return solved

collect_constraints(FN_AST)

print("[TYPES]")
for var_name, var_type in TYPE_MAP.items():
    print(f"  - {var_name} => {var_type}")
print()

print("[CONSTRAINTS]")
for constraint in CONSTRAINTS:
    print(f"  - {constraint}")
print()

# will contain the solved types from the TYPE_MAP and CONSTRAINTS
solved = unify(CONSTRAINTS)
print("[SOLVED]")
for Left, Right in solved.items():
    print(f"  - {Left} => {Right}")
