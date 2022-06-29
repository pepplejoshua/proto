package scope

import (
	"proto/ast"
	"proto/shared"
)

// might relocate to a "Runtime package" as it is only useful at runtime, alongside scope
type RuntimeFunctionWithEnvironment struct {
	OriginalFunction    *ast.FunctionDef
	EnclosedEnvironment *Environment
}

type BindingTuple struct {
	Defined bool
	Expr    ast.Expression
	Type    ast.ProtoType // used by typechecker
	Mutable bool
}

type Environment struct {
	Enclosing *Environment
	Bindings  map[string]*BindingTuple
}

func NewEnviroment(enclosing *Environment) *Environment {
	return &Environment{
		Enclosing: enclosing,
		Bindings:  make(map[string]*BindingTuple),
	}
}

func (e *Environment) Bind(ident string, value *BindingTuple) {
	e.Bindings[ident] = value
}

func (e *Environment) Get(ident string) *BindingTuple {
	value, found := e.Bindings[ident]

	if !found {
		if e.Enclosing != nil {
			return e.Enclosing.Get(ident)
		} else {
			shared.ReportErrorAndExit("Environment", "Undefined variable '"+ident+"'.")
		}
	}

	return value
}

func (e *Environment) Update(ident string, value *BindingTuple) {
	_, exists := e.Bindings[ident]

	if !exists {
		if e.Enclosing != nil {
			e.Enclosing.Update(ident, value)
		} else {
			shared.ReportErrorAndExit("Environment", "Undefined variable '"+ident+"'.")
		}
	} else {
		e.Bindings[ident] = value
	}
}

func (e *Environment) Contains(ident string) bool {
	_, exists := e.Bindings[ident]

	if !exists {
		if e.Enclosing != nil {
			return e.Enclosing.Contains(ident)
		}
	}
	return exists
}
