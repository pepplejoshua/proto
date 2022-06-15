package ast

import "strings"

type VariableDecl struct {
	Assignee Identifier
	Assigned Expression
	Mutable  bool
}

func (v *VariableDecl) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("(")
	if v.Mutable {
		repr.WriteString("mut ")
	} else {
		repr.WriteString("let ")
	}

	repr.WriteString(v.Assignee.LiteralRepr())

	return repr.String()
}
