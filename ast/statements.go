package ast

import "strings"

type VariableDecl struct {
	Assignee Identifier
	Assigned Expression
	Mutable  bool
	VarType  ProtoType
}

func (v *VariableDecl) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("(")
	if v.Mutable {
		repr.WriteString("mut ")
	} else {
		repr.WriteString("let ")
	}

	repr.WriteString(v.Assignee.LiteralRepr() + ": ")
	repr.WriteString(v.VarType.TypeSignature())
	if v.Assigned != nil {
		repr.WriteString(" " + v.Assigned.LiteralRepr())
	}
	repr.WriteString(")")

	return repr.String()
}
