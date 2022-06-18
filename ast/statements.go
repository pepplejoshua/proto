package ast

import (
	"proto/lexer"
	"strings"
)

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

type Struct struct {
	Start   lexer.ProtoToken
	Name    Identifier
	Members []*Identifier
}

func (s *Struct) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("(struct ")
	repr.WriteString(s.Name.LiteralRepr() + " { ")

	for index, mem := range s.Members {
		repr.WriteString(mem.LiteralRepr() + ": ")
		repr.WriteString(mem.Id_Type.TypeSignature())

		if index+1 < len(s.Members) {
			repr.WriteString(", ")
		}
	}
	repr.WriteString(" })")

	return repr.String()
}

type Assignment struct {
	Target          Expression
	AssignmentToken lexer.ProtoToken
	Assigned        Expression
}

func (a *Assignment) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("(")
	str.WriteString(a.Target.LiteralRepr())
	str.WriteString(" = ")
	str.WriteString(a.Assigned.LiteralRepr())
	str.WriteString(")")
	return str.String()
}

type PromotedExpr struct {
	Start lexer.ProtoToken
	Expr  Expression
}

func (p *PromotedExpr) LiteralRepr() string {
	return p.Expr.LiteralRepr() + ";"
}
