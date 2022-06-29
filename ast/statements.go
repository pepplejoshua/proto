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
	str.WriteString(" " + a.AssignmentToken.Literal + " ")
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

type GenericForLoop struct {
	Start         lexer.ProtoToken
	Init          *VariableDecl
	LoopCondition Expression
	Update        ProtoNode
	Body          *Block
}

func (g *GenericForLoop) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("(for ")
	repr.WriteString(g.Init.LiteralRepr() + " ")
	repr.WriteString(g.LoopCondition.LiteralRepr() + " ")
	repr.WriteString(g.Update.LiteralRepr() + " ")
	repr.WriteString(g.Body.LiteralRepr() + ")")

	return repr.String()
}

type CollectionsForLoop struct {
	Start      lexer.ProtoToken
	LoopVar    Identifier
	Collection Expression
	Body       *Block
}

func (c *CollectionsForLoop) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("(for ")
	repr.WriteString(c.LoopVar.LiteralRepr() + " in ")
	repr.WriteString(c.Collection.LiteralRepr() + " ")
	repr.WriteString(c.Body.LiteralRepr() + ")")

	return repr.String()
}

type InfiniteLoop struct {
	Start lexer.ProtoToken
	Body  *Block
}

func (i *InfiniteLoop) LiteralRepr() string {
	return "(loop " + i.Body.LiteralRepr() + ")"
}

type WhileLoop struct {
	Start         lexer.ProtoToken
	LoopCondition Expression
	Body          *Block
}

func (w *WhileLoop) LiteralRepr() string {
	return "(while " + w.LoopCondition.LiteralRepr() + " " + w.Body.LiteralRepr() + ")"
}

type FunctionDef struct {
	Start                 lexer.ProtoToken
	Name                  *Identifier
	ParameterList         []*Identifier
	ReturnType            ProtoType
	Body                  *Block
	FunctionTypeSignature *Proto_Function
}

func (fn *FunctionDef) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("(fn " + fn.Name.LiteralRepr() + "(")

	for indx, id := range fn.ParameterList {
		repr.WriteString(id.LiteralRepr() + ": " + id.Id_Type.TypeSignature())
		if indx+1 < len(fn.ParameterList) {
			repr.WriteString(", ")
		}
	}

	repr.WriteString(") -> ")
	repr.WriteString(fn.ReturnType.TypeSignature() + " ")

	repr.WriteString(fn.Body.LiteralRepr() + ")")

	return repr.String()
}

type Return struct {
	Token lexer.ProtoToken
	Value Expression
}

func (r *Return) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("(return")
	if r.Value != nil {
		repr.WriteString(" " + r.Value.LiteralRepr() + ")")
	} else {
		repr.WriteString(")")
	}

	return repr.String()
}

type Break struct {
	Token lexer.ProtoToken
}

func (b *Break) LiteralRepr() string {
	return "(break)"
}

type Continue struct {
	Token lexer.ProtoToken
}

func (c *Continue) LiteralRepr() string {
	return "(continue)"
}
