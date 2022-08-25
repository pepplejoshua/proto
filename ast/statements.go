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

	if v.Mutable {
		repr.WriteString("mut ")
	} else {
		repr.WriteString("let ")
	}

	repr.WriteString(v.Assignee.LiteralRepr())
	if v.Assigned != nil {
		repr.WriteString(" = " + v.Assigned.LiteralRepr())
	}

	return repr.String()
}

func (v *VariableDecl) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if !v.Mutable {
		c.Write("const", use_tab, false)
	}

	// if we haven't assigned anything to it, then we need the actual
	// type, not auto
	c.Write(v.VarType.CppTypeSignature(), false, true)
	c.Write(v.Assignee.LiteralRepr(), false, true)
	if v.Assigned != nil {
		c.Write("= ", false, true)
		v.Assigned.AsCppCode(c, false, false)
	}

	if newline {
		c.WriteLine(";", false)
	} else {
		c.Write(";", false, false)
	}
}

type Struct struct {
	Start   lexer.ProtoToken
	Name    Identifier
	Members []*Identifier
}

func (s *Struct) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("struct ")
	repr.WriteString(s.Name.LiteralRepr() + " { ")

	for index, mem := range s.Members {
		repr.WriteString(mem.LiteralRepr() + ": ")
		repr.WriteString(mem.Id_Type.TypeSignature())

		if index+1 < len(s.Members) {
			repr.WriteString(", ")
		}
	}
	repr.WriteString(" }")

	return repr.String()
}

func (s *Struct) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.Write(s.Name.LiteralRepr()+" {", use_tab, false)

	c.Indent()
	for _, item := range s.Members {
		c.Write(item.Id_Type.CppTypeSignature(), true, false)
		item.AsCppCode(c, false, false)
		c.WriteLine(";", false)
	}
	c.Dedent()

	c.WriteLine("};", true)
}

type Assignment struct {
	Target          Expression
	AssignmentToken lexer.ProtoToken
	Assigned        Expression
	HasSemiColon    bool
}

func (a *Assignment) LiteralRepr() string {
	var str strings.Builder

	str.WriteString(a.Target.LiteralRepr())
	str.WriteString(" " + a.AssignmentToken.Literal + " ")
	str.WriteString(a.Assigned.LiteralRepr())
	return str.String()
}

func (a *Assignment) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	a.Target.AsCppCode(c, use_tab, false)
	c.Write(a.AssignmentToken.Literal+" ", false, true)
	a.Assigned.AsCppCode(c, false, false)
	if newline {
		if a.HasSemiColon {
			c.WriteLine(";", false)
		} else {
			c.WriteLine("", false)
		}
	} else {
		if a.HasSemiColon {
			c.Write(";", false, false)
		} else {
			c.Write("", false, false)
		}
	}
}

type PromotedExpr struct {
	Start lexer.ProtoToken
	Expr  Expression
}

func (p *PromotedExpr) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	p.Expr.AsCppCode(c, use_tab, false)
	if newline {
		c.WriteLine(";", false)
	} else {
		c.Write(";", false, false)
	}
}

func (p *PromotedExpr) LiteralRepr() string {
	return p.Expr.LiteralRepr() + ";"
}

type GenericForLoop struct {
	Start         lexer.ProtoToken
	Init          *VariableDecl
	LoopCondition Expression
	Update        ProtoNode
	Body          *BlockStmt
}

func (g *GenericForLoop) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("for ")
	repr.WriteString(g.Init.LiteralRepr() + " ")
	repr.WriteString(g.LoopCondition.LiteralRepr() + " ")
	repr.WriteString(g.Update.LiteralRepr() + " ")
	repr.WriteString(g.Body.LiteralRepr())

	return repr.String()
}

func (g *GenericForLoop) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.Write("for (", use_tab, false)
	g.Init.AsCppCode(c, false, false)
	g.LoopCondition.AsCppCode(c, false, false)
	c.Write(";", false, false)
	g.Update.AsCppCode(c, false, false)
	c.WriteLine(")", false)

	g.Body.AsCppCode(c, true, true)
}

type CollectionsForLoop struct {
	Start      lexer.ProtoToken
	LoopVar    Identifier
	Collection Expression
	Body       *BlockStmt
}

func (c *CollectionsForLoop) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("for ")
	repr.WriteString(c.LoopVar.LiteralRepr() + " in ")
	repr.WriteString(c.Collection.LiteralRepr() + " ")
	repr.WriteString(c.Body.LiteralRepr())

	return repr.String()
}

func (cfl *CollectionsForLoop) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.NewLine()
	}
	c.Write("for (auto", true, false)
	cfl.LoopVar.AsCppCode(c, false, false)
	c.Write(": ", false, true)
	cfl.Collection.AsCppCode(c, false, false)
	c.Write(")", false, false)

	cfl.Body.AsCppCode(c, true, true)
}

type InfiniteLoop struct {
	Start lexer.ProtoToken
	Body  *BlockStmt
}

func (i *InfiniteLoop) LiteralRepr() string {
	return "loop " + i.Body.LiteralRepr()
}

func (i *InfiniteLoop) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.NewLine()
	}

	c.WriteLine("while (true)", true)
	i.Body.AsCppCode(c, true, true)
}

type WhileLoop struct {
	Start         lexer.ProtoToken
	LoopCondition Expression
	Body          *BlockStmt
}

func (w *WhileLoop) LiteralRepr() string {
	return "while " + w.LoopCondition.LiteralRepr() + " " + w.Body.LiteralRepr()
}

func (w *WhileLoop) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.NewLine()
	}

	c.Write("while (", true, false)
	w.LoopCondition.AsCppCode(c, false, false)
	c.Write(")", false, false)
	w.Body.AsCppCode(c, true, true)
}

type FunctionDef struct {
	Start                 lexer.ProtoToken
	Name                  *Identifier
	ParameterList         []*Identifier
	ReturnType            ProtoType
	Body                  *BlockExpr
	FunctionTypeSignature *Proto_Function
}

func (fn *FunctionDef) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("fn " + fn.Name.LiteralRepr() + "(")

	for indx, id := range fn.ParameterList {
		repr.WriteString(id.LiteralRepr() + ": " + id.Id_Type.TypeSignature())
		if indx+1 < len(fn.ParameterList) {
			repr.WriteString(", ")
		}
	}

	repr.WriteString(") -> ")
	repr.WriteString(fn.ReturnType.TypeSignature() + " ")

	repr.WriteString(fn.Body.LiteralRepr())

	return repr.String()
}

func (fn *FunctionDef) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.Write(fn.ReturnType.CppTypeSignature(), true, false)

	if fn.Name.LiteralRepr() == "main" {
		c.Write("__main", false, true)
	} else {
		c.Write(fn.Name.LiteralRepr(), false, true)
	}
	c.Write("(", false, false)

	for index, param := range fn.ParameterList {
		c.Write(param.Id_Type.CppTypeSignature()+" ", false, false)
		param.AsCppCode(c, false, false)

		if index+1 < len(fn.ParameterList) {
			c.Write(", ", false, false)
		}
	}
	c.WriteLine(") {", false)
	generate_code_from_block_expr(c, fn.Body)
	c.WriteLine("}", true)
	c.NewLine()
}

type Return struct {
	Token lexer.ProtoToken
	Value Expression
}

func (r *Return) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString("return")
	if r.Value != nil {
		repr.WriteString(" " + r.Value.LiteralRepr() + ";")
	} else {
		repr.WriteString(";")
	}

	return repr.String()
}

func (r *Return) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.Write("return", use_tab, false)

	if r.Value == nil {
		c.Write("Proto_Unit();", false, true)
	} else {
		c.Write(" ", false, false)
		r.Value.AsCppCode(c, false, false)
		c.Write(";", false, false)
	}

	if newline {
		c.NewLine()
	}
}

type Break struct {
	Token lexer.ProtoToken
}

func (b *Break) LiteralRepr() string {
	return "break;"
}

func (b *Break) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.WriteLine("break;", true)
}

type Continue struct {
	Token lexer.ProtoToken
}

func (c *Continue) LiteralRepr() string {
	return "continue;"
}

func (b *Continue) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.WriteLine("continue;", true)
}

type BlockStmt struct {
	Start    lexer.ProtoToken
	Contents []ProtoNode
}

func (b *BlockStmt) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("{ ")
	for index, node := range b.Contents {
		str.WriteString(node.LiteralRepr())

		if index+1 < len(b.Contents) {
			str.WriteString(" ")
		}
	}

	str.WriteString(" }")

	return str.String()
}

func (b *BlockStmt) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine("{", use_tab)
	} else {
		c.Write("{", use_tab, false)
	}
	c.Indent()
	for _, node := range b.Contents {
		node.AsCppCode(c, true, true)
	}
	c.Dedent()

	if newline {
		c.WriteLine("}", true)
	} else {
		c.Write("}", true, false)
	}
}

type IfStmt struct {
	Start     lexer.ProtoToken
	Condition Expression
	ThenBody  *BlockStmt
	ElseBody  ProtoNode
}

func (i *IfStmt) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("if ")
	str.WriteString(i.Condition.LiteralRepr() + " ")
	str.WriteString(i.ThenBody.LiteralRepr())

	if i.ElseBody != nil {
		str.WriteString(" else " + i.ElseBody.LiteralRepr())
	}
	return str.String()
}

func (i *IfStmt) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.NewLine()
	}
	c.Write("if (", true, false)
	i.Condition.AsCppCode(c, false, false)
	c.Write(") ", false, false)
	i.ThenBody.AsCppCode(c, true, true)

	c.Write("else", true, true)
	switch actual := i.ElseBody.(type) {
	case *BlockStmt:
		actual.AsCppCode(c, true, true)
	case *IfStmt:
		actual.AsCppCode(c, true, true)
	case nil:
	}
	// if i.ElseBody != nil {
	// 	c.Write(" else ")
	// 	code.WriteString(" else " + i.ElseBody.AsCppCode(tab))
	// }
}
