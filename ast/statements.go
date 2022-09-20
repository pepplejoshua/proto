package ast

import (
	"proto/lexer"
	"proto/shared"
	"strings"
)

type CppLiteral struct {
	Start lexer.ProtoToken
	Lines []string
}

func (cl *CppLiteral) LiteralRepr() string {
	var msg strings.Builder
	msg.WriteString("cpp {")
	for _, line := range cl.Lines {
		msg.WriteString("  " + line)
	}
	msg.WriteString("}")
	return msg.String()
}

func (cl *CppLiteral) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	for _, line := range cl.Lines {
		c.WriteLine(line, use_tab)
	}

	if newline {
		c.WriteLine("", use_tab)
	}
}

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
	c.Write("struct "+s.Name.LiteralRepr()+" {", use_tab, false)

	c.Indent()
	for _, item := range s.Members {
		c.Write(item.Id_Type.CppTypeSignature()+" ", true, false)
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
	IsMain                bool
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

	if fn.Name.LiteralRepr() == "main" && fn.IsMain {
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
	Start         lexer.ProtoToken
	End           lexer.ProtoToken
	Contents      []ProtoNode
	Modules       []*Module
	Functions     []*FunctionDef
	VariableDecls []*VariableDecl
	Structs       []*Struct
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

type UseStmt struct {
	Start lexer.ProtoToken
	Paths []*Path
}

func (us *UseStmt) LiteralRepr() string {
	var msg strings.Builder

	for _, path := range us.Paths {
		msg.WriteString("use ")
		msg.WriteString(path.String() + ";\n")
	}
	return msg.String()
}

func (us *UseStmt) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	for _, path := range us.Paths {
		// decide what steps are required to compile a use statement
		// into C++ include/using syntax
		file := shared.Make_src_path(path.PathSrcLoc, false)
		c.AddInclude("\"" + file + "\"")
		if len(path.Pieces) == 1 {
			continue
		}
		// first of all, detect how much of the pieces left are modules
		index := 0
		import_all := false
		alias_import := false
		var last_name *PathIDNode
	loop:
		for in, piece := range path.Pieces {
			switch ac := piece.(type) {
			case *PathIDNode:
				if ac.Type == lexer.STAR {
					// println("*", ac.Type)
					import_all = true
					last_name = path.Pieces[in-1].(*PathIDNode)
					index = in
					break loop
				} else {
					// println(ac.String(), ac.Type)
					index = in
					continue
				}
			case *UseAs:
				// println(ac.String(), ac.PType)
				alias_import = true
				index = in
				last_name = path.Pieces[in-1].(*PathIDNode)
				break loop
			}
		}
		// include_line := fmt.Sprintf("#include '%s'", file)
		// c.WriteLine(include_line, use_tab)
		using_line := "using "
		accum := []string{}
		has_func_var_struct := false
		for in, piece := range path.Pieces {
			id := piece.(*PathIDNode)
			if id.Type == lexer.FN || id.Type == lexer.LET || id.Type == lexer.STRUCT {
				has_func_var_struct = true
				accum = append(accum, piece.String()+";")
				break
			}
			if in == index {
				if !import_all && !alias_import {
					accum = append(accum, piece.String())
				}
				if len(accum)-1 >= 0 {
					accum[len(accum)-1] += ";"
				}
				break
			}
			if in < index {
				accum = append(accum, piece.String())
			}
		}

		if has_func_var_struct {
			using_line += strings.Join(accum, "::")
		} else {
			using_line += "namespace " + strings.Join(accum, "::")
		}

		c.WriteLine(using_line, use_tab)

		// we still have nodes to process
		// which would be a *, or an as new_id
		piece := path.Pieces[index]
		if import_all && index < len(path.Pieces)-1 {
			using := "using namespace " + last_name.String() + ";"
			c.WriteLine(using, use_tab)
		} else if alias_import {
			as := piece.(*UseAs)
			using := "auto " + as.As.LiteralRepr() + " = " + last_name.String() + ";"
			c.WriteLine(using, use_tab)
		}
	}
	if newline {
		c.WriteLine("", use_tab)
	}
}

type UsePath interface {
	String() string
}

type Path struct {
	Start      *lexer.ProtoToken
	Pieces     []UsePath
	PathSrcLoc string
	UseSrcLoc  string
	Matchable  []UsePath
}

func (p *Path) String() string {
	var str strings.Builder

	for index, item := range p.Pieces {
		str.WriteString(item.String())
		if index+1 == len(p.Pieces)-1 {
			switch p.Pieces[index+1].(type) {
			case *PathIDNode:
				str.WriteString("::")
			case *UseAs:
				str.WriteString(" ")
			}
		} else if index+1 < len(p.Pieces)-1 {
			str.WriteString("::")
		}
	}
	return str.String()
}

type PathIDNode struct {
	Id   *Identifier
	Type string
}

func (n *PathIDNode) String() string {
	return n.Id.LiteralRepr()
}

// use parent::child_a::kid as grandchild
type UseAs struct {
	As    *Identifier
	Type  ProtoType
	PType string
}

func (as *UseAs) String() string {
	return "as " + as.As.LiteralRepr()
}

type Module struct {
	Start lexer.ProtoToken
	Body  *BlockStmt
	Name  *Identifier
}

func (m *Module) LiteralRepr() string {
	var str strings.Builder
	str.WriteString("mod " + m.Name.LiteralRepr() + "\n")
	str.WriteString(m.Body.LiteralRepr())
	return str.String()
}

func (m *Module) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.Write("namespace "+m.Name.LiteralRepr(), use_tab, false)
	m.Body.AsCppCode(c, true, true)
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

	if i.ElseBody != nil {
		c.Write("else", true, true)
		switch actual := i.ElseBody.(type) {
		case *BlockStmt:
			actual.AsCppCode(c, true, true)
		case *IfStmt:
			actual.AsCppCode(c, true, true)
		}
	}
}
