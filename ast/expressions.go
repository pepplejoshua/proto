package ast

import (
	"proto/lexer"
	"strings"
)

type Expression interface { // anything that doesn't implement this is a statement
	ProtoNode
	Type() ProtoType
}

type Identifier struct {
	Token   lexer.ProtoToken
	Id_Type ProtoType
}

func (i *Identifier) LiteralRepr() string {
	return i.Token.Literal
}

func (i *Identifier) Type() ProtoType {
	return i.Id_Type
}

func (i *Identifier) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if strings.Contains(i.Id_Type.CppTypeSignature(), "vector") {
		c.AddInclude("<vector>")
	} else if strings.Contains(i.Id_Type.CppTypeSignature(), "string") {
		c.AddInclude("<string>")
	} else if strings.Contains(i.Id_Type.CppTypeSignature(), "tuple") {
		c.AddInclude("<tuple>")
	}
	if newline {
		c.WriteLine(i.LiteralRepr(), use_tab)
	} else {
		c.Write(i.LiteralRepr(), use_tab, false)
	}
}

type BinaryOp struct {
	Left     Expression
	Right    Expression
	Operator lexer.ProtoToken
	Op_Type  ProtoType
}

func (b *BinaryOp) LiteralRepr() string {
	var msg strings.Builder

	msg.WriteString(b.Left.LiteralRepr())
	msg.WriteString(" " + b.Operator.Literal)
	msg.WriteString(" " + b.Right.LiteralRepr())

	return msg.String()
}

func (b *BinaryOp) Type() ProtoType {
	return b.Op_Type
}

func (b *BinaryOp) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine(b.LiteralRepr(), use_tab)
	} else {
		b.Left.AsCppCode(c, false, false)
		c.Write(b.Operator.Literal+" ", false, true)
		b.Right.AsCppCode(c, false, false)
	}
}

type UnaryOp struct {
	Operand  Expression
	Operator lexer.ProtoToken
	Op_Type  ProtoType
}

func (u *UnaryOp) LiteralRepr() string {
	var msg strings.Builder

	msg.WriteString(u.Operator.Literal)
	msg.WriteString(" " + u.Operand.LiteralRepr())

	return msg.String()
}

func (u *UnaryOp) Type() ProtoType {
	return u.Op_Type
}

func (u *UnaryOp) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if u.Operator.Literal == "not" {
		c.Write("!", false, true)
	} else {
		c.Write(u.Operator.Literal, false, true)
	}
	u.Operand.AsCppCode(c, false, false)
}

type Unit struct {
	Token lexer.ProtoToken
}

func (u *Unit) LiteralRepr() string {
	return "()"
}

func (u *Unit) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.WriteLine("Proto_Unit()", false)
}

func (u *Unit) Type() ProtoType {
	return &Proto_Unit{}
}

type I64 struct {
	Token lexer.ProtoToken
}

func (i *I64) LiteralRepr() string {
	return i.Token.Literal
}

func (i *I64) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine(i.LiteralRepr(), use_tab)
	} else {
		c.Write(i.LiteralRepr(), use_tab, false)
	}
}

func (i *I64) Type() ProtoType {
	return &Proto_Builtin{
		TypeToken: lexer.ProtoToken{
			Type:      lexer.I64_TYPE,
			Literal:   "i64",
			TokenSpan: i.Token.TokenSpan,
		},
	}
}

type Char struct {
	Token lexer.ProtoToken
}

func (c *Char) LiteralRepr() string {
	return "'" + c.Token.Literal + "'"
}

func (ch *Char) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine(ch.LiteralRepr(), use_tab)
	} else {
		c.Write(ch.LiteralRepr(), use_tab, false)
	}
}

func (c *Char) Type() ProtoType {
	return &Proto_Builtin{
		TypeToken: lexer.ProtoToken{
			Type:      lexer.CHAR_TYPE,
			Literal:   "char",
			TokenSpan: c.Token.TokenSpan,
		},
	}
}

type String struct {
	Token lexer.ProtoToken
}

func (s *String) LiteralRepr() string {
	return s.Token.Literal
}

func (s *String) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine(s.LiteralRepr(), use_tab)
	} else {
		c.Write(s.LiteralRepr(), use_tab, false)
	}
	c.AddInclude("<string>")
}

func (s *String) Type() ProtoType {
	return &Proto_Builtin{
		TypeToken: lexer.ProtoToken{
			Type:      lexer.STRING_TYPE,
			Literal:   "str",
			TokenSpan: s.Token.TokenSpan,
		},
	}
}

type Boolean struct {
	Value bool
	Token lexer.ProtoToken
}

func (b *Boolean) LiteralRepr() string {
	return b.Token.Literal
}

func (b *Boolean) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine(b.LiteralRepr(), use_tab)
	} else {
		c.Write(b.LiteralRepr(), use_tab, false)
	}
}

func (b *Boolean) Type() ProtoType {
	return &Proto_Builtin{
		TypeToken: lexer.ProtoToken{
			Type:      lexer.BOOL_TYPE,
			Literal:   "bool",
			TokenSpan: b.Token.TokenSpan,
		},
	}
}

type Array struct {
	Items     []Expression
	Token     lexer.ProtoToken
	ArrayType *Proto_Array
}

func (a *Array) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.Write("vector<"+a.ArrayType.InternalType.CppTypeSignature()+">{", use_tab, true)
	for index, item := range a.Items {
		item.AsCppCode(c, false, false)

		if index+1 < len(a.Items) {
			c.Write(", ", false, false)
		}
	}
	c.WriteLine(" }", false)
	c.AddInclude("<vector>")
}

func (a *Array) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("[")

	for index, item := range a.Items {
		str.WriteString(item.LiteralRepr())

		if index+1 < len(a.Items) {
			str.WriteString(", ")
		}
	}
	str.WriteString("]")
	return str.String()
}

func (a *Array) Type() ProtoType {
	return a.ArrayType
}

type Tuple struct {
	Items     []Expression
	Token     lexer.ProtoToken
	TupleType *Proto_Tuple
}

func (t *Tuple) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	tuple_items := NewCodeGenerator()
	c.Write("tuple<", use_tab, false)

	for index, item := range t.Items {
		c.Write(item.Type().CppTypeSignature(), false, false)
		item.AsCppCode(tuple_items, false, false)
		if index+1 < len(t.Items) {
			c.Write(", ", false, false)
			tuple_items.Write(", ", false, false)
		}
	}

	c.Write("> ("+tuple_items.CollectString()+")", false, false)
	c.AddInclude("<tuple>")
	for k := range tuple_items.includes {
		c.AddInclude(k)
	}

	if newline {
		c.WriteLine("", false)
	}
}

func (t *Tuple) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("(")

	for index, item := range t.Items {
		str.WriteString(item.LiteralRepr())

		if index+1 < len(t.Items) || len(t.Items) == 1 {
			str.WriteString(", ")
		}
	}
	str.WriteString(")")
	return str.String()
}

func (t *Tuple) Type() ProtoType {
	return t.TupleType
}

type BlockExpr struct {
	Start     lexer.ProtoToken
	Contents  []ProtoNode
	BlockType ProtoType
}

func (b *BlockExpr) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.WriteLine("[&](){", use_tab)
	generate_code_from_block_expr(c, b)
	c.Write("}()", true, false)
	if newline {
		c.NewLine()
	}
}

func (b *BlockExpr) LiteralRepr() string {
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

func (b *BlockExpr) Type() ProtoType {
	return b.BlockType
}

type IfExpr struct {
	Start     lexer.ProtoToken
	Condition Expression
	ThenBody  *BlockExpr
	ElseBody  ProtoNode
	IfType    ProtoType
}

func (i *IfExpr) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("if ")
	str.WriteString(i.Condition.LiteralRepr() + " ")
	str.WriteString(i.ThenBody.LiteralRepr())

	if i.ElseBody != nil {
		str.WriteString(" else " + i.ElseBody.LiteralRepr())
	}

	return str.String()
}

func (i *IfExpr) Type() ProtoType {
	return i.IfType
}

func (i *IfExpr) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.WriteLine("[&]() {", false)
	c.Indent()
	c.Write("if (", true, false)
	i.Condition.AsCppCode(c, false, false)
	c.WriteLine(") {", false)

	generate_code_from_block_expr(c, i.ThenBody)
	c.Write("}", true, false)

	switch actual := i.ElseBody.(type) {
	case *BlockExpr:
		c.WriteLine(" else {", false)
		generate_code_from_block_expr(c, actual)
		c.WriteLine("}", true)
	case *IfExpr:
		c.WriteLine(" else {", false)
		c.Write("return ", true, false)
		c.Indent()
		actual.AsCppCode(c, false, false)
		c.Dedent()
		c.WriteLine("}", true)
	}
	c.Dedent()
	c.Write("}()", true, false)

	if newline {
		c.NewLine()
	}
}

type CallExpression struct {
	Start      lexer.ProtoToken
	Callable   Expression
	Arguments  []Expression
	ReturnType ProtoType
}

func (c *CallExpression) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString(c.Callable.LiteralRepr() + "(")

	for index, arg := range c.Arguments {
		repr.WriteString(arg.LiteralRepr())
		if index+1 < len(c.Arguments) {
			repr.WriteString(", ")
		}
	}

	repr.WriteString(")")
	return repr.String()
}

func (cl *CallExpression) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	cl.Callable.AsCppCode(c, use_tab, false)
	c.Write("(", false, false)
	for index, arg := range cl.Arguments {
		arg.AsCppCode(c, false, false)

		if index+1 < len(cl.Arguments) {
			c.Write(", ", false, false)
		}
	}
	c.Write(")", false, false)
	if newline {
		c.NewLine()
	}
}

func (c *CallExpression) Type() ProtoType {
	return c.ReturnType
}

type IndexExpression struct {
	Start     lexer.ProtoToken
	Indexable Expression
	Index     Expression
	ValueType ProtoType
}

func (i *IndexExpression) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString(i.Indexable.LiteralRepr() + "[")
	repr.WriteString(i.Index.LiteralRepr() + "]")

	return repr.String()
}

func (i *IndexExpression) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	i.Indexable.AsCppCode(c, use_tab, false)
	c.Write("[", false, false)
	i.Index.AsCppCode(c, false, false)
	c.Write("]", false, false)
	if newline {
		c.NewLine()
	}
}

func (i *IndexExpression) Type() ProtoType {
	return i.ValueType
}

type Range struct {
	Start     Expression
	PastEnd   Expression
	Operator  lexer.ProtoToken
	RangeType *Proto_Range
}

func (r *Range) LiteralRepr() string {
	return r.Start.LiteralRepr() + ".." + r.PastEnd.LiteralRepr()
}

func (r *Range) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine(r.LiteralRepr(), use_tab)
	} else {
		c.Write(r.LiteralRepr(), use_tab, false)
	}
}

func (r *Range) Type() ProtoType {
	return r.RangeType
}

type InclusiveRange struct {
	Start     Expression
	End       Expression
	Operator  lexer.ProtoToken
	RangeType *Proto_Range
}

func (i *InclusiveRange) LiteralRepr() string {
	return i.Start.LiteralRepr() + "..=" + i.End.LiteralRepr()
}

func (i *InclusiveRange) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine(i.LiteralRepr(), use_tab)
	} else {
		c.Write(i.LiteralRepr(), use_tab, false)
	}
}

func (i *InclusiveRange) Type() ProtoType {
	return i.RangeType
}

type Membership struct {
	Start          lexer.ProtoToken
	Object         Expression
	Member         Expression
	MembershipType ProtoType
}

func (m *Membership) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString(m.Object.LiteralRepr() + ".")
	repr.WriteString(m.Member.LiteralRepr())

	return repr.String()
}

func (m *Membership) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if _, ok := m.Object.Type().(*Proto_Tuple); ok {
		mem := m.Member.LiteralRepr()
		c.Write("get<"+mem+">(", use_tab, false)
		m.Object.AsCppCode(c, false, false)
		c.Write(")", false, newline)
		c.AddInclude("<tuple>")
	} else {
		m.Object.AsCppCode(c, use_tab, false)
		c.Write(".", false, false)
		m.Member.AsCppCode(c, false, false)
	}
}

func (m *Membership) Type() ProtoType {
	return m.MembershipType
}

type ModuleAccess struct {
	Start      lexer.ProtoToken
	Mod        Expression
	Member     Expression
	MemberType ProtoType
}

func (m *ModuleAccess) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString(m.Mod.LiteralRepr() + "::")
	repr.WriteString(m.Member.LiteralRepr())

	return repr.String()
}

func (m *ModuleAccess) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	if newline {
		c.WriteLine(m.LiteralRepr(), use_tab)
	} else {
		c.Write(m.LiteralRepr(), use_tab, false)
	}
}

func (m *ModuleAccess) Type() ProtoType {
	return m.MemberType
}

type StructInitialization struct {
	Start      lexer.ProtoToken
	StructName *Identifier
	Fields     map[*Identifier]Expression
	StructType *Proto_UserDef
}

func (s *StructInitialization) LiteralRepr() string {
	var repr strings.Builder

	repr.WriteString(s.StructName.LiteralRepr() + " { ")

	var fieldsAndInit []string
	for field, init := range s.Fields {
		fieldsAndInit = append(fieldsAndInit, field.LiteralRepr()+": "+init.LiteralRepr())
	}

	for index, str := range fieldsAndInit {
		repr.WriteString(str)

		if index+1 < len(fieldsAndInit) {
			repr.WriteString(", ")
		}
	}

	repr.WriteString(" }")

	return repr.String()
}

func (s *StructInitialization) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.Write(s.StructName.LiteralRepr()+"(", use_tab, false)

	index := 0
	len_ := len(s.Fields)

	// maintains the order in the struct definition
	// to allow correct c++ code gen
	for _, mem := range s.StructType.Definition.Members {
		for id, expr := range s.Fields {
			if id.LiteralRepr() == mem.LiteralRepr() {
				expr.AsCppCode(c, false, false)
				break
			}
		}
		if index+1 < len_ {
			c.Write(", ", false, false)
		}
		index += 1
	}
	c.WriteLine(")", false)
}

func (s *StructInitialization) Type() ProtoType {
	return s.StructType
}

type Reference struct {
	Start   lexer.ProtoToken
	RefType *Proto_Reference
	Value   Expression
}

func (r *Reference) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	r.Value.AsCppCode(c, use_tab, newline)
	// c.Write("make_shared<"+r.RefType.Inner.CppTypeSignature()+">",
	// 	use_tab, false)
	// c.Write("(", false, false)
	// if newline {
	// 	r.Value.AsCppCode(c, false, true)
	// } else {
	// 	r.Value.AsCppCode(c, false, false)
	// }
	// c.Write(")", false, false)
	// c.AddInclude("<memory>")
}

func (r *Reference) LiteralRepr() string {
	return "&" + r.Value.LiteralRepr()
}

func (r *Reference) Type() ProtoType {
	return r.RefType
}

type Dereference struct {
	Start     lexer.ProtoToken
	Value     Expression
	DerefType ProtoType
}

func (d *Dereference) AsCppCode(c *CodeGenerator, use_tab bool, newline bool) {
	c.Write("*", true, false)
	if newline {
		d.Value.AsCppCode(c, false, true)
	} else {
		d.Value.AsCppCode(c, false, false)
	}
}

func (d *Dereference) LiteralRepr() string {
	return "*" + d.Value.LiteralRepr()
}

func (d *Dereference) Type() ProtoType {
	return d.DerefType
}
