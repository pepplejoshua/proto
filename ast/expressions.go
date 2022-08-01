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

type BinaryOp struct {
	Left     Expression
	Right    Expression
	Operator lexer.ProtoToken
	Op_Type  ProtoType
}

func (b *BinaryOp) LiteralRepr() string {
	var msg strings.Builder

	msg.WriteString("(")
	msg.WriteString(b.Operator.Literal)
	msg.WriteString(" " + b.Left.LiteralRepr())
	msg.WriteString(" " + b.Right.LiteralRepr() + ")")

	return msg.String()
}

func (b *BinaryOp) Type() ProtoType {
	return b.Op_Type
}

type UnaryOp struct {
	Operand  Expression
	Operator lexer.ProtoToken
	Op_Type  ProtoType
}

func (u *UnaryOp) LiteralRepr() string {
	var msg strings.Builder

	msg.WriteString("(")
	msg.WriteString(u.Operator.Literal)
	msg.WriteString(" " + u.Operand.LiteralRepr() + ")")

	return msg.String()
}

func (u *UnaryOp) Type() ProtoType {
	return u.Op_Type
}

type Unit struct {
	Token lexer.ProtoToken
}

func (u *Unit) LiteralRepr() string {
	return "()"
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

type Block struct {
	Start     lexer.ProtoToken
	Contents  []ProtoNode
	BlockType ProtoType
}

func (b *Block) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("{ ")
	for index, node := range b.Contents {
		str.WriteString(node.LiteralRepr())

		if index+1 < len(b.Contents) {
			str.WriteString(" ")
		}
	}

	str.WriteString(" }: ")
	str.WriteString(b.Type().TypeSignature())

	return str.String()
}

func (b *Block) Type() ProtoType {
	return b.BlockType
}

type IfConditional struct {
	Start     lexer.ProtoToken
	Condition Expression
	ThenBody  *Block
	ElseBody  Expression
	IfType    ProtoType
}

func (i *IfConditional) LiteralRepr() string {
	var str strings.Builder

	str.WriteString("(if ")
	str.WriteString(i.Condition.LiteralRepr() + " ")
	str.WriteString(i.ThenBody.LiteralRepr())

	if i.ElseBody != nil {
		str.WriteString(" else " + i.ElseBody.LiteralRepr())
	}

	str.WriteString("): " + i.IfType.TypeSignature())

	return str.String()
}

func (i *IfConditional) Type() ProtoType {
	return i.IfType
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
	return r.Start.LiteralRepr() + ".." + r.PastEnd.LiteralRepr() +
		": " + r.RangeType.TypeSignature()
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
	return i.Start.LiteralRepr() + "..=" + i.End.LiteralRepr() +
		": " + i.RangeType.TypeSignature()
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

func (m *Membership) Type() ProtoType {
	return m.MembershipType
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

func (s *StructInitialization) Type() ProtoType {
	return s.StructType
}
