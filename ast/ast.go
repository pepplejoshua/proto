package ast

type ProtoNode interface {
	LiteralRepr() string
}

// a proto program just contains a bunch of ProtoNodes
type ProtoProgram struct {
	Main         *FunctionDef
	Contents     []ProtoNode
	FunctionDefs []*FunctionDef
	Structs      []*Struct
}
