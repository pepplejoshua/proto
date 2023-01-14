package types

type Type interface {
	String() string
}

type I64_type struct{}

func (i *I64_type) String() string {
	return "i64"
}

type Char_Type struct {}

func (c *Char_Type) String() string {
	return "char"
}

