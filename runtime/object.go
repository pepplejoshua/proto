package runtime

import (
	"fmt"
	"strings"
)

type RuntimeObj interface {
	String() string
}

type I64 struct {
	Value int64
}

func (i *I64) String() string {
	return fmt.Sprintf("%d", i.Value)
}

type Bool struct {
	Value bool
}

func (b *Bool) String() string {
	if b.Value {
		return "true"
	} else {
		return "false"
	}
}

type Char struct {
	Value string
}

func (c *Char) String() string {
	return c.Value
}

type Unit struct{}

func (u *Unit) String() string {
	return "()"
}

type String struct {
	Value string
}

func (s *String) String() string {
	return s.Value
}

type Array struct {
	Items []RuntimeObj
}

func (a *Array) String() string {
	var msg strings.Builder
	msg.WriteString("[")
	for index, item := range a.Items {
		msg.WriteString(item.String())

		if index+1 != len(a.Items) {
			msg.WriteString(", ")
		}
	}
	msg.WriteString("]")
	return msg.String()
}
