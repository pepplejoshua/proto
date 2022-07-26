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
	return "'" + c.Value + "'"
}

func (c *Char) Character() string {
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

type Tuple struct {
	Items []RuntimeObj
}

func (t *Tuple) String() string {
	var msg strings.Builder
	msg.WriteString("(")
	for index, item := range t.Items {
		msg.WriteString(item.String())

		if index+1 != len(t.Items) {
			msg.WriteString(", ")
		}
	}
	msg.WriteString(")")
	return msg.String()
}

type Range struct {
	Start   RuntimeObj
	PastEnd RuntimeObj
}

func (r *Range) String() string {
	return r.Start.String() + ".." + r.PastEnd.String()
}

type InclusiveRange struct {
	Start RuntimeObj
	End   RuntimeObj
}

func (ir *InclusiveRange) String() string {
	return ir.Start.String() + "..=" + ir.End.String()
}

type ProtoStruct struct {
	Name    string
	Members []string
}

func (s *ProtoStruct) String() string {
	var msg strings.Builder

	msg.WriteString("struct " + s.Name + " { ")
	for index, mem := range s.Members {
		msg.WriteString(mem)

		if index+1 < len(s.Members) {
			msg.WriteString(", ")
		}
	}
	msg.WriteString(" }")

	return msg.String()
}

type InitializedStruct struct {
	StructName string
	Members    map[string]RuntimeObj
}

func (is *InitializedStruct) String() string {
	var msg strings.Builder

	msg.WriteString(is.StructName + "{ ")
	length := len(is.Members)
	index := 0
	for mem, val := range is.Members {
		msg.WriteString(mem + ": " + val.String())

		if index+1 < length {
			msg.WriteString(", ")
		}
		index++
	}
	msg.WriteString(" }")

	return msg.String()
}
