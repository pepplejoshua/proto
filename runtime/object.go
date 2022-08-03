package runtime

import (
	"fmt"
	"strings"
)

type RuntimeObj interface {
	String() string
	Copy() RuntimeObj
}

type I64 struct {
	Value int64
}

func (i *I64) String() string {
	return fmt.Sprintf("%d", i.Value)
}

func (i *I64) Copy() RuntimeObj {
	return i
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

func (b *Bool) Copy() RuntimeObj {
	return b
}

type Char struct {
	Value string
}

func (c *Char) String() string {
	return "'" + c.Value + "'"
}

func (c *Char) Copy() RuntimeObj {
	return c
}

func (c *Char) Character() string {
	return c.Value
}

type Unit struct{}

func (u *Unit) String() string {
	return "()"
}

func (u *Unit) Copy() RuntimeObj {
	return u
}

type String struct {
	Value string
}

func (s *String) String() string {
	return s.Value
}

func (s *String) Content() string {
	return s.Value[1 : len(s.Value)-1]
}

func (s *String) Copy() RuntimeObj {
	return s
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

func (a *Array) Copy() RuntimeObj {
	arr := &Array{
		Items: []RuntimeObj{},
	}

	copy(arr.Items, a.Items)
	return arr
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

func (t *Tuple) Copy() RuntimeObj {
	tup := &Tuple{
		Items: []RuntimeObj{},
	}

	copy(t.Items, tup.Items)

	return tup
}

type Range struct {
	Start   RuntimeObj
	PastEnd RuntimeObj
}

func (r *Range) String() string {
	return r.Start.String() + ".." + r.PastEnd.String()
}

func (r *Range) Copy() RuntimeObj {
	rng := &Range{
		Start:   r.Start.Copy(),
		PastEnd: r.PastEnd.Copy(),
	}

	return rng
}

type InclusiveRange struct {
	Start RuntimeObj
	End   RuntimeObj
}

func (ir *InclusiveRange) String() string {
	return ir.Start.String() + "..=" + ir.End.String()
}

func (ir *InclusiveRange) Copy() RuntimeObj {
	irng := &InclusiveRange{
		Start: ir.Start.Copy(),
		End:   ir.End.Copy(),
	}

	return irng
}

type InitializedStruct struct {
	StructName string
	Members    map[string]RuntimeObj
}

func (is *InitializedStruct) String() string {
	var msg strings.Builder

	msg.WriteString(is.StructName + " { ")
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

func (is *InitializedStruct) Copy() RuntimeObj {
	n_is := &InitializedStruct{
		StructName: is.StructName,
		Members:    map[string]RuntimeObj{},
	}

	for name, value := range is.Members {
		n_is.Members[name] = value.Copy()
	}

	return n_is
}
