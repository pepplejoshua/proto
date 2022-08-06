package runtime

import (
	"fmt"
	"strings"
)

type RuntimeObj interface {
	String() string
	Copy() RuntimeObj
	GetOriginal() RuntimeObj
}

type I64 struct {
	Value    int64
	Original *I64
}

func (i *I64) String() string {
	return fmt.Sprintf("%d", i.Value)
}

func (i *I64) Copy() RuntimeObj {
	return &I64{
		Value:    i.Value,
		Original: i,
	}
}

func (i *I64) GetOriginal() RuntimeObj {
	return i.Original
}

type Bool struct {
	Value    bool
	Original *Bool
}

func (b *Bool) String() string {
	if b.Value {
		return "true"
	} else {
		return "false"
	}
}

func (b *Bool) Copy() RuntimeObj {
	return &Bool{
		Value:    b.Value,
		Original: b,
	}
}

func (b *Bool) GetOriginal() RuntimeObj {
	return b.Original
}

type Char struct {
	Value    string
	Original *Char
}

func (c *Char) String() string {
	return "'" + c.Value + "'"
}

func (c *Char) Copy() RuntimeObj {
	return &Char{
		Value:    c.Value,
		Original: c,
	}
}

func (c *Char) GetOriginal() RuntimeObj {
	return c.Original
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

func (u *Unit) GetOriginal() RuntimeObj {
	return u
}

type String struct {
	Value    string
	Original *String
}

// GetOriginal implements RuntimeObj
func (s *String) GetOriginal() RuntimeObj {
	return s.Original
}

func (s *String) String() string {
	return s.Value
}

func (s *String) Content() string {
	return s.Value[1 : len(s.Value)-1]
}

func (s *String) Copy() RuntimeObj {
	return &String{
		Value:    s.Value,
		Original: s,
	}
}

type Array struct {
	Items    []RuntimeObj
	Original *Array
}

// GetOriginal implements RuntimeObj
func (a *Array) GetOriginal() RuntimeObj {
	if a.Original == nil {
		return a
	}
	return a.Original
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
		Items: make([]RuntimeObj, len(a.Items)),
	}

	arr.Original = a

	for index, item := range a.Items {
		arr.Items[index] = item.Copy()
	}
	// copy(arr.Items, a.Items)
	return arr
}

type Tuple struct {
	Items    []RuntimeObj
	Original *Tuple
}

// GetOriginal implements RuntimeObj
func (t *Tuple) GetOriginal() RuntimeObj {
	if t.Original == nil {
		return t
	}
	return t.Original
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

	tup.Original = t
	copy(t.Items, tup.Items)

	return tup
}

type Range struct {
	Start   RuntimeObj
	PastEnd RuntimeObj
}

// GetOriginal implements RuntimeObj
func (r *Range) GetOriginal() RuntimeObj {
	return r
}

func (r *Range) String() string {
	return r.Start.String() + ".." + r.PastEnd.String()
}

func (r *Range) Copy() RuntimeObj {
	return r
}

type InclusiveRange struct {
	Start RuntimeObj
	End   RuntimeObj
}

// GetOriginal implements RuntimeObj
func (ir *InclusiveRange) GetOriginal() RuntimeObj {
	return ir
}

func (ir *InclusiveRange) String() string {
	return ir.Start.String() + "..=" + ir.End.String()
}

func (ir *InclusiveRange) Copy() RuntimeObj {
	return ir
}

type InitializedStruct struct {
	StructName string
	Members    map[string]RuntimeObj
	Original   *InitializedStruct
}

// GetOriginal implements RuntimeObj
func (is *InitializedStruct) GetOriginal() RuntimeObj {
	if is.Original == nil {
		return is
	}
	return is.Original
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

	n_is.Original = is

	return n_is
}

type Ref struct {
	Value RuntimeObj
}

func (r *Ref) String() string {
	return "&" + r.Value.String()
}

func (r *Ref) Copy() RuntimeObj {
	return r
}

func (r *Ref) GetOriginal() RuntimeObj {
	return r
}
