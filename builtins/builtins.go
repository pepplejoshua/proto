package builtins

import (
	"fmt"
	"proto/ast"
	"proto/runtime"
	"proto/shared"
	"strings"
)

func Sprintf(str runtime.RuntimeObj, args ...runtime.RuntimeObj) runtime.RuntimeObj {
	locations := GetFormatPositions(str.String())
	if len(locations) != len(args) {
		var msg strings.Builder
		msg.WriteString(fmt.Sprintf("%s format expected %d arguments but is called with %d.",
			"sprintf", len(locations), len(args)))
		shared.ReportErrorAndExit("VM", msg.String())
	}

	res := ""
	if len(locations) != 0 {
		start := 0
		for _, coord := range locations {
			res += str.String()[start:coord[0]]

			switch actual := args[coord[2]].(type) {
			case *runtime.Char:
				res += actual.Character()
			case *runtime.String:
				res += actual.Content()
			default:
				res += args[coord[2]].String()
			}
			start = coord[1]
		}
		res += str.String()[start:]
	} else {
		switch actual := str.(type) {
		case *runtime.Char:
			res = actual.Character()
		case *runtime.String:
			res = actual.Content()
		default:
			res = actual.String()
		}
	}

	return &runtime.String{
		Value: res,
	}
}

func Printf(str runtime.RuntimeObj, args ...runtime.RuntimeObj) runtime.RuntimeObj {
	locations := GetFormatPositions(str.String())
	if len(locations) != len(args) {
		var msg strings.Builder
		msg.WriteString(fmt.Sprintf("%s format expected %d arguments but is called with %d.",
			"sprintf", len(locations), len(args)))
		shared.ReportErrorAndExit("VM", msg.String())
	}

	res := ""
	if len(locations) != 0 {
		start := 0
		for _, coord := range locations {
			res += str.String()[start:coord[0]]

			switch actual := args[coord[2]].(type) {
			case *runtime.Char:
				res += actual.Character()
			case *runtime.String:
				res += actual.Content()
			default:
				res += args[coord[2]].String()
			}
			start = coord[1]
		}
		res += str.String()[start:]
	} else {
		res = str.String()
	}
	print(res)
	return &runtime.Unit{}
}

func GetFormatPositions(str string) [][]int {
	locations := [][]int{}
	count_of_fpos := 0
	for i := 0; i < len(str); i++ {
		char := string(str[i])
		if char == "{" {
			if i+1 != len(str) {
				char = string(str[i+1])
				if char == "#" {
					if i+2 != len(str) {
						char = string(str[i+2])
						if char == "}" {
							start := i
							end := i + 3
							pack := []int{start, end, count_of_fpos}
							count_of_fpos++
							locations = append(locations, pack)
							i = i + 2
							continue
						}
					}
				}
			}
		}
	}
	return locations
}

func TypeCheckFormatString(fn *BuiltinFn, call *ast.CallExpression) bool {
	args := call.Arguments
	if len(args) < 1 {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s typed function expects at least %d arguments but got called with %d arguments.",
			fn.LiteralRepr(), len(fn.Params)-1, len(call.Arguments)))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	first := args[0]
	if first.Type().TypeSignature() != "str" {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("Expected argument %d to be typed %s but got argument of type %s.",
			1, "str", first.Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	// if if is a raw string
	if str, ok := first.(*ast.String); ok {
		locations := GetFormatPositions(str.LiteralRepr())
		if len(locations) != len(args)-1 {
			var msg strings.Builder
			line := call.Start.TokenSpan.Line
			col := call.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("%s format expected %d arguments but is called with %d.",
				fn.Name, len(locations), len(args)-1))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			return true
		}
	}

	return false
}

func Print(str runtime.RuntimeObj, args ...runtime.RuntimeObj) runtime.RuntimeObj {
	if str == nil {
		print()
	} else {
		var out strings.Builder

		if len(args) == 0 {
			out.WriteString(str.String())
		} else {
			out.WriteString(str.String() + " ")
			for index, arg := range args {
				out.WriteString(arg.String())
				if index+1 < len(args) {
					out.WriteString(" ")
				}
			}

		}
		print(out.String())
	}
	return &runtime.Unit{}
}

func Println(str runtime.RuntimeObj, args ...runtime.RuntimeObj) runtime.RuntimeObj {
	if str == nil {
		print()
	} else {
		var out strings.Builder

		if len(args) == 0 {
			out.WriteString(str.String())
		} else {
			out.WriteString(str.String() + " ")
			for index, arg := range args {
				out.WriteString(arg.String())
				if index+1 < len(args) {
					out.WriteString(" ")
				}
			}
		}
		println(out.String())
	}
	return &runtime.Unit{}
}

func Len(arr runtime.RuntimeObj, args ...runtime.RuntimeObj) runtime.RuntimeObj {
	var length int64
	switch actual := arr.(type) {
	case *runtime.Array:
		length = int64(len(actual.Items))
	case *runtime.String:
		length = int64(len(actual.Content()))
	}

	return &runtime.I64{
		Value: length,
	}
}

func TypeCheckForArrayString(fn *BuiltinFn, call *ast.CallExpression) bool {
	args := call.Arguments
	if len(call.Arguments) != 1 {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s typed function expects %d arguments but got called with %d arguments.",
			fn.LiteralRepr(), len(fn.Params), len(call.Arguments)))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	array := args[0]
	switch actual := array.Type().(type) {
	case *ast.Proto_Array:
	case *ast.Proto_Builtin:
		if actual.TypeSignature() != "str" {
			var msg strings.Builder
			line := call.Start.TokenSpan.Line
			col := call.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("%s expects an Array or a string.",
				fn.Name))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			return true
		}
	default:
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects an Array or a string.",
			fn.Name))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}
	return false
}

type ProtoGoFn func(runtime.RuntimeObj, ...runtime.RuntimeObj) runtime.RuntimeObj

type BuiltinFn struct {
	Fn             ProtoGoFn
	Params         []string
	Returns        string
	Name           string
	HasTypeChecker bool
	TypeChecker    func(*BuiltinFn, *ast.CallExpression) bool
}

var Builtins = []*BuiltinFn{
	{
		Fn:             Sprintf,
		Params:         []string{"str", "...AnyProtoType"},
		Returns:        "str",
		Name:           "sprintf",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckFormatString,
	},
	{
		Fn:             Printf,
		Params:         []string{"str", "...AnyProtoType"},
		Returns:        "()",
		Name:           "printf",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckFormatString,
	},
	{
		Fn:             Print,
		Params:         []string{"...AnyProtoType"},
		Returns:        "()",
		HasTypeChecker: false,
		Name:           "print",
	},
	{
		Fn:             Println,
		Params:         []string{"...AnyProtoType"},
		Returns:        "()",
		HasTypeChecker: false,
		Name:           "println",
	},
	{
		Fn:             Len,
		Params:         []string{"str | Array of AnyProtoType"},
		Returns:        "i64",
		Name:           "len",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckForArrayString,
	},
}

func (bf *BuiltinFn) LiteralRepr() string {
	return "fn " + bf.Name + "(" +
		strings.Join(bf.Params, ", ") +
		") -> " + bf.Returns
}

func IsBuiltin(name string) bool {
	for _, b := range Builtins {
		if name == b.Name {
			return true
		}
	}
	return false
}

func GetBuiltin(name string) *BuiltinFn {
	for _, b := range Builtins {
		if name == b.Name {
			return b
		}
	}
	return nil
}

func GetBuiltinIndex(name string) int {
	for index, b := range Builtins {
		if name == b.Name {
			return index
		}
	}
	return -1
}
