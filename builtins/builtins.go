package builtins

import (
	"fmt"
	"proto/ast"
	"proto/lexer"
	"proto/runtime"
	"proto/shared"
	"strings"
)

func Stringf(args ...runtime.RuntimeObj) runtime.RuntimeObj {
	str := args[0]
	locations := GetFormatPositions(str.String())
	if len(locations) != len(args)-1 {
		var msg strings.Builder
		msg.WriteString(fmt.Sprintf("%s format expected %d arguments but is called with %d.",
			"stringf", len(locations), len(args)-1))
		shared.ReportErrorAndExit("VM", msg.String())
	}

	res := ""
	if len(locations) != 0 {
		start := 0
		for _, coord := range locations {
			res += str.String()[start:coord[0]]

			switch actual := args[coord[2]+1].(type) {
			case *runtime.Char:
				res += actual.Character()
			case *runtime.String:
				res += actual.Content()
			default:
				// println(args[coord[2]+1].String())
				res += args[coord[2]+1].String()
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

func Printf(args ...runtime.RuntimeObj) runtime.RuntimeObj {
	str := args[0]
	locations := GetFormatPositions(str.String())
	if len(locations) != len(args)-1 {
		var msg strings.Builder
		msg.WriteString(fmt.Sprintf("%s format expected %d arguments but is called with %d.",
			"printf", len(locations), len(args)-1))
		shared.ReportErrorAndExit("VM", msg.String())
	}

	res := ""
	if len(locations) != 0 {
		start := 0
		for _, coord := range locations {
			res += str.String()[start:coord[0]]

			switch actual := args[coord[2]+1].(type) {
			case *runtime.Char:
				res += actual.Character()
			case *runtime.String:
				res += actual.Content()
			default:
				res += args[coord[2]+1].String()
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

func Print(args ...runtime.RuntimeObj) runtime.RuntimeObj {
	if len(args) == 0 {
		print()
	} else {
		var out strings.Builder

		for index, arg := range args {
			switch actual := arg.(type) {
			case *runtime.String:
				out.WriteString(actual.Content())
			case *runtime.Char:
				out.WriteString(actual.Character())
			default:
				out.WriteString(arg.String())
			}
			if index+1 < len(args) {
				out.WriteString(" ")
			}
		}
		print(out.String())
	}
	return &runtime.Unit{}
}

func Println(args ...runtime.RuntimeObj) runtime.RuntimeObj {
	if len(args) == 0 {
		println()
	} else {
		var out strings.Builder

		for index, arg := range args {
			switch actual := arg.(type) {
			case *runtime.String:
				out.WriteString(actual.Content())
			case *runtime.Char:
				out.WriteString(actual.Character())
			default:
				out.WriteString(arg.String())
			}
			if index+1 < len(args) {
				out.WriteString(" ")
			}
		}
		println(out.String())
	}
	return &runtime.Unit{}
}

func Len(args ...runtime.RuntimeObj) runtime.RuntimeObj {
	arr := args[0]
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
			msg.WriteString(fmt.Sprintf("%s expects an Array or a string, but was called with %s.",
				fn.Name, actual.TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			return true
		}
	default:
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects an Array or a string, but was called with %s.",
			fn.Name, actual.TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}
	return false
}

func Append(args ...runtime.RuntimeObj) runtime.RuntimeObj {
	return &runtime.Unit{}
}

func TypeCheckAppend(fn *BuiltinFn, call *ast.CallExpression) bool {
	args := call.Arguments

	if len(args) != 2 {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s typed function expects %d arguments but got called with %d arguments.",
			fn.LiteralRepr(), len(fn.Params), len(call.Arguments)))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	if array, ok := args[0].Type().(*ast.Proto_Array); ok {
		item := args[1]
		if array.InternalType.TypeSignature() != item.Type().TypeSignature() {
			var msg strings.Builder
			line := call.Start.TokenSpan.Line
			col := call.Start.TokenSpan.Col
			msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
			msg.WriteString(fmt.Sprintf("Cannot append item of type %s to an Array of %s.",
				item.Type().TypeSignature(), array.InternalType.TypeSignature()))
			shared.ReportErrorAndExit("TypeChecker", msg.String())
			return true
		}
	} else {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects an Array as its first argument but got %s.",
			fn.Name, args[0].Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}
	return false
}

func Char_To_Int(vals ...runtime.RuntimeObj) runtime.RuntimeObj {
	char := vals[0].(*runtime.Char)

	if len(char.Character()) == 0 {
		// empty string
		var msg strings.Builder
		msg.WriteString("char_to_int expects a non-empty char as its only argument but got ' '.")
		shared.ReportErrorAndExit("TypeChecker", msg.String())
	}
	num := &runtime.I64{
		Value: int64(char.Character()[0]),
	}
	return num
}

func TypeCheckCharToInt(fn *BuiltinFn, call *ast.CallExpression) bool {
	args := call.Arguments
	if len(args) != 1 {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s typed function expects %d arguments but got called with %d arguments.",
			fn.LiteralRepr(), len(fn.Params), len(call.Arguments)))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	if char, ok := args[0].Type().(*ast.Proto_Builtin); !ok || char.TypeSignature() != "char" {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects a char as its only argument but got %s.",
			fn.Name, args[0].Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}
	return false
}

func Int_To_Char(vals ...runtime.RuntimeObj) runtime.RuntimeObj {
	i64 := vals[0].(*runtime.I64)

	char := &runtime.Char{
		Value: string(rune(i64.Value)),
	}
	return char
}

func TypeCheckIntToChar(fn *BuiltinFn, call *ast.CallExpression) bool {
	args := call.Arguments
	if len(args) != 1 {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s typed function expects %d arguments but got called with %d arguments.",
			fn.LiteralRepr(), len(fn.Params), len(call.Arguments)))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	if i64, ok := args[0].Type().(*ast.Proto_Builtin); !ok || i64.TypeSignature() != "i64" {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects an i64 as its only argument but got %s.",
			fn.Name, args[0].Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}
	return false
}

func Range_Start(vals ...runtime.RuntimeObj) runtime.RuntimeObj {
	var start runtime.RuntimeObj
	if val, ok := vals[0].(*runtime.Range); ok {
		start = val.Start
	} else if val, ok := vals[0].(*runtime.InclusiveRange); ok {
		start = val.Start
	}
	return start
}

func Range_End(vals ...runtime.RuntimeObj) runtime.RuntimeObj {
	var start runtime.RuntimeObj
	if val, ok := vals[0].(*runtime.Range); ok {
		start = val.PastEnd
	} else if val, ok := vals[0].(*runtime.InclusiveRange); ok {
		start = val.End
	}
	return start
}

func Assert(vals ...runtime.RuntimeObj) runtime.RuntimeObj {
	return &runtime.Unit{}
}

func TypeCheckRange(fn *BuiltinFn, call *ast.CallExpression) bool {
	args := call.Arguments

	if len(args) != 1 {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s typed function expects %d arguments but got called with %d arguments.",
			fn.LiteralRepr(), len(fn.Params), len(call.Arguments)))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	if reg, ok := args[0].Type().(*ast.Proto_Range); !ok {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects an Range<i64> or Range<char> as its only argument but got %s.",
			fn.Name, args[0].Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	} else {
		fn.Returns = reg.InternalType
	}
	return false
}

func TypeCheckAssert(fn *BuiltinFn, call *ast.CallExpression) bool {
	args := call.Arguments

	if len(args) != 1 {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s typed function expects %d arguments but got called with %d arguments.",
			fn.LiteralRepr(), len(fn.Params), len(call.Arguments)))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	if boolean, ok := args[0].Type().(*ast.Proto_Builtin); !ok || boolean.TypeSignature() != "bool" {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects an bool as its first argument but got %s.",
			fn.Name, args[0].Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}
	return false
}

func TypeCheckAssertMessage(fn *BuiltinFn, call *ast.CallExpression) bool {
	args := call.Arguments

	if len(args) != 2 {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s typed function expects %d arguments but got called with %d arguments.",
			fn.LiteralRepr(), len(fn.Params), len(call.Arguments)))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	if boolean, ok := args[0].Type().(*ast.Proto_Builtin); !ok || boolean.TypeSignature() != "bool" {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects an bool as its first argument but got %s.",
			fn.Name, args[0].Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}

	if str, ok := args[1].Type().(*ast.Proto_Builtin); !ok || str.TypeSignature() != "str" {
		var msg strings.Builder
		line := call.Start.TokenSpan.Line
		col := call.Start.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString(fmt.Sprintf("%s expects a string as its second argument but got %s.",
			fn.Name, args[0].Type().TypeSignature()))
		shared.ReportErrorAndExit("TypeChecker", msg.String())
		return true
	}
	return false
}

type ProtoGoFn func(...runtime.RuntimeObj) runtime.RuntimeObj

type BuiltinFn struct {
	Fn             ProtoGoFn
	Params         []string
	Returns        ast.ProtoType
	Name           string
	HasTypeChecker bool
	TypeChecker    func(*BuiltinFn, *ast.CallExpression) bool
}

var Builtins = []*BuiltinFn{
	{
		Fn:     Stringf,
		Params: []string{"str", "...AnyProtoType"},
		Returns: &ast.Proto_Builtin{
			TypeToken: lexer.ProtoToken{
				Type:      lexer.STRING_TYPE,
				Literal:   "str",
				TokenSpan: lexer.Span{},
			},
		},
		Name:           "stringf",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckFormatString,
	},
	{
		Fn:             Printf,
		Params:         []string{"str", "...AnyProtoType"},
		Returns:        &ast.Proto_Unit{},
		Name:           "printf",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckFormatString,
	},
	{
		Fn:             Print,
		Params:         []string{"...AnyProtoType"},
		Returns:        &ast.Proto_Unit{},
		HasTypeChecker: false,
		Name:           "print",
	},
	{
		Fn:             Println,
		Params:         []string{"...AnyProtoType"},
		Returns:        &ast.Proto_Unit{},
		HasTypeChecker: false,
		Name:           "println",
	},
	{
		Fn:     Len,
		Params: []string{"str | Array of AnyProtoType"},
		Returns: &ast.Proto_Builtin{
			TypeToken: lexer.ProtoToken{
				Type:      lexer.I64_TYPE,
				Literal:   "i64",
				TokenSpan: lexer.Span{},
			},
		},
		Name:           "len",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckForArrayString,
	},
	{
		Fn:     Char_To_Int,
		Params: []string{"char"},
		Returns: &ast.Proto_Builtin{
			TypeToken: lexer.ProtoToken{
				Type:      lexer.I64_TYPE,
				Literal:   "i64",
				TokenSpan: lexer.Span{},
			},
		},
		Name:           "char_to_int",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckCharToInt,
	},
	{
		Fn:     Int_To_Char,
		Params: []string{"int"},
		Returns: &ast.Proto_Builtin{
			TypeToken: lexer.ProtoToken{
				Type:      lexer.CHAR_TYPE,
				Literal:   "char",
				TokenSpan: lexer.Span{},
			},
		},
		Name:           "int_to_char",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckIntToChar,
	},
	{
		Fn:     Range_Start,
		Params: []string{"Range<i64|char>"},
		Returns: &ast.Proto_Builtin{
			TypeToken: lexer.ProtoToken{
				Type:      lexer.CHAR_TYPE,
				Literal:   "char",
				TokenSpan: lexer.Span{},
			},
		},
		Name:           "range_start",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckRange,
	},
	{
		Fn:     Range_End,
		Params: []string{"Range<i64|char>"},
		Returns: &ast.Proto_Builtin{
			TypeToken: lexer.ProtoToken{
				Type:      lexer.CHAR_TYPE,
				Literal:   "char",
				TokenSpan: lexer.Span{},
			},
		},
		Name:           "range_end",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckRange,
	},
	{
		Fn:             Assert,
		Params:         []string{"bool"},
		Returns:        &ast.Proto_Unit{},
		Name:           "assert",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckAssert,
	},
	{
		Fn:             Assert,
		Params:         []string{"bool", "str"},
		Returns:        &ast.Proto_Unit{},
		Name:           "assertm",
		HasTypeChecker: true,
		TypeChecker:    TypeCheckAssertMessage,
	},
}

func (bf *BuiltinFn) LiteralRepr() string {
	return "fn " + bf.Name + "(" +
		strings.Join(bf.Params, ", ") +
		") -> " + bf.Returns.TypeSignature()
}

func (bf *BuiltinFn) AsCppCode(c *ast.CodeGenerator, use_tab bool, newline bool) {
	c.WriteLine(bf.LiteralRepr(), use_tab)
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
