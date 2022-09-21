package parser

import (
	"fmt"
	"path/filepath"
	"proto/ast"
	"proto/lexer"
	"proto/shared"
	"strings"
)

type Parser struct {
	lex     *lexer.Lexer
	cur     lexer.ProtoToken
	peek    lexer.ProtoToken
	index   int
	tokens  []lexer.ProtoToken
	file    string
	dir     string
	Program *ast.ProtoProgram
}

func NewWithLexer(file string, l *lexer.Lexer) *Parser {
	p := &Parser{
		lex:    l,
		index:  0,
		tokens: []lexer.ProtoToken{},
		file:   file,
	}

	token := l.Next_Token()
	for token.Type != lexer.END {
		if token.Type == lexer.ERROR {
			// lexer error
			var msg strings.Builder
			msg.WriteString(fmt.Sprintf("%d:%d ", token.TokenSpan.Line, token.TokenSpan.Col))
			msg.WriteString(token.Literal)
			shared.ReportErrorAndExit("Lexer", msg.String())
		}
		p.tokens = append(p.tokens, token)
		token = l.Next_Token()
	}
	p.tokens = append(p.tokens, token)
	p.nextToken() // initialize p.peek
	p.nextToken() // initialize p.cur and update p.peek
	return p
}

func New(dir, file, src string) *Parser {
	l := lexer.New(file, src)
	p := &Parser{
		lex:    l,
		index:  0,
		tokens: []lexer.ProtoToken{},
		file:   file,
		dir:    dir,
	}

	token := l.Next_Token()
	for token.Type != lexer.END {
		if token.Type == lexer.ERROR {
			// lexer error
			var msg strings.Builder
			msg.WriteString(fmt.Sprintf("%d:%d ", token.TokenSpan.Line, token.TokenSpan.Col))
			msg.WriteString(token.Literal)
			shared.ReportErrorAndExit("Lexer", msg.String())
		}
		p.tokens = append(p.tokens, token)
		token = l.Next_Token()
	}
	p.tokens = append(p.tokens, token)
	p.nextToken() // initialize p.peek
	p.nextToken() // initialize p.cur and update p.peek
	return p
}

func (p *Parser) nextToken() {
	if p.index < len(p.tokens) {
		p.cur = p.peek
		p.peek = p.tokens[p.index]
		p.index++
	} else {
		p.cur = p.peek
		p.peek = p.tokens[len(p.tokens)-1]
	}
}

func (p *Parser) consume(expected lexer.TokenType) {
	if p.cur.Type == expected {
		p.nextToken() // move to next token
	} else {
		var msg strings.Builder
		msg.WriteString(fmt.Sprintf("%s %d:%d Expected token of type ", p.file, p.cur.TokenSpan.Line, p.cur.TokenSpan.Col))
		msg.WriteString(string(expected) + " but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
}

func (p *Parser) parse_identifier() *ast.Identifier {
	var ident *ast.Identifier
	switch p.cur.Type {
	case lexer.IDENT:
		id := p.cur
		p.consume(id.Type)
		ident = &ast.Identifier{
			Token:   id,
			Id_Type: &ast.Proto_Untyped{},
		}
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected an identifier but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return ident
}

func (p *Parser) parse_general_block(allow_end_expr bool) ast.ProtoNode {
	start := p.cur
	p.consume(lexer.OPEN_CURLY)
	var contents []ast.ProtoNode
	is_expr_block := false
	for p.cur.Type != lexer.CLOSE_CURLY {
		n_start := p.cur.TokenSpan
		node := p.parse_protonode(allow_end_expr)
		if _, ok := node.(ast.Expression); ok {
			if p.cur.Type != lexer.CLOSE_CURLY {
				if allow_end_expr {
					// if this is not the last node in the block, disallow an expression
					var msg strings.Builder
					msg.WriteString(fmt.Sprint(n_start.Line) + ":" + fmt.Sprint(n_start.Col))
					msg.WriteString(" Expressions are only allowed as the last line of the block.")
					shared.ReportErrorAndExit("Parser", msg.String())
				} else {
					var msg strings.Builder
					msg.WriteString(fmt.Sprint(n_start.Line) + ":" + fmt.Sprint(n_start.Col))
					msg.WriteString(" Expressions are not allowed in statement block.")
					shared.ReportErrorAndExit("Parser", msg.String())
				}
			} else if allow_end_expr {
				is_expr_block = true
				contents = append(contents, node)
			} else {
				var msg strings.Builder
				msg.WriteString(fmt.Sprint(n_start.Line) + ":" + fmt.Sprint(n_start.Col))
				msg.WriteString(" Expressions are not allowed in statement block.")
				shared.ReportErrorAndExit("Parser", msg.String())
			}
		} else {
			contents = append(contents, node)
		}
	}
	p.consume(lexer.CLOSE_CURLY)

	if allow_end_expr && is_expr_block {
		return &ast.BlockExpr{
			Start:     start,
			Contents:  contents,
			BlockType: nil,
		}
	} else {
		return &ast.BlockStmt{
			Start:    start,
			Contents: contents,
		}
	}
}

func (p *Parser) parse_block_expr() *ast.BlockExpr {
	start := p.cur
	p.consume(lexer.OPEN_CURLY)
	var contents []ast.ProtoNode
	for p.cur.Type != lexer.CLOSE_CURLY {
		n_start := p.cur.TokenSpan
		node := p.parse_protonode(true)
		if _, ok := node.(ast.Expression); ok {
			if p.cur.Type != lexer.CLOSE_CURLY {
				// if this is not the last node in the block, disallow an expression
				var msg strings.Builder
				msg.WriteString(fmt.Sprint(n_start.Line) + ":" + fmt.Sprint(n_start.Col))
				msg.WriteString(" Expressions are only allowed as the last line of the block.")
				shared.ReportErrorAndExit("Parser", msg.String())
			} else {
				contents = append(contents, node)
			}
		} else {
			contents = append(contents, node)
		}
	}
	p.consume(lexer.CLOSE_CURLY)

	return &ast.BlockExpr{
		Start:     start,
		Contents:  contents,
		BlockType: nil,
	}
}

func (p *Parser) parse_block_stmt() *ast.BlockStmt {
	start := p.cur
	p.consume(lexer.OPEN_CURLY)
	modules := []*ast.Module{}
	funcs := []*ast.FunctionDef{}
	var_decls := []*ast.VariableDecl{}
	structs := []*ast.Struct{}

	var contents []ast.ProtoNode
	for p.cur.Type != lexer.CLOSE_CURLY {
		n_start := p.cur.TokenSpan
		node := p.parse_protonode(false)
		switch actual := node.(type) {
		case ast.Expression:
			// if this is not the last node in the block, disallow an expression
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(n_start.Line) + ":" + fmt.Sprint(n_start.Col))
			msg.WriteString(" Expressions are not allowed in a statement block.")
			shared.ReportErrorAndExit("Parser", msg.String())
		case *ast.Module:
			modules = append(modules, actual)
		case *ast.FunctionDef:
			funcs = append(funcs, actual)
		case *ast.VariableDecl:
			var_decls = append(var_decls, actual)
		case *ast.Struct:
			structs = append(structs, actual)
		}
		contents = append(contents, node)
	}
	end := p.cur
	p.consume(lexer.CLOSE_CURLY)

	return &ast.BlockStmt{
		Start:         start,
		End:           end,
		Contents:      contents,
		Modules:       modules,
		Functions:     funcs,
		VariableDecls: var_decls,
		Structs:       structs,
	}
}

func (p *Parser) parse_general_if(allow_general_block bool) ast.ProtoNode {
	start := p.cur
	p.consume(start.Type)

	condition := p.parse_expr(true)

	is_expr_if := false

	then_body := p.parse_general_block(allow_general_block)
	if _, ok := then_body.(*ast.BlockExpr); ok {
		is_expr_if = true
	}

	var elsebody ast.ProtoNode = nil

	if p.cur.Type == lexer.ELSE {
		p.consume(p.cur.Type)

		if p.cur.Type == lexer.IF {
			elsebody = p.parse_general_if(is_expr_if)
		} else {
			elsebody = p.parse_general_block(is_expr_if)
		}
	}

	if is_expr_if {
		res := &ast.IfExpr{
			Start:     start,
			Condition: condition,
			ThenBody:  then_body.(*ast.BlockExpr),
			ElseBody:  elsebody,
			IfType:    nil,
		}
		return res
	} else {
		res := &ast.IfStmt{
			Start:     start,
			Condition: condition,
			ThenBody:  then_body.(*ast.BlockStmt),
			ElseBody:  elsebody,
		}
		return res
	}
}

func (p *Parser) parse_if_expr() *ast.IfExpr {
	start := p.cur
	p.consume(p.cur.Type)

	condition := p.parse_expr(true)

	thenbody := p.parse_block_expr()

	var elsebody ast.Expression = nil
	if p.cur.Type == lexer.ELSE {
		p.consume(p.cur.Type)

		if p.cur.Type == lexer.IF {
			elsebody = p.parse_if_expr()
		} else {
			elsebody = p.parse_block_expr()
		}
	} else {
		var msg strings.Builder
		line := p.cur.TokenSpan.Line
		col := p.cur.TokenSpan.Col
		msg.WriteString(fmt.Sprintf("%d:%d ", line, col))
		msg.WriteString("If conditional expression requires an else statement.")
		shared.ReportErrorAndExit("Parser", msg.String())
	}

	return &ast.IfExpr{
		Start:     start,
		Condition: condition,
		ThenBody:  thenbody,
		ElseBody:  elsebody,
		IfType:    nil,
	}
}

func (p *Parser) parse_primary(skip_struct_expr bool) ast.Expression {
	var val ast.Expression
	switch p.cur.Type {
	case lexer.I64:
		i64 := p.cur
		p.consume(i64.Type)
		val = &ast.I64{
			Token: i64,
		}
	case lexer.CHAR:
		char := p.cur
		p.consume(char.Type)
		val = &ast.Char{
			Token: char,
		}
	case lexer.STRING:
		str := p.cur
		p.consume(str.Type)
		val = &ast.String{
			Token: str,
		}
	case lexer.TRUE, lexer.FALSE:
		primary := p.cur
		p.consume(primary.Type)
		boolean := &ast.Boolean{
			Token: primary,
		}

		if primary.Type == lexer.TRUE {
			boolean.Value = true
		} else {
			boolean.Value = false
		}
		val = boolean
	case lexer.IDENT:
		struct_name := p.parse_identifier()
		if p.cur.Type == lexer.OPEN_CURLY && !skip_struct_expr {
			start := p.cur
			p.consume(p.cur.Type)

			fields := make(map[*ast.Identifier]ast.Expression)
			for p.cur.Type != lexer.CLOSE_CURLY && p.cur.Type != lexer.END {
				field := p.parse_identifier()
				if p.cur.Type != lexer.COLON {
					fields[field] = field
				} else {
					// if we have colon, consume it
					p.consume(p.cur.Type)
					init := p.parse_expr(false)
					fields[field] = init
				}

				if p.cur.Type != lexer.CLOSE_CURLY {
					p.consume(lexer.COMMA)
				}
			}
			p.consume(lexer.CLOSE_CURLY)
			val = &ast.StructInitialization{
				Start:      start,
				StructName: struct_name,
				Fields:     fields,
				StructType: &ast.Proto_UserDef{
					Name: struct_name,
					Definition: &ast.Struct{
						Start:   start,
						Name:    *struct_name,
						Members: []*ast.Identifier{},
					},
				},
			}
		} else {
			val = struct_name
		}
	case lexer.OPEN_PAREN:
		start := p.cur
		p.consume(p.cur.Type)
		if p.cur.Type == lexer.CLOSE_PAREN { // parse a unit expression
			p.consume(p.cur.Type)
			val = &ast.Unit{
				Token: start,
			}
		} else {
			val = p.parse_expr(false)
			if p.cur.Type == lexer.COMMA { // we will parse a tuple instead
				p.consume(p.cur.Type)
				var items []ast.Expression
				var types []ast.ProtoType
				items = append(items, val)
				types = append(types, val.Type())
				for p.cur.Type != lexer.END && p.cur.Type != lexer.CLOSE_PAREN {
					expr := p.parse_expr(false)
					items = append(items, expr)
					types = append(types, expr.Type())
					if p.cur.Type != lexer.CLOSE_PAREN {
						p.consume(lexer.COMMA)
					}
				}
				p.consume(lexer.CLOSE_PAREN)
				val = &ast.Tuple{
					Items: items,
					Token: start,
					TupleType: &ast.Proto_Tuple{
						InternalTypes: types,
					},
				}
			} else {
				p.consume(lexer.CLOSE_PAREN)
			}
		}
	case lexer.OPEN_BRACKET:
		start := p.cur
		p.consume(p.cur.Type)
		var items []ast.Expression
		var arr_type ast.ProtoType = nil
		tried_annotation := false
		index := p.index - 2 // track current token for later use
		var expr ast.Expression = nil
		for p.cur.Type != lexer.END && p.cur.Type != lexer.CLOSE_BRACKET {
			if arr_type == nil && !tried_annotation {
				// see if array is type annotated
				potential := p.parse_type(true, false)
				tried_annotation = true
				if potential != nil {
					if p.cur.Type != lexer.SEMI_COLON {
						p.cur = p.tokens[index]
						p.peek = p.tokens[index+1]
						p.index = index + 2
						continue
					}
					arr_type = potential
					p.consume(lexer.SEMI_COLON)
					continue
				} else {
					p.cur = p.tokens[index]
					p.peek = p.tokens[index+1]
					p.index = index + 2
					continue
				}
			}

			if expr == nil {
				expr = p.parse_expr(false)
			}
			items = append(items, expr)

			if arr_type == nil {
				// first pass
				arr_type = expr.Type()
			} else if arr_type.TypeSignature() != "untyped" &&
				expr.Type().TypeSignature() != "untyped" &&
				expr.Type().TypeSignature() != arr_type.TypeSignature() {
				// an array with inconsistent typing
				arr_type = &ast.Proto_Untyped{}
			}

			if p.cur.Type != lexer.CLOSE_BRACKET {
				p.consume(lexer.COMMA)
			}
			expr = nil
		}
		p.consume(lexer.CLOSE_BRACKET)

		arr := &ast.Array{
			Items: items,
			Token: start,
			ArrayType: &ast.Proto_Array{
				InternalType: &ast.Proto_EmptyArray{},
			},
		}
		if arr_type != nil {
			arr.ArrayType.InternalType = arr_type
		}
		val = arr
	case lexer.OPEN_CURLY:
		val = p.parse_block_expr()
	case lexer.IF:
		val = p.parse_if_expr()
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected an expression but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return val
}

func (p *Parser) parse_call_expression(skip_struct_expr bool) ast.Expression {
	call := p.parse_primary(skip_struct_expr)

	for {
		if p.cur.Type == lexer.OPEN_PAREN {
			start := p.cur
			p.consume(p.cur.Type)
			var args []ast.Expression
			for p.cur.Type != lexer.END && p.cur.Type != lexer.CLOSE_PAREN {
				if len(args) > 255 {
					var msg strings.Builder
					msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
					msg.WriteString(" Function Calls only allows 255 arguments.")
					shared.ReportErrorAndExit("Parser", msg.String())
				}
				args = append(args, p.parse_expr(false))
				if p.cur.Type != lexer.CLOSE_PAREN {
					p.consume(lexer.COMMA)
				}
			}

			p.consume(lexer.CLOSE_PAREN)
			call = &ast.CallExpression{
				Start:      start,
				Callable:   call,
				Arguments:  args,
				ReturnType: &ast.Proto_Untyped{},
			}
		} else if p.cur.Type == lexer.OPEN_BRACKET {
			start := p.cur
			p.consume(p.cur.Type)

			index := p.parse_expr(false)
			p.consume(lexer.CLOSE_BRACKET)

			call = &ast.IndexExpression{
				Start:     start,
				Indexable: call,
				Index:     index,
				ValueType: &ast.Proto_Untyped{},
			}
		} else if p.cur.Type == lexer.DOT {
			start := p.cur
			p.consume(p.cur.Type)

			if p.cur.Type == lexer.IDENT || p.cur.Type == lexer.I64 {
				member := p.parse_primary(true)

				call = &ast.Membership{
					Start:          start,
					Object:         call,
					Member:         member,
					MembershipType: &ast.Proto_Untyped{},
				}
			} else {
				var msg strings.Builder
				msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
				msg.WriteString(" Expected token of type ")
				msg.WriteString(string(lexer.IDENT) + " or " + string(lexer.I64) + " but found ")
				msg.WriteString(string(p.cur.Type) + ".")
				shared.ReportErrorAndExit("Parser", msg.String())
			}
		} else if p.cur.Type == lexer.PATH_SEP {
			start := p.cur
			p.consume(p.cur.Type)

			if p.cur.Type != lexer.IDENT {
				var msg strings.Builder
				msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
				msg.WriteString(" Expected token of type ")
				msg.WriteString(string(lexer.IDENT) + " or " + string(lexer.I64) + " but found ")
				msg.WriteString(string(p.cur.Type) + ".")
				shared.ReportErrorAndExit("Parser", msg.String())
			}
			call = &ast.ModuleAccess{
				Start:      start,
				Mod:        call,
				Member:     p.parse_primary(true),
				MemberType: &ast.Proto_Untyped{},
			}
		} else {
			break
		}
	}

	return call
}

func (p *Parser) parse_unary(skip_struct_expr bool) ast.Expression {
	var val ast.Expression
	switch p.cur.Type {
	case lexer.NOT, lexer.MINUS:
		operator := p.cur
		p.consume(operator.Type)
		operand := p.parse_unary(skip_struct_expr)
		val = &ast.UnaryOp{
			Operand:  operand,
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	case lexer.REF:
		operator := p.cur
		p.consume(operator.Type)
		line := p.cur.TokenSpan.Line
		col := p.cur.TokenSpan.Col
		operand := p.parse_call_expression(false)
		// check that the reference is to something valid
		switch operand.(type) {
		case *ast.CallExpression, *ast.IndexExpression, *ast.Membership, *ast.Identifier,
			*ast.Tuple, *ast.Array:
		default:
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(line) + ":" + fmt.Sprint(col))
			msg.WriteString(" Expected a referencable value but found ")
			msg.WriteString(operand.LiteralRepr() + ".")
			shared.ReportErrorAndExit("Parser", msg.String())
		}
		val = &ast.Reference{
			Start: operator,
			RefType: &ast.Proto_Reference{
				Inner: operand.Type(),
			},
			Value: operand,
		}
	case lexer.STAR:
		operator := p.cur
		p.consume(operator.Type)
		operand := p.parse_call_expression(false)
		val = &ast.Dereference{
			Start:     operator,
			Value:     operand,
			DerefType: &ast.Proto_Untyped{},
		}
	default:
		val = p.parse_call_expression(skip_struct_expr)
	}
	return val
}

func (p *Parser) parse_factor(skip_struct_expr bool) ast.Expression {
	factor := p.parse_unary(skip_struct_expr)

	for p.cur.Type == lexer.STAR || p.cur.Type == lexer.SLASH ||
		p.cur.Type == lexer.MODULO {
		operator := p.cur
		p.consume(operator.Type)
		factor = &ast.BinaryOp{
			Left:     factor,
			Right:    p.parse_unary(skip_struct_expr),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return factor
}

func (p *Parser) parse_term(skip_struct_expr bool) ast.Expression {
	term := p.parse_factor(skip_struct_expr)

	for p.cur.Type == lexer.PLUS || p.cur.Type == lexer.MINUS {
		operator := p.cur
		p.consume(operator.Type)
		term = &ast.BinaryOp{
			Left:     term,
			Right:    p.parse_factor(skip_struct_expr),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return term
}

func (p *Parser) parse_comparison(skip_struct_expr bool) ast.Expression {
	comp := p.parse_term(skip_struct_expr)

	for p.cur.Type == lexer.GREATER_THAN || p.cur.Type == lexer.GREATER_OR_EQUAL ||
		p.cur.Type == lexer.LESS_THAN || p.cur.Type == lexer.LESS_OR_EQUAL {
		operator := p.cur
		p.consume(operator.Type)
		comp = &ast.BinaryOp{
			Left:     comp,
			Right:    p.parse_term(skip_struct_expr),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return comp
}

func (p *Parser) parse_equality(skip_struct_expr bool) ast.Expression {
	eq := p.parse_comparison(skip_struct_expr)

	for p.cur.Type == lexer.IS_EQUAL_TO || p.cur.Type == lexer.NOT_EQUAL_TO {
		operator := p.cur
		p.consume(operator.Type)
		eq = &ast.BinaryOp{
			Left:     eq,
			Right:    p.parse_comparison(skip_struct_expr),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return eq
}

func (p *Parser) parse_and(skip_struct_expr bool) ast.Expression {
	and := p.parse_equality(skip_struct_expr)

	for p.cur.Type == lexer.AND {
		operator := p.cur
		p.consume(operator.Type)
		and = &ast.BinaryOp{
			Left:     and,
			Right:    p.parse_equality(skip_struct_expr),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return and
}

func (p *Parser) parse_or(skip_struct_expr bool) ast.Expression {
	or := p.parse_and(skip_struct_expr)

	for p.cur.Type == lexer.OR {
		operator := p.cur
		p.consume(operator.Type)
		or = &ast.BinaryOp{
			Left:     or,
			Right:    p.parse_and(skip_struct_expr),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return or
}

func (p *Parser) parse_range(skip_struct_expr bool) ast.Expression {
	range_start := p.parse_or(skip_struct_expr)

	if p.cur.Type == lexer.RANGE ||
		p.cur.Type == lexer.INCLUSIVE_RANGE {
		operator := p.cur
		p.consume(p.cur.Type)

		end := p.parse_or(skip_struct_expr)

		var range_type ast.ProtoType = &ast.Proto_Untyped{}
		if range_start.Type().TypeSignature() == end.Type().TypeSignature() &&
			!strings.Contains(range_start.Type().TypeSignature(), "untyped") {
			range_type = range_start.Type()
		}

		if operator.Type == lexer.RANGE {
			range_start = &ast.Range{
				Start:    range_start,
				PastEnd:  end,
				Operator: operator,
				RangeType: &ast.Proto_Range{
					InternalType: range_type,
				},
			}
		} else {
			range_start = &ast.InclusiveRange{
				Start:    range_start,
				End:      end,
				Operator: operator,
				RangeType: &ast.Proto_Range{
					InternalType: range_type,
				},
			}
		}
	}

	return range_start
}

func (p *Parser) parse_expr(skip_struct_expr bool) ast.Expression {
	return p.parse_range(skip_struct_expr)
}

func (p *Parser) parse_assignment(check_for_semi bool, skip_struct_expr bool) ast.ProtoNode {
	target := p.parse_expr(skip_struct_expr)

	if p.cur.Type == lexer.ASSIGN ||
		p.cur.Type == lexer.PLUS_EQUAL ||
		p.cur.Type == lexer.MINUS_EQUAL ||
		p.cur.Type == lexer.STAR_EQUAL ||
		p.cur.Type == lexer.SLASH_EQUAL ||
		p.cur.Type == lexer.MODULO_EQUAL {
		operator := p.cur
		p.consume(p.cur.Type)
		assigned := p.parse_expr(false)
		assignment := &ast.Assignment{
			Target:          target,
			AssignmentToken: operator,
			Assigned:        assigned,
		}

		if check_for_semi {
			assignment.HasSemiColon = true
			p.consume(lexer.SEMI_COLON)
		} else {
			assignment.HasSemiColon = false
		}
		return assignment
	}

	return target
}

func (p *Parser) parse_type(try bool, allow_variad bool) ast.ProtoType {
	var proto_type ast.ProtoType

	switch p.cur.Type {
	case lexer.I64_TYPE, lexer.CHAR_TYPE, lexer.BOOL_TYPE, lexer.STRING_TYPE:
		proto_type = &ast.Proto_Builtin{
			TypeToken: p.cur,
		}
		p.consume(p.cur.Type)
	case lexer.REF:
		p.consume(p.cur.Type)
		line := p.cur.TokenSpan.Line
		col := p.cur.TokenSpan.Col
		internal := p.parse_type(try, false)
		if internal == nil {
			return internal
		}
		switch internal.(type) {
		case *ast.Proto_Reference:
			// disallow a reference of a reference
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(line) + ":" + fmt.Sprint(col))
			msg.WriteString(" References of references are not allowed.")
			shared.ReportErrorAndExit("Parser", msg.String())
		default:
			proto_type = &ast.Proto_Reference{
				Inner: internal,
			}
		}
	case lexer.OPEN_BRACKET: // we are dealing with an array
		p.consume(p.cur.Type)
		internal := p.parse_type(try, false)
		if internal == nil {
			return internal
		}
		p.consume(lexer.CLOSE_BRACKET)
		proto_type = &ast.Proto_Array{
			InternalType: internal,
		}
	case lexer.RANGE_TYPE:
		p.consume(p.cur.Type)
		p.consume(lexer.LESS_THAN)
		start := p.cur
		inner_type := p.parse_type(try, false)
		switch actual := inner_type.(type) {
		case *ast.Proto_Builtin:
			if actual.TypeToken.Literal != "i64" && actual.TypeToken.Literal != "char" {
				var msg strings.Builder
				msg.WriteString(fmt.Sprint(start.TokenSpan.Line) + ":" + fmt.Sprint(start.TokenSpan.Col))
				msg.WriteString(" Expected Range type signature to contain i64 or char type but got ")
				msg.WriteString(inner_type.TypeSignature() + ".")
				shared.ReportErrorAndExit("Parser", msg.String())
			}
		default:
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(start.TokenSpan.Line) + ":" + fmt.Sprint(start.TokenSpan.Col))
			msg.WriteString(" Expected Range type signature to contain i64 or char type but got ")
			msg.WriteString(inner_type.TypeSignature() + ".")
			shared.ReportErrorAndExit("Parser", msg.String())
		}
		p.consume(lexer.GREATER_THAN)
		proto_type = &ast.Proto_Range{
			InternalType: inner_type,
		}
	case lexer.VARIAD:
		start := p.cur
		if allow_variad {
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(start.TokenSpan.Line) + ":" + fmt.Sprint(start.TokenSpan.Col))
			msg.WriteString(" Variadic type is only allowed in function definition.")
			shared.ReportErrorAndExit("Parser", msg.String())
		}
		p.consume(start.Type)
		// actual_type := p.parse_type(true, false)
		// if actual_type == nil {
		// 	// we have an any type
		// } else {
		// 	// we have a variadic type allowing 0 or more of actual_type
		// }
	case lexer.OPEN_PAREN:
		p.consume(p.cur.Type)
		var contained []ast.ProtoType
		for p.cur.Type != lexer.CLOSE_PAREN && p.cur.Type != lexer.END {
			item := p.parse_type(try, false)
			if item == nil {
				return item
			}
			contained = append(contained, item)
			if p.cur.Type != lexer.CLOSE_PAREN {
				p.consume(lexer.COMMA)
			}
		}
		p.consume(lexer.CLOSE_PAREN)
		if len(contained) == 0 {
			proto_type = &ast.Proto_Unit{}
		} else {
			proto_type = &ast.Proto_Tuple{
				InternalTypes: contained,
			}
		}
	case lexer.IDENT:
		token := p.parse_identifier()
		proto_type = &ast.Proto_UserDef{
			Name:       token,
			Definition: &ast.Struct{},
		}
	case lexer.FN:
		p.consume(p.cur.Type)
		tuple_type := p.parse_type(try, allow_variad)

		switch actual := tuple_type.(type) {
		case *ast.Proto_Tuple:
			var return_type ast.ProtoType = nil

			if p.cur.Type == lexer.ARROW {
				p.consume(lexer.ARROW)
				return_type = p.parse_type(try, false)
			} else {
				return_type = &ast.Proto_Unit{}
			}
			proto_type = &ast.Proto_Function{
				Params: actual,
				Return: return_type,
				Fn:     &ast.FunctionDef{},
			}
		case *ast.Proto_Unit:
			var return_type ast.ProtoType = nil

			if p.cur.Type == lexer.ARROW {
				p.consume(lexer.ARROW)
				return_type = p.parse_type(try, false)
			} else {
				return_type = &ast.Proto_Unit{}
			}
			proto_type = &ast.Proto_Function{
				Params: &ast.Proto_Tuple{
					InternalTypes: []ast.ProtoType{},
				},
				Return: return_type,
				Fn:     &ast.FunctionDef{},
			}
		default:
			if !try {
				var msg strings.Builder
				msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
				msg.WriteString(" Expected function type signature to contain tuple of parameter types but found ")
				msg.WriteString(actual.TypeSignature() + ".")
				shared.ReportErrorAndExit("Parser", msg.String())
			} else {
				return nil
			}
		}
	default:
		if !try {
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
			msg.WriteString(" Expected type signature but found ")
			msg.WriteString(string(p.cur.Type) + ".")
			shared.ReportErrorAndExit("Parser", msg.String())
		} else {
			return nil
		}
	}
	return proto_type
}

func (p *Parser) parse_let_mut() *ast.VariableDecl {
	keyword := p.cur.Type
	p.consume(p.cur.Type)

	ident := p.parse_identifier()

	var var_type ast.ProtoType = &ast.Proto_Untyped{}
	if p.cur.Type == lexer.COLON {
		p.consume(p.cur.Type)
		// handle parsing type
		var_type = p.parse_type(false, false)
	}

	var assigned_expr ast.Expression = nil
	if p.cur.Type == lexer.ASSIGN {
		p.consume(lexer.ASSIGN)
		assigned_expr = p.parse_expr(false)
	}

	p.consume(lexer.SEMI_COLON)

	// if variable is not typed or initialized, throw an error
	if var_type.TypeSignature() == "untyped" && assigned_expr == nil {
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(ident.Token.TokenSpan.Line) + ":" + fmt.Sprint(ident.Token.TokenSpan.Col))
		msg.WriteString(fmt.Sprintf(" Expected '%s' to be typed or initialized.", ident.LiteralRepr()))
		shared.ReportErrorAndExit("Parser", msg.String())
	}

	declaration := &ast.VariableDecl{
		Assignee: *ident,
		Assigned: assigned_expr,
		Mutable:  false,
		VarType:  var_type,
	}

	if keyword == lexer.MUT {
		declaration.Mutable = true
	}

	return declaration
}

func (p *Parser) parse_struct() *ast.Struct {
	start := p.cur
	p.consume(p.cur.Type)

	struct_name := p.parse_identifier()
	p.consume(lexer.OPEN_CURLY)

	var members []*ast.Identifier

	for p.cur.Type != lexer.END && p.cur.Type != lexer.CLOSE_CURLY {
		mem := p.parse_identifier()
		p.consume(lexer.COLON)
		mem_type := p.parse_type(false, false)
		mem.Id_Type = mem_type
		members = append(members, mem)
		if p.cur.Type != lexer.CLOSE_CURLY {
			p.consume(lexer.COMMA)
		}
	}

	p.consume(p.cur.Type)

	proto_struct := &ast.Struct{
		Start:   start,
		Name:    *struct_name,
		Members: members,
	}

	return proto_struct
}

func (p *Parser) parse_for_loop() ast.ProtoNode {
	var node ast.ProtoNode

	start := p.cur
	p.consume(p.cur.Type)

	if p.cur.Type == lexer.MUT {
		// we have a Generic for loop
		init := p.parse_let_mut()
		if init.Assigned == nil {
			// if loop variable is not initialized, throw error
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(init.Assignee.Token.TokenSpan.Line) + ":" + fmt.Sprint(init.Assignee.Token.TokenSpan.Col))
			msg.WriteString(fmt.Sprintf(" Expected '%s' to be initialized.", init.Assignee.LiteralRepr()))
			shared.ReportErrorAndExit("Parser", msg.String())
		}

		loop_condition := p.parse_expr(false)
		p.consume(lexer.SEMI_COLON)
		update := p.parse_assignment(false, true)
		body := p.parse_block_stmt()

		node = &ast.GenericForLoop{
			Start:         start,
			Init:          init,
			LoopCondition: loop_condition,
			Update:        update,
			Body:          body,
		}
	} else if p.cur.Type == lexer.LET {
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Loop Variables have to be mutable (change let to mut).")
		shared.ReportErrorAndExit("Parser", msg.String())
	} else if p.cur.Type == lexer.IDENT && p.peek.Type == lexer.IN {
		// we have a collections for loop
		loop_var := p.parse_identifier()
		p.consume(lexer.IN)
		collection := p.parse_expr(true)
		body := p.parse_block_stmt()

		node = &ast.CollectionsForLoop{
			Start:      start,
			LoopVar:    *loop_var,
			Collection: collection,
			Body:       body,
		}
	} else {
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Incorrectly formatted for loop.")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return node
}

func (p *Parser) parse_infinite_loop() *ast.InfiniteLoop {
	start := p.cur
	p.consume(p.cur.Type)

	body := p.parse_block_stmt()

	return &ast.InfiniteLoop{
		Start: start,
		Body:  body,
	}
}

func (p *Parser) parse_while_loop() *ast.WhileLoop {
	start := p.cur
	p.consume(p.cur.Type)

	loop_condition := p.parse_expr(true)
	body := p.parse_block_stmt()

	return &ast.WhileLoop{
		Start:         start,
		LoopCondition: loop_condition,
		Body:          body,
	}
}

func (p *Parser) parse_module() *ast.Module {
	start := p.cur
	p.consume(start.Type)

	name := p.parse_identifier()
	body := p.parse_block_stmt()
	mod := &ast.Module{
		Start: start,
		Body:  body,
		Name:  name,
	}

	return mod
}

func (p *Parser) parse_use_statement() *ast.UseStmt {
	use_stmt := &ast.UseStmt{
		Start: p.cur,
		Paths: []*ast.Path{},
	}
	p.consume(p.cur.Type)

	use_stmt.Paths = p.parse_paths()
	p.consume(lexer.SEMI_COLON)
	p.Program.Imports = append(p.Program.Imports, use_stmt)
	return use_stmt
}

func (p *Parser) parse_paths() []*ast.Path {
	paths := []*ast.Path{}

	start := p.cur
	path := &ast.Path{
		Start:  &start,
		Pieces: []ast.UsePath{},
	}

	switch p.cur.Type {
	case lexer.IDENT:
		for p.cur.Type == lexer.IDENT {
			id := p.parse_identifier()
			path.Pieces = append(path.Pieces, &ast.PathIDNode{
				Id: id,
			})
			if p.cur.Type == lexer.PATH_SEP && p.peek.Type == lexer.IDENT {
				p.consume(p.cur.Type)
			}
		}
		if p.cur.Type == lexer.AS {
			p.consume(p.cur.Type)
			as_id := p.parse_identifier()
			as_node := &ast.UseAs{
				As: as_id,
			}
			path.Pieces = append(path.Pieces, as_node)
			paths = append(paths, path)
			return paths
		}
		if p.cur.Type == lexer.PATH_SEP {
			p.consume(p.cur.Type)
			switch p.cur.Type {
			case lexer.STAR:
				path.Pieces = append(path.Pieces, &ast.PathIDNode{
					Id: &ast.Identifier{
						Token:   p.cur,
						Id_Type: nil,
					},
				})
				p.consume(p.cur.Type)
				paths = append(paths, path)
				return paths
			case lexer.OPEN_BRACKET:
				p.consume(p.cur.Type)
				for p.cur.Type != lexer.CLOSE_BRACKET && p.cur.Type != lexer.END {
					tree_path := p.parse_paths()
					for _, t_path := range tree_path {
						pieces := []ast.UsePath{}
						pieces = append(pieces, path.Pieces...)
						pieces = append(pieces, t_path.Pieces...)
						paths = append(paths, &ast.Path{
							Start:  &start,
							Pieces: pieces,
						})
					}
					if p.cur.Type == lexer.COMMA {
						p.consume(p.cur.Type)
					}
				}
				p.consume(lexer.CLOSE_BRACKET)
				return paths
			default:
				var msg strings.Builder
				msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
				msg.WriteString(" Unexpected token " + p.cur.Literal + ".")
				shared.ReportErrorAndExit("Parser", msg.String())
			}
		}
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected an identifier but got token " + p.cur.Literal + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	paths = append(paths, path)
	return paths
}

func (p *Parser) parse_function_definition() *ast.FunctionDef {
	start := p.cur
	p.consume(p.cur.Type)

	name := p.parse_identifier()
	var paramslist []*ast.Identifier

	p.consume(lexer.OPEN_PAREN)
	for p.cur.Type != lexer.CLOSE_PAREN {
		if len(paramslist) > 255 {
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
			msg.WriteString(" Function Definitions only allows 255 parameters.")
			shared.ReportErrorAndExit("Parser", msg.String())
		}
		ident := p.parse_identifier()
		p.consume(lexer.COLON)
		ident.Id_Type = p.parse_type(false, false)
		paramslist = append(paramslist, ident)
		if p.cur.Type != lexer.CLOSE_PAREN {
			p.consume(lexer.COMMA)
		}
	}
	p.consume(lexer.CLOSE_PAREN)

	// check for proper use of variadic type
	// only last parameter can use variadic type

	var return_type ast.ProtoType = &ast.Proto_Unit{}
	if p.cur.Type == lexer.ARROW {
		p.consume(p.cur.Type)
		return_type = p.parse_type(false, false)
	}
	body := p.parse_block_expr()

	fn_def := &ast.FunctionDef{
		Start:                 start,
		Name:                  name,
		ParameterList:         paramslist,
		ReturnType:            return_type,
		Body:                  body,
		IsMain:                false,
		FunctionTypeSignature: &ast.Proto_Function{},
	}
	type_params := &ast.Proto_Tuple{
		InternalTypes: []ast.ProtoType{},
	}
	for _, param := range paramslist {
		type_params.InternalTypes = append(type_params.InternalTypes, param.Id_Type)
	}
	function_type_sig := &ast.Proto_Function{
		Params: type_params,
		Return: return_type,
		Fn:     fn_def,
	}
	fn_def.FunctionTypeSignature = function_type_sig

	return fn_def
}

func (p *Parser) parse_cpp_literal() *ast.CppLiteral {
	literal := &ast.CppLiteral{
		Start: p.cur,
		Lines: []string{},
	}

	p.consume(lexer.CPP)
	p.consume(lexer.OPEN_CURLY)
	lines := []string{}
	line := ""
	start := p.cur
	for p.cur.Type != lexer.CLOSE_CURLY && p.cur.Type != lexer.END {
		if p.cur.TokenSpan.Line != start.TokenSpan.Line {
			start = p.cur
			lines = append(lines, line)
			line = p.cur.Literal
			p.consume(p.cur.Type)
			continue
		}
		line += p.cur.Literal
		p.consume(p.cur.Type)
	}
	lines = append(lines, line)
	p.consume(lexer.CLOSE_CURLY)
	literal.Lines = lines
	return literal
}

func (p *Parser) parse_protonode(allow_general_block bool) ast.ProtoNode {
	var node ast.ProtoNode
	switch p.cur.Type {
	case lexer.LET, lexer.MUT:
		node = p.parse_let_mut()
	case lexer.STRUCT:
		node = p.parse_struct()
	case lexer.FOR:
		node = p.parse_for_loop()
	case lexer.LOOP:
		node = p.parse_infinite_loop()
	case lexer.WHILE:
		node = p.parse_while_loop()
	case lexer.FN:
		node = p.parse_function_definition()
	case lexer.BREAK:
		node = &ast.Break{
			Token: p.cur,
		}
		p.consume(p.cur.Type)
		p.consume(lexer.SEMI_COLON)
	case lexer.RETURN:
		return_statement := &ast.Return{
			Token: p.cur,
		}
		p.consume(p.cur.Type)
		if p.cur.Type != lexer.SEMI_COLON {
			return_statement.Value = p.parse_expr(false)
		}
		p.consume(lexer.SEMI_COLON)
		node = return_statement
	case lexer.CONTINUE:
		node = &ast.Continue{
			Token: p.cur,
		}
		p.consume(p.cur.Type)
		p.consume(lexer.SEMI_COLON)
	case lexer.IF:
		node = p.parse_general_if(allow_general_block)
		if expr, ok := node.(*ast.IfExpr); ok && p.cur.Type == lexer.SEMI_COLON {
			node = &ast.PromotedExpr{
				Start: p.cur,
				Expr:  expr,
			}
			p.consume(p.cur.Type)
		}
	case lexer.CPP:
		node = p.parse_cpp_literal()
	case lexer.MOD:
		node = p.parse_module()
	case lexer.OPEN_CURLY:
		if allow_general_block {
			node = p.parse_general_block(allow_general_block)
			if expr, ok := node.(*ast.BlockExpr); ok && p.cur.Type == lexer.SEMI_COLON {
				node = &ast.PromotedExpr{
					Start: p.cur,
					Expr:  expr,
				}
				p.consume(p.cur.Type)
			}
		} else {
			node = p.parse_block_stmt()
		}
	case lexer.USE:
		node = p.parse_use_statement()
	default:
		potential_expr := p.parse_assignment(true, false)

		if actual, ok := potential_expr.(ast.Expression); ok {
			if p.cur.Type == lexer.SEMI_COLON { // allow semi-colon to promote expr to statement
				p.consume(p.cur.Type)
				node = &ast.PromotedExpr{
					Expr: actual,
				}
			} else {
				node = actual
			}
		} else {
			node = potential_expr
		}
	}

	return node
}

func Parse(file string, provide_main bool) *ast.ProtoProgram {
	src := shared.ReadFile(file)
	dir := filepath.Dir(shared.Get_abs_path("Parser", file))
	parser := New(dir, file, src)
	return Top_Level(parser, provide_main)
}

func Top_Level(p *Parser, provide_main bool) *ast.ProtoProgram {
	code := &ast.ProtoProgram{
		Start:         lexer.ProtoToken{},
		End:           lexer.ProtoToken{},
		Path:          "",
		Main:          &ast.FunctionDef{},
		Contents:      []ast.ProtoNode{},
		FunctionDefs:  []*ast.FunctionDef{},
		Structs:       []*ast.Struct{},
		Imports:       []*ast.UseStmt{},
		Modules:       []*ast.Module{},
		VariableDecls: []*ast.VariableDecl{},
	}

	p.Program = code
	has_main := false
	var main_def_loc lexer.Span

	start := p.cur
	for p.cur.Type != lexer.END {
		node := p.parse_protonode(false)
		switch actual := node.(type) {
		case *ast.FunctionDef:
			code.FunctionDefs = append(code.FunctionDefs, actual)
			if provide_main {
				if actual.Name.LiteralRepr() == "main" && !has_main {
					has_main = true
					actual.IsMain = true
					main_def_loc = actual.Name.Token.TokenSpan
					code.Main = actual
				} else if actual.Name.LiteralRepr() == "main" && has_main {
					name := actual.Name.Token
					line := name.TokenSpan.Line
					col := name.TokenSpan.Col
					var msg strings.Builder
					msg.WriteString(fmt.Sprintf("%d:%d", line, col))
					msg.WriteString(fmt.Sprintf(" Provided duplicate main function. First definition is found at %d:%d.",
						main_def_loc.Line, main_def_loc.Col))
					shared.ReportErrorAndExit("Parser", msg.String())
				}
			}
			code.Contents = append(code.Contents, node)
		case *ast.Struct:
			code.Structs = append(code.Structs, actual)
			code.Contents = append(code.Contents, actual)
		case *ast.Module:
			code.Modules = append(code.Modules, actual)
			code.Contents = append(code.Contents, actual)
		case *ast.VariableDecl:
			code.VariableDecls = append(code.VariableDecls, actual)
			code.Contents = append(code.Contents, actual)
		case ast.Expression:
			var msg strings.Builder
			msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
			msg.WriteString(" Expressions are not allowed in the global scope of the program.")
			shared.ReportErrorAndExit("Parser", msg.String())
		default:
			code.Contents = append(code.Contents, node)
		}
	}
	end := p.cur
	p.consume(lexer.END)

	if provide_main && !has_main {
		var msg strings.Builder
		msg.WriteString("Expected a main function to act as code entry point.")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	code.Path = p.file
	code.Start = start
	code.End = end
	return code
}
