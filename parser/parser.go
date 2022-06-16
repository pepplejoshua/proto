package parser

import (
	"fmt"
	"proto/ast"
	"proto/lexer"
	"proto/shared"
	"strings"
)

func New(src string) *Parser {
	l := lexer.New(src)
	p := &Parser{
		lex: l,
	}

	p.nextToken() // initialize p.peek
	p.nextToken() // initialize p.cur and update p.peek
	return p
}

type Parser struct {
	lex  *lexer.Lexer
	cur  lexer.ProtoToken
	peek lexer.ProtoToken
}

func (p *Parser) nextToken() {
	p.cur = p.peek
	p.peek = p.lex.Next_Token()
}

func (p *Parser) consume(expected lexer.TokenType) {
	if p.cur.Type == expected {
		p.nextToken() // move to next token
	} else {
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected token of type ")
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

func (p *Parser) parse_primary() ast.Expression {
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
		val = p.parse_identifier()
	case lexer.OPEN_PAREN:
		start := p.cur
		p.consume(p.cur.Type)
		val = p.parse_expr()
		if p.cur.Type == lexer.COMMA { // we will parse a tuple instead
			p.consume(p.cur.Type)
			var items []ast.Expression
			var types []ast.ProtoType
			items = append(items, val)
			types = append(types, val.Type())
			for p.cur.Type != lexer.END && p.cur.Type != lexer.CLOSE_PAREN {
				expr := p.parse_expr()
				items = append(items, expr)
				types = append(types, expr.Type())
				if p.cur.Type == lexer.COMMA {
					p.consume(p.cur.Type)
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
	case lexer.OPEN_BRACKET:
		start := p.cur
		p.consume(p.cur.Type)
		var items []ast.Expression
		for p.cur.Type != lexer.END && p.cur.Type != lexer.CLOSE_BRACKET {
			expr := p.parse_expr()
			items = append(items, expr)
			if p.cur.Type == lexer.COMMA {
				p.consume(p.cur.Type)
			}
		}
		p.consume(lexer.CLOSE_BRACKET)
		val = &ast.Array{
			Items: items,
			Token: start,
			ArrayType: &ast.Proto_Array{
				InternalType: &ast.Proto_Untyped{},
			},
		}
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected an expression but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}
	return val
}

func (p *Parser) parse_unary() ast.Expression {
	var val ast.Expression
	switch p.cur.Type {
	case lexer.NOT, lexer.MINUS:
		operator := p.cur
		p.consume(operator.Type)
		operand := p.parse_unary()
		val = &ast.UnaryOp{
			Operand:  operand,
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	default:
		val = p.parse_primary()
	}
	return val
}

func (p *Parser) parse_factor() ast.Expression {
	factor := p.parse_unary()

	for p.cur.Type == lexer.STAR || p.cur.Type == lexer.SLASH ||
		p.cur.Type == lexer.MODULO {
		operator := p.cur
		p.consume(operator.Type)
		factor = &ast.BinaryOp{
			Left:     factor,
			Right:    p.parse_unary(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return factor
}

func (p *Parser) parse_term() ast.Expression {
	term := p.parse_factor()

	for p.cur.Type == lexer.PLUS || p.cur.Type == lexer.MINUS {
		operator := p.cur
		p.consume(operator.Type)
		term = &ast.BinaryOp{
			Left:     term,
			Right:    p.parse_factor(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return term
}

func (p *Parser) parse_comparison() ast.Expression {
	comp := p.parse_term()

	for p.cur.Type == lexer.GREATER_THAN || p.cur.Type == lexer.GREATER_OR_EQUAL ||
		p.cur.Type == lexer.LESS_THAN || p.cur.Type == lexer.LESS_OR_EQUAL {
		operator := p.cur
		p.consume(operator.Type)
		comp = &ast.BinaryOp{
			Left:     comp,
			Right:    p.parse_term(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return comp
}

func (p *Parser) parse_equality() ast.Expression {
	eq := p.parse_comparison()

	for p.cur.Type == lexer.IS_EQUAL_TO || p.cur.Type == lexer.NOT_EQUAL_TO {
		operator := p.cur
		p.consume(operator.Type)
		eq = &ast.BinaryOp{
			Left:     eq,
			Right:    p.parse_comparison(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return eq
}

func (p *Parser) parse_and() ast.Expression {
	and := p.parse_equality()

	for p.cur.Type == lexer.AND {
		operator := p.cur
		p.consume(operator.Type)
		and = &ast.BinaryOp{
			Left:     and,
			Right:    p.parse_equality(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return and
}

func (p *Parser) parse_or() ast.Expression {
	or := p.parse_and()

	for p.cur.Type == lexer.OR {
		operator := p.cur
		p.consume(operator.Type)
		or = &ast.BinaryOp{
			Left:     or,
			Right:    p.parse_and(),
			Operator: operator,
			Op_Type:  &ast.Proto_Untyped{},
		}
	}
	return or
}

func (p *Parser) parse_expr() ast.Expression {
	return p.parse_or()
}

func (p *Parser) parse_type() ast.ProtoType {
	var proto_type ast.ProtoType

	switch p.cur.Type {
	case lexer.I64_TYPE, lexer.CHAR_TYPE, lexer.BOOL_TYPE, lexer.STRING_TYPE:
		proto_type = &ast.Proto_Builtin{
			TypeToken: p.cur,
		}
		p.consume(p.cur.Type)
	case lexer.OPEN_BRACKET: // we are dealing with an array
		p.consume(p.cur.Type)
		internal := p.parse_type()
		p.consume(lexer.CLOSE_BRACKET)
		proto_type = &ast.Proto_Array{
			InternalType: internal,
		}
	case lexer.OPEN_PAREN:
		p.consume(p.cur.Type)
		var contained []ast.ProtoType
		for p.cur.Type != lexer.CLOSE_PAREN && p.cur.Type != lexer.END {
			item := p.parse_type()
			contained = append(contained, item)
			if p.cur.Type == lexer.COMMA {
				p.consume(p.cur.Type)
			}
		}
		p.consume(p.cur.Type)
		proto_type = &ast.Proto_Tuple{
			InternalTypes: contained,
		}
	default:
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(p.cur.TokenSpan.Line) + ":" + fmt.Sprint(p.cur.TokenSpan.Col))
		msg.WriteString(" Expected type signature but found ")
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
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
		var_type = p.parse_type()
	}

	var assigned_expr ast.Expression = nil
	if p.cur.Type == lexer.ASSIGN {
		p.consume(p.cur.Type)
		assigned_expr = p.parse_expr()
	}

	p.consume(lexer.SEMI_COLON)

	// if variable is not typed or initialized, throw an error
	if var_type.TypeSignature() == "untyped" && assigned_expr == nil {
		var msg strings.Builder
		msg.WriteString(fmt.Sprint(ident.Token.TokenSpan.Line) + ":" + fmt.Sprint(ident.Token.TokenSpan.Col))
		msg.WriteString(fmt.Sprintf(" Expected '%s' to be typed or initialized.", ident.LiteralRepr()))
		msg.WriteString(string(p.cur.Type) + ".")
		shared.ReportErrorAndExit("Parser", msg.String())
	}

	if assigned_expr != nil && !strings.Contains(assigned_expr.Type().TypeSignature(), "untyped") {
		var_type = assigned_expr.Type()
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

func Parse(src string) *ast.ProtoProgram {
	parser := New(src)
	prog := top_level(parser)
	return prog
}

func top_level(p *Parser) *ast.ProtoProgram {
	code := &ast.ProtoProgram{
		Contents: []ast.ProtoNode{},
	}

	for p.cur.Type != lexer.END {
		var node ast.ProtoNode
		switch p.cur.Type {
		case lexer.LET, lexer.MUT:
			node = p.parse_let_mut()
		default:
			node = p.parse_expr()
			if p.cur.Type == lexer.SEMI_COLON { // allow conditional semi-colon
				p.consume(p.cur.Type)
			}
		}
		code.Contents = append(code.Contents, node)
	}

	return code
}