/*
The parser consumes the token stream produced by the lexer and builds an AST.
It keeps a current token index plus first-error state. Public parse calls either
lex source code first or accept an existing token array, then parse a file node
containing top-level declarations.

The parser is hand-written recursive descent. Top-level declaration parsing
looks at the tokens after `::` to decide whether it is parsing a struct,
function, or constant declaration. Blocks parse statements one at a time, and
statements dispatch to returns, local declarations, assignments, and compound
assignments.

Expression parsing is split by shape. Primary expressions handle names,
literals, casts, parenthesized expressions, and inferred struct literals.
Postfix parsing then attaches field access, indexing, calls, and typed struct
literals. Binary expressions use precedence climbing so `*` and `/` bind more
tightly than `+` and `-`.
*/

Parse_State :: struct {
    tokens        : [] Token;
    index         : int;
    has_error     : bool;
    error_message : string;
    error_span    : Source_Span;
}

parse :: (input: string) -> Parse_Result {
    lex_result := lex(input);
    if !lex_result.success {
        result: Parse_Result;
        result.success = false;
        result.error = lex_result.error;
        return result;
    }

    return parse_tokens(lex_result.tokens);
}

parse_tokens :: (tokens: [] Token) -> Parse_Result {
    ps: Parse_State;
    ps.tokens = tokens;

    root := parse_file(*ps);

    result: Parse_Result;
    if ps.has_error {
        result.success = false;
        result.error = .{ message = ps.error_message, span = ps.error_span };
        return result;
    }

    result.root = root;
    result.success = true;
    return result;
}

parse_file :: (ps: *Parse_State) -> *Node {
    root := new_node(.FILE, current(ps).span);

    while !check(ps, .EOF) && !ps.has_error {
        decl := parse_top_level_declaration(ps);
        if decl array_add(*root.declarations, decl);
    }

    return root;
}

parse_top_level_declaration :: (ps: *Parse_State) -> *Node {
    name_token := expect(ps, .IDENTIFIER, "Expected top-level declaration name");
    expect(ps, .COLON_COLON, "Expected '::' after top-level declaration name");

    if match(ps, .STRUCT) return parse_struct_after_header(ps, name_token);
    if check(ps, .OPEN_PAREN) return parse_function_after_header(ps, name_token);

    node := new_node(.CONST_DECL, name_token.span);
    node.name = name_token.text;
    node.rhs = parse_expression(ps);
    expect(ps, .SEMICOLON, "Expected ';' after constant declaration");
    return node;
}

parse_function :: (ps: *Parse_State) -> *Node {
    name_token := expect(ps, .IDENTIFIER, "Expected function name");
    expect(ps, .COLON_COLON, "Expected '::' after function name");
    return parse_function_after_header(ps, name_token);
}

parse_function_after_header :: (ps: *Parse_State, name_token: Token) -> *Node {
    expect(ps, .OPEN_PAREN, "Expected '(' before parameter list");

    fn := new_node(.FUNCTION, name_token.span);
    fn.name = name_token.text;

    if !check(ps, .CLOSE_PAREN) {
        while !ps.has_error {
            param := parse_param(ps);
            if param array_add(*fn.params, param);

            if !match(ps, .COMMA) break;
            if check(ps, .CLOSE_PAREN) break;
        }
    }

    expect(ps, .CLOSE_PAREN, "Expected ')' after parameter list");
    expect(ps, .ARROW, "Expected '->' before return type");
    fn.return_type = parse_type(ps);
    fn.body = parse_block(ps);
    return fn;
}

parse_struct_after_header :: (ps: *Parse_State, name_token: Token) -> *Node {
    node := new_node(.STRUCT_DECL, name_token.span);
    node.name = name_token.text;

    expect(ps, .OPEN_BRACE, "Expected '{' before struct body");
    while !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) && !ps.has_error {
        field_name := expect(ps, .IDENTIFIER, "Expected struct field name");
        field := new_node(.STRUCT_FIELD, field_name.span);
        field.name = field_name.text;

        expect(ps, .COLON, "Expected ':' after struct field name");
        field.type_node = parse_type(ps);
        if match(ps, .EQUAL) field.rhs = parse_expression(ps);
        expect(ps, .SEMICOLON, "Expected ';' after struct field");

        array_add(*node.declarations, field);
    }
    expect(ps, .CLOSE_BRACE, "Expected '}' after struct body");
    return node;
}

parse_param :: (ps: *Parse_State) -> *Node {
    name_token := expect(ps, .IDENTIFIER, "Expected parameter name");
    expect(ps, .COLON, "Expected ':' after parameter name");

    param := new_node(.PARAM, name_token.span);
    param.name = name_token.text;
    param.type_node = parse_type(ps);
    return param;
}

parse_type :: (ps: *Parse_State) -> *Node {
    if match(ps, .STAR) {
        star := previous(ps);
        node := new_node(.POINTER_TYPE, star.span);
        node.type_node = parse_type(ps);
        return node;
    }

    if match(ps, .DOLLAR) {
        dollar := previous(ps);
        name_token := expect(ps, .IDENTIFIER, "Expected polymorphic type name after '$'");
        node := new_node(.POLYMORPHIC_TYPE, dollar.span);
        node.name = name_token.text;

        if match(ps, .SLASH) {
            slash := previous(ps);
            expect(ps, .INTERFACE, "Expected 'interface' after '/' in polymorphic type");

            constraint := new_node(.INTERFACE_TYPE, slash.span);
            constraint.type_node = parse_type(ps);
            node.type_node = constraint;
        }

        return node;
    }

    name_token := expect(ps, .IDENTIFIER, "Expected type name");
    node := new_node(.TYPE_NAME, name_token.span);
    node.name = name_token.text;
    return node;
}

parse_block :: (ps: *Parse_State) -> *Node {
    open := expect(ps, .OPEN_BRACE, "Expected '{' before block");
    block := new_node(.BLOCK, open.span);

    while !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) && !ps.has_error {
        stmt := parse_statement(ps);
        if stmt array_add(*block.statements, stmt);
    }

    expect(ps, .CLOSE_BRACE, "Expected '}' after block");
    return block;
}

parse_statement :: (ps: *Parse_State) -> *Node {
    if match(ps, .RETURN) return parse_return_statement(ps, previous(ps));

    if check(ps, .IDENTIFIER) {
        if check_at(ps, 1, .COLON_EQUAL) return parse_local_infer_decl(ps);
        if check_at(ps, 1, .COLON) return parse_local_typed_decl(ps);
    }

    lhs := parse_expression(ps);
    if match(ps, .EQUAL) {
        op := previous(ps);
        node := new_node(.ASSIGNMENT, lhs.span);
        node.lhs = lhs;
        node.op = op.kind;
        node.rhs = parse_expression(ps);
        expect(ps, .SEMICOLON, "Expected ';' after assignment");
        return node;
    }

    if match(ps, .PLUS_EQUAL) {
        op := previous(ps);
        node := new_node(.COMPOUND_ASSIGNMENT, lhs.span);
        node.lhs = lhs;
        node.op = op.kind;
        node.rhs = parse_expression(ps);
        expect(ps, .SEMICOLON, "Expected ';' after compound assignment");
        return node;
    }

    set_error(ps, "Expected assignment statement", current(ps).span);
    return null;
}

parse_return_statement :: (ps: *Parse_State, return_token: Token) -> *Node {
    node := new_node(.RETURN_STATEMENT, return_token.span);
    node.rhs = parse_expression(ps);
    expect(ps, .SEMICOLON, "Expected ';' after return value");
    return node;
}

parse_local_infer_decl :: (ps: *Parse_State) -> *Node {
    name_token := expect(ps, .IDENTIFIER, "Expected local name");
    expect(ps, .COLON_EQUAL, "Expected ':=' after local name");

    node := new_node(.LOCAL_INFER_DECL, name_token.span);
    node.name = name_token.text;
    node.rhs = parse_expression(ps);
    expect(ps, .SEMICOLON, "Expected ';' after local declaration");
    return node;
}

parse_local_typed_decl :: (ps: *Parse_State) -> *Node {
    name_token := expect(ps, .IDENTIFIER, "Expected local name");
    expect(ps, .COLON, "Expected ':' after local name");

    node := new_node(.LOCAL_TYPED_DECL, name_token.span);
    node.name = name_token.text;
    node.type_node = parse_type(ps);
    expect(ps, .EQUAL, "Expected '=' after typed local declaration");
    node.rhs = parse_expression(ps);
    expect(ps, .SEMICOLON, "Expected ';' after local declaration");
    return node;
}

parse_expression :: (ps: *Parse_State) -> *Node {
    return parse_binary_expression(ps, 0);
}

parse_binary_expression :: (ps: *Parse_State, min_precedence: int) -> *Node {
    left := parse_unary_expression(ps);

    while !ps.has_error {
        op := current(ps);
        precedence := binary_precedence(op.kind);
        if precedence < min_precedence break;

        advance(ps);
        right := parse_binary_expression(ps, precedence + 1);

        binary := new_node(.BINARY_EXPR, left.span);
        binary.op = op.kind;
        binary.lhs = left;
        binary.rhs = right;
        left = binary;
    }

    return left;
}

parse_unary_expression :: (ps: *Parse_State) -> *Node {
    if match(ps, .PLUS) || match(ps, .MINUS) || match(ps, .STAR) {
        op := previous(ps);
        node := new_node(.UNARY_EXPR, op.span);
        node.op = op.kind;
        node.rhs = parse_unary_expression(ps);
        return node;
    }

    return parse_postfix_expression(ps);
}

parse_postfix_expression :: (ps: *Parse_State) -> *Node {
    expr := parse_primary_expression(ps);

    while !ps.has_error {
        if match(ps, .DOT) {
            dot := previous(ps);
            if match(ps, .OPEN_BRACE) {
                expr = parse_struct_literal_after_open(ps, dot, expr);
                continue;
            }

            field := expect(ps, .IDENTIFIER, "Expected field name after '.'");
            node := new_node(.FIELD_ACCESS_EXPR, dot.span);
            node.lhs = expr;
            node.name = field.text;
            expr = node;
            continue;
        }

        if match(ps, .OPEN_BRACKET) {
            open := previous(ps);
            node := new_node(.INDEX_EXPR, open.span);
            node.lhs = expr;
            node.rhs = parse_expression(ps);
            expect(ps, .CLOSE_BRACKET, "Expected ']' after index expression");
            expr = node;
            continue;
        }

        if match(ps, .OPEN_PAREN) {
            open := previous(ps);
            node := new_node(.CALL_EXPR, open.span);
            node.callee = expr;

            if !check(ps, .CLOSE_PAREN) {
                while !ps.has_error {
                    arg := parse_expression(ps);
                    if arg array_add(*node.args, arg);
                    if !match(ps, .COMMA) break;
                    if check(ps, .CLOSE_PAREN) break;
                }
            }

            expect(ps, .CLOSE_PAREN, "Expected ')' after call arguments");
            expr = node;
            continue;
        }

        break;
    }

    return expr;
}

parse_primary_expression :: (ps: *Parse_State) -> *Node {
    if match(ps, .CAST) {
        cast_token := previous(ps);
        node := new_node(.CAST_EXPR, cast_token.span);
        expect(ps, .OPEN_PAREN, "Expected '(' after cast");
        node.type_node = parse_type(ps);
        expect(ps, .CLOSE_PAREN, "Expected ')' after cast type");
        node.rhs = parse_unary_expression(ps);
        return node;
    }

    if match(ps, .DOT) {
        dot := previous(ps);
        expect(ps, .OPEN_BRACE, "Expected '{' after '.' in inferred struct literal");
        return parse_struct_literal_after_open(ps, dot, null);
    }

    if match(ps, .IDENTIFIER) {
        token := previous(ps);
        node := new_node(.IDENTIFIER_EXPR, token.span);
        node.name = token.text;
        return node;
    }

    if match(ps, .INT_LITERAL) {
        token := previous(ps);
        node := new_node(.INT_LITERAL_EXPR, token.span);
        node.literal = token.text;
        return node;
    }

    if match(ps, .FLOAT_LITERAL) {
        token := previous(ps);
        node := new_node(.FLOAT_LITERAL_EXPR, token.span);
        node.literal = token.text;
        return node;
    }

    if match(ps, .STRING_LITERAL) {
        token := previous(ps);
        node := new_node(.STRING_LITERAL_EXPR, token.span);
        node.literal = token.text;
        return node;
    }

    if match(ps, .OPEN_PAREN) {
        expr := parse_expression(ps);
        expect(ps, .CLOSE_PAREN, "Expected ')' after expression");
        return expr;
    }

    set_error(ps, "Expected expression", current(ps).span);
    return null;
}

parse_struct_literal_after_open :: (ps: *Parse_State, start_token: Token, type_expr: *Node) -> *Node {
    node := new_node(.STRUCT_LITERAL_EXPR, start_token.span);
    node.type_node = type_expr;

    if !check(ps, .CLOSE_BRACE) {
        while !ps.has_error {
            entry := new_node(.STRUCT_LITERAL_ENTRY, current(ps).span);

            if check(ps, .IDENTIFIER) && check_at(ps, 1, .EQUAL) {
                name := advance(ps);
                entry.name = name.text;
                expect(ps, .EQUAL, "Expected '=' after struct literal field name");
                entry.rhs = parse_expression(ps);
            } else {
                entry.rhs = parse_expression(ps);
            }

            array_add(*node.args, entry);

            if !match(ps, .COMMA) break;
            if check(ps, .CLOSE_BRACE) break;
        }
    }

    expect(ps, .CLOSE_BRACE, "Expected '}' after struct literal");
    return node;
}

binary_precedence :: (kind: Token_Kind) -> int {
    if kind == .STAR || kind == .SLASH return 20;
    if kind == .PLUS || kind == .MINUS return 10;
    return -1;
}

new_node :: (kind: Node_Kind, span: Source_Span) -> *Node {
    node := New(Node);
    node.kind = kind;
    node.span = span;
    return node;
}

current :: inline (ps: *Parse_State) -> Token {
    if ps.index >= ps.tokens.count return ps.tokens[ps.tokens.count - 1];
    return ps.tokens[ps.index];
}

previous :: inline (ps: *Parse_State) -> Token {
    if ps.index <= 0 return ps.tokens[0];
    return ps.tokens[ps.index - 1];
}

check :: inline (ps: *Parse_State, kind: Token_Kind) -> bool {
    return current(ps).kind == kind;
}

check_at :: inline (ps: *Parse_State, ahead: int, kind: Token_Kind) -> bool {
    idx := ps.index + ahead;
    if idx >= ps.tokens.count return false;
    return ps.tokens[idx].kind == kind;
}

advance :: (ps: *Parse_State) -> Token {
    if !check(ps, .EOF) ps.index += 1;
    return previous(ps);
}

match :: (ps: *Parse_State, kind: Token_Kind) -> bool {
    if !check(ps, kind) return false;
    advance(ps);
    return true;
}

expect :: (ps: *Parse_State, kind: Token_Kind, message: string) -> Token {
    if check(ps, kind) return advance(ps);

    token := current(ps);
    set_error(ps, message, token.span);
    return token;
}

set_error :: (ps: *Parse_State, message: string, span: Source_Span) {
    if ps.has_error return;
    ps.has_error = true;
    ps.error_message = message;
    ps.error_span = span;
}
