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
    errors        : [64] Error;
    error_count   : int;
}

PARSER_PROFILE_ENABLED :: false;

#if PARSER_PROFILE_ENABLED {
Parser_Profile_Slot :: enum {
    PARSE_TOKENS;
    PARSE_FILE;
    TOP_LEVEL_DECL;
    FUNCTION;
    STRUCT_DECL;
    PARAM;
    TYPE;
    BLOCK;
    STATEMENT;
    EXPRESSION;
    BINARY_EXPRESSION;
    UNARY_EXPRESSION;
    POSTFIX_EXPRESSION;
    PRIMARY_EXPRESSION;
    NEW_NODE;
}

PARSER_PROFILE_SLOT_COUNT :: 15;

parser_profile_enabled := false;
parser_profile_counts: [PARSER_PROFILE_SLOT_COUNT] int;
parser_profile_seconds: [PARSER_PROFILE_SLOT_COUNT] float64;
parser_profile_tokens_consumed: [PARSER_PROFILE_SLOT_COUNT] int;
parser_profile_node_counts: [cast(int) Node_Kind.ERROR + 1] int;

parser_profile_slot_names: [PARSER_PROFILE_SLOT_COUNT] string = .[
    "parse_tokens",
    "parse_file",
    "parse_top_level_declaration",
    "parse_function",
    "parse_struct_after_header",
    "parse_param",
    "parse_type",
    "parse_block_after_open",
    "parse_statement",
    "parse_expression",
    "parse_binary_expression",
    "parse_unary_expression",
    "parse_postfix_expression",
    "parse_primary_expression",
    "new_node",
];

parser_profile_reset :: () {
    for i: 0..PARSER_PROFILE_SLOT_COUNT-1 {
        parser_profile_counts[i] = 0;
        parser_profile_seconds[i] = 0.0;
        parser_profile_tokens_consumed[i] = 0;
    }

    for i: 0..parser_profile_node_counts.count-1 parser_profile_node_counts[i] = 0;
}

parser_profile_set_enabled :: (enabled: bool) {
    parser_profile_enabled = enabled;
}

parser_profile_print :: () {
    print("profile,function,calls,total_ms,avg_us,tokens_consumed\n");
    for i: 0..PARSER_PROFILE_SLOT_COUNT-1 {
        calls := parser_profile_counts[i];
        if calls == 0 continue;

        total_ms := parser_profile_seconds[i] * 1000.0;
        avg_us := parser_profile_seconds[i] * 1000000.0 / cast(float64) calls;
        print("profile,%,%,%,%,%\n", parser_profile_slot_names[i], calls, total_ms, avg_us, parser_profile_tokens_consumed[i]);
    }

    print("profile_nodes,kind,count\n");
    for i: 0..parser_profile_node_counts.count-1 {
        count := parser_profile_node_counts[i];
        if count == 0 continue;
        print("profile_nodes,%,%\n", cast(Node_Kind) i, count);
    }
}
} else {
parser_profile_reset :: () {
}

parser_profile_set_enabled :: (enabled: bool) {
}

parser_profile_print :: () {
}
}

parse :: (input: string) -> Parse_Result {
    lex_result := lex(input);
    result := parse_tokens(lex_result.tokens);
    array_free(lex_result.tokens);

    // Lexing and parsing diagnostics are independent of tree availability.
    // An invalid token or unfinished string must not discard the syntax tree
    // built from the remainder of the current editor revision.
    if !lex_result.success {
        for i: 0..lex_result.error_count-1 {
            if i >= lex_result.errors.count break;
            parse_result_add_error(*result, lex_result.errors[i]);
        }
    }
    result.success = result.error_count == 0;
    return result;
}

parse_tokens :: (tokens: [] Token) -> Parse_Result {
    #if PARSER_PROFILE_ENABLED {
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.PARSE_TOKENS] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.PARSE_TOKENS] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.PARSE_TOKENS] += tokens.count;
        }
    }

    ps: Parse_State;
    ps.tokens = tokens;

    root := parse_file(*ps);

    result: Parse_Result;
    result.root = root;
    result.error_count = ps.error_count;
    copy_count := min(ps.error_count, result.errors.count);
    if copy_count > 0 {
        for i: 0..copy_count-1 result.errors[i] = ps.errors[i];
        result.error = result.errors[0];
    }
    result.success = result.error_count == 0;
    return result;
}

parse_result_add_error :: (result: *Parse_Result, error: Error) {
    if result.error_count == 0 || error.span.start_position_in_input < result.error.span.start_position_in_input {
        result.error = error;
    }
    if result.error_count < result.errors.count result.errors[result.error_count] = error;
    result.error_count += 1;
    result.success = false;
}

parse_file :: (ps: *Parse_State) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.PARSE_FILE] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.PARSE_FILE] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.PARSE_FILE] += ps.index - profile_start_index;
        }
    }

    root := new_node(.FILE, current(ps).span);

    parse_top_level_items(ps, root);
    return root;
}

parse_top_level_items :: (ps: *Parse_State, root: *Node, stop_at_close_brace := false) {
    while !check(ps, .EOF) {
        if ps.has_error {
            error_node := parser_recover_pending_error(ps, ps.index, top_level = true);
            if error_node array_add(*root.statements, error_node);
            continue;
        }
        if stop_at_close_brace && check(ps, .CLOSE_BRACE) return;

        if match(ps, .HASH) {
            hash_token := previous(ps);
            if !check(ps, .EOF) && current(ps).text == "if" {
                parse_top_level_if_directive(ps, root, hash_token);
                continue;
            }
            if !check(ps, .EOF) && current(ps).text == "module_parameters" {
                parse_top_level_module_parameters(ps, root);
                continue;
            }
            skip_top_level_compiler_directive(ps);
            continue;
        }

        if match(ps, .AT) {
            skip_top_level_annotation(ps);
            continue;
        }

        if match(ps, .ELSE) {
            skip_top_level_else_directive(ps);
            continue;
        }

        if match(ps, .USING) {
            skip_param_modifiers(ps);
            if !is_top_level_using_declaration(ps) {
                skip_top_level_using_statement(ps);
                continue;
            }

            decl := parse_top_level_declaration(ps);
            if decl array_add(*root.declarations, decl);
            continue;
        }

        decl := parse_top_level_declaration(ps);
        if decl array_add(*root.declarations, decl);
    }
}

parse_top_level_module_parameters :: (ps: *Parse_State, root: *Node) {
    name := expect(ps, .IDENTIFIER, "Expected 'module_parameters' after '#'");
    if name.text != "module_parameters" {
        set_error(ps, "Expected '#module_parameters'", name.span);
        return;
    }

    // Module parameters are file-scope declarations. Their defaults may refer
    // to declarations in the optional body (for example Render_API.OPENGL), so
    // retain both the parameter lists and the body in the same file syntax tree.
    while match(ps, .OPEN_PAREN) {
        if !check(ps, .CLOSE_PAREN) {
            while !ps.has_error {
                param := parse_param(ps);
                if param != null {
                    param.kind = ifx param.type_node != null then .GLOBAL_TYPED_DECL else .CONST_DECL;
                    array_add(*root.declarations, param);
                }

                if !match(ps, .COMMA) break;
                if check(ps, .CLOSE_PAREN) break;
            }
        }
        expect(ps, .CLOSE_PAREN, "Expected ')' after #module_parameters list");
    }

    if match(ps, .OPEN_BRACE) {
        parse_top_level_items(ps, root, stop_at_close_brace = true);
        expect(ps, .CLOSE_BRACE, "Expected '}' after #module_parameters body");
    }
    match(ps, .SEMICOLON);
}

parse_top_level_if_directive :: (ps: *Parse_State, root: *Node, hash_token: Token) {
    if check(ps, .EOF) || current(ps).text != "if" {
        skip_top_level_compiler_directive(ps);
        return;
    }
    advance(ps);

    // Jai also uses `#if value == { case ... }` as a compile-time switch.
    // Its brace contains case directives rather than declarations, so keep
    // treating that form as an opaque directive body.
    if top_level_if_is_switch_form(ps) {
        while !check(ps, .OPEN_BRACE) && !check(ps, .SEMICOLON) && !check(ps, .EOF) && !ps.has_error {
            advance(ps);
        }
        advance(ps);
        skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
        match(ps, .SEMICOLON);
        return;
    }

    // Retain the condition in the syntax tree even though declarations from
    // both branches are flattened into the file. This lets semantic clients
    // classify compiler constants and enum members inside the `#if` itself.
    directive := new_node(.COMPILER_DIRECTIVE_STATEMENT, hash_token.span);
    directive.name = "if";
    directive.lhs = parse_expression(ps);
    array_add(*root.statements, directive);

    if !match(ps, .OPEN_BRACE) {
        match(ps, .SEMICOLON);
        return;
    }

    parse_top_level_items(ps, root, stop_at_close_brace = true);
    expect(ps, .CLOSE_BRACE, "Expected '}' after top-level #if body");

    if !match(ps, .ELSE) return;
    if match(ps, .HASH) {
        parse_top_level_if_directive(ps, root, previous(ps));
        return;
    }

    while !check(ps, .OPEN_BRACE) && !check(ps, .SEMICOLON) && !check(ps, .EOF) && !ps.has_error {
        advance(ps);
    }
    if match(ps, .OPEN_BRACE) {
        parse_top_level_items(ps, root, stop_at_close_brace = true);
        expect(ps, .CLOSE_BRACE, "Expected '}' after top-level #if else body");
    } else {
        match(ps, .SEMICOLON);
    }
}

top_level_if_is_switch_form :: (ps: *Parse_State) -> bool {
    scan := ps.index;
    while scan < ps.tokens.count {
        kind := ps.tokens[scan].kind;
        if kind == .OPEN_BRACE {
            return scan > ps.index && ps.tokens[scan - 1].kind == .EQUAL_EQUAL;
        }
        if kind == .SEMICOLON || kind == .EOF return false;
        scan += 1;
    }
    return false;
}

skip_top_level_compiler_directive :: (ps: *Parse_State) {
    start_line := previous(ps).span.line;
    continued_line := false;

    while !check(ps, .EOF) && !ps.has_error {
        if match(ps, .SEMICOLON) return;

        if match(ps, .OPEN_PAREN) {
            skip_balanced_tokens(ps, .OPEN_PAREN, .CLOSE_PAREN);
            continue;
        }

        if match(ps, .OPEN_BRACKET) {
            skip_balanced_tokens(ps, .OPEN_BRACKET, .CLOSE_BRACKET);
            continue;
        }

        if match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
            skip_top_level_compiler_directive_else_chain(ps);
            match(ps, .SEMICOLON);
            return;
        }

        if current(ps).span.line > start_line {
            if is_directive_continuation_token(current(ps).kind) continued_line = true;
            if !continued_line return;
        }

        advance(ps);
    }
}

is_directive_continuation_token :: (kind: Token_Kind) -> bool {
    return kind == .OR_OR || kind == .AND_AND || kind == .PIPE || kind == .AMPERSAND || kind == .PLUS || kind == .MINUS || kind == .STAR || kind == .SLASH;
}

skip_top_level_compiler_directive_else_chain :: (ps: *Parse_State) {
    while match(ps, .ELSE) {
        while !check(ps, .OPEN_BRACE) && !check(ps, .EOF) && !ps.has_error {
            advance(ps);
        }

        if match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
        } else {
            return;
        }
    }
}

skip_balanced_tokens :: (ps: *Parse_State, open_kind: Token_Kind, close_kind: Token_Kind) {
    depth := 1;

    while depth > 0 && !check(ps, .EOF) && !ps.has_error {
        if match(ps, open_kind) {
            depth += 1;
            continue;
        }

        if match(ps, close_kind) {
            depth -= 1;
            continue;
        }

        advance(ps);
    }
}

skip_expression_until_statement_end :: (ps: *Parse_State) {
    while !check(ps, .SEMICOLON) && !check(ps, .EOF) && !ps.has_error {
        if match(ps, .OPEN_PAREN) {
            skip_balanced_tokens(ps, .OPEN_PAREN, .CLOSE_PAREN);
            continue;
        }

        if match(ps, .OPEN_BRACKET) {
            skip_balanced_tokens(ps, .OPEN_BRACKET, .CLOSE_BRACKET);
            continue;
        }

        if match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
            continue;
        }

        advance(ps);
    }

    match(ps, .SEMICOLON);
}

is_top_level_using_declaration :: (ps: *Parse_State) -> bool {
    return check(ps, .IDENTIFIER) && (check_at(ps, 1, .COLON_COLON) || check_at(ps, 1, .COLON) || check_at(ps, 1, .COMMA));
}

skip_top_level_using_statement :: (ps: *Parse_State) {
    start_line := current(ps).span.line;

    while !check(ps, .EOF) && !ps.has_error {
        if match(ps, .SEMICOLON) return;
        if current(ps).span.line > start_line return;
        advance(ps);
    }
}

skip_top_level_else_directive :: (ps: *Parse_State) {
    start_line := previous(ps).span.line;

    while !check(ps, .EOF) && !ps.has_error {
        if match(ps, .SEMICOLON) return;
        if match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
            match(ps, .SEMICOLON);
            return;
        }
        if current(ps).span.line > start_line return;
        advance(ps);
    }
}

skip_top_level_annotation :: (ps: *Parse_State) {
    start_line := previous(ps).span.line;

    while !check(ps, .EOF) && !ps.has_error {
        if match(ps, .SEMICOLON) return;
        if match(ps, .OPEN_PAREN) {
            skip_balanced_tokens(ps, .OPEN_PAREN, .CLOSE_PAREN);
            continue;
        }
        if current(ps).span.line > start_line return;
        advance(ps);
    }
}

parse_top_level_declaration :: (ps: *Parse_State) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.TOP_LEVEL_DECL] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.TOP_LEVEL_DECL] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.TOP_LEVEL_DECL] += ps.index - profile_start_index;
        }
    }

    name_token := expect(ps, .IDENTIFIER, "Expected top-level declaration name");

    if name_token.text == "operator" return parse_operator_declaration_after_name(ps, name_token);

    if match(ps, .COLON_EQUAL) {
        node := new_node(.LOCAL_INFER_DECL, name_token.span);
        node.name = name_token.text;
        node.rhs = parse_expression(ps);
        match(ps, .SEMICOLON);
        return node;
    }

    if match(ps, .COLON) return parse_global_typed_declaration_after_name(ps, name_token);

    if match(ps, .COMMA) return parse_global_group_typed_declaration_after_name(ps, name_token);

    expect(ps, .COLON_COLON, "Expected '::' after top-level declaration name");

    if match(ps, .STRUCT) || match(ps, .UNION) return parse_struct_after_header(ps, name_token);
    if match(ps, .ENUM) || match(ps, .ENUM_FLAGS) return parse_enum_after_header(ps, name_token);
    skip_declaration_modifiers(ps);
    if check(ps, .OPEN_PAREN) && is_function_header_ahead(ps) return parse_function_after_header(ps, name_token);

    node := new_node(.CONST_DECL, name_token.span);
    node.name = name_token.text;
    node.rhs = parse_expression(ps);
    match(ps, .SEMICOLON);
    return node;
}

skip_declaration_modifiers :: (ps: *Parse_State) {
    while check(ps, .IDENTIFIER) {
        text := current(ps).text;
        if text != "inline" && text != "no_inline" break;
        advance(ps);
    }
}

is_function_header_ahead :: (ps: *Parse_State) -> bool {
    if !check(ps, .OPEN_PAREN) return false;

    scan := ps.index + 1;
    depth := 1;

    while scan < ps.tokens.count && depth > 0 {
        if ps.tokens[scan].kind == .OPEN_PAREN depth += 1;
        else if ps.tokens[scan].kind == .CLOSE_PAREN depth -= 1;
        scan += 1;
    }

    if depth != 0 || scan >= ps.tokens.count return false;

    kind := ps.tokens[scan].kind;
    return kind == .ARROW || kind == .OPEN_BRACE || kind == .HASH;
}

parse_operator_declaration_after_name :: (ps: *Parse_State, operator_token: Token) -> *Node {
    name := operator_token.text;

    while !check(ps, .COLON_COLON) && !check(ps, .EOF) && !ps.has_error {
        name = tprint("%%", name, current(ps).text);
        advance(ps);
    }

    expect(ps, .COLON_COLON, "Expected '::' after operator declaration name");
    skip_declaration_modifiers(ps);
    if !check(ps, .OPEN_PAREN) {
        node := new_node(.CONST_DECL, operator_token.span);
        node.name = name;
        skip_expression_until_statement_end(ps);
        return node;
    }

    fn := parse_function_after_header(ps, operator_token);
    fn.name = name;
    return fn;
}

parse_enum_after_header :: (ps: *Parse_State, name_token: Token) -> *Node {
    node := new_node(.ENUM_DECL, name_token.span);
    node.name = name_token.text;

    while !check(ps, .OPEN_BRACE) && !check(ps, .EOF) && !ps.has_error {
        advance(ps);
    }

    open := expect(ps, .OPEN_BRACE, "Expected '{' before enum body");
    node.body = new_node(.BLOCK, open.span);

    while !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) {
        if ps.has_error {
            error_node := parser_recover_pending_error(ps, ps.index);
            if error_node array_add(*node.declarations, error_node);
            continue;
        }
        if check(ps, .IDENTIFIER) {
            value := advance(ps);
            if match(ps, .COLON_COLON) {
                // Jai's explicit enum values use `NAME :: expression` (the
                // built-in Allocator_Mode is one example).  Only actual
                // enum-scoped type/procedure declarations should be skipped.
                if (check(ps, .OPEN_PAREN) && is_function_header_ahead(ps)) ||
                   check(ps, .STRUCT) || check(ps, .UNION) ||
                   check(ps, .ENUM) || check(ps, .ENUM_FLAGS) {
                    skip_enum_scoped_declaration(ps);
                    continue;
                }

                entry := new_node(.CONST_DECL, value.span);
                entry.name = value.text;
                if !check(ps, .SEMICOLON) && !check(ps, .COMMA) && !check(ps, .CLOSE_BRACE) {
                    entry.rhs = parse_expression(ps);
                }
                array_add(*node.declarations, entry);
                match(ps, .SEMICOLON);
                match(ps, .COMMA);
                continue;
            }

            entry := new_node(.CONST_DECL, value.span);
            entry.name = value.text;
            array_add(*node.declarations, entry);

            if match(ps, .EQUAL) entry.rhs = parse_expression(ps);
            match(ps, .SEMICOLON);
            match(ps, .COMMA);
            continue;
        }

        advance(ps);
    }

    expect(ps, .CLOSE_BRACE, "Expected '}' after enum body");
    match(ps, .SEMICOLON);
    return node;
}

skip_enum_scoped_declaration :: (ps: *Parse_State) {
    while !check(ps, .SEMICOLON) && !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) && !ps.has_error {
        if match(ps, .OPEN_PAREN) {
            skip_balanced_tokens(ps, .OPEN_PAREN, .CLOSE_PAREN);
            continue;
        }

        if match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
            match(ps, .SEMICOLON);
            return;
        }

        advance(ps);
    }

    match(ps, .SEMICOLON);
    match(ps, .COMMA);
}

parse_global_typed_declaration_after_name :: (ps: *Parse_State, name_token: Token) -> *Node {
    node := new_node(.GLOBAL_TYPED_DECL, name_token.span);
    node.name = name_token.text;
    node.type_node = parse_type(ps);

    if match(ps, .COLON) || match(ps, .EQUAL) {
        node.rhs = parse_expression(ps);
    }

    match(ps, .SEMICOLON);
    return node;
}

parse_global_group_typed_declaration_after_name :: (ps: *Parse_State, first_name: Token) -> *Node {
    node := new_node(.LOCAL_GROUP_DECL, first_name.span);
    names: [..] Token;
    array_add(*names, first_name);

    while !ps.has_error {
        name := expect(ps, .IDENTIFIER, "Expected global name after ','");
        array_add(*names, name);
        if !match(ps, .COMMA) break;
    }

    if match(ps, .COLON_EQUAL) {
        value := parse_expression_list(ps);
        node.rhs = value;
        match(ps, .SEMICOLON);

        for names {
            global := new_node(.LOCAL_INFER_DECL, it.span);
            global.name = it.text;
            global.rhs = value;
            array_add(*node.declarations, global);
        }

        return node;
    }

    expect(ps, .COLON, "Expected ':' after global names");
    value_type := parse_type(ps);
    default_value: *Node = null;
    if match(ps, .EQUAL) || match(ps, .COLON) default_value = parse_expression(ps);
    match(ps, .SEMICOLON);

    for names {
        global := new_node(.GLOBAL_TYPED_DECL, it.span);
        global.name = it.text;
        global.type_node = value_type;
        global.rhs = default_value;
        array_add(*node.declarations, global);
    }

    return node;
}

parse_function :: (ps: *Parse_State) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.FUNCTION] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.FUNCTION] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.FUNCTION] += ps.index - profile_start_index;
        }
    }

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
    if match(ps, .ARROW) {
        fn.return_type = parse_return_type(ps);
    }
    if !parse_function_body_directives(ps, fn) return fn;
    if match(ps, .SEMICOLON) return fn;
    fn.body = parse_block(ps);
    match(ps, .SEMICOLON);
    return fn;
}

parse_function_body_directives :: (ps: *Parse_State, fn: *Node) -> bool {
    while match(ps, .HASH) {
        hash_token := previous(ps);
        start_line := previous(ps).span.line;
        directive_name := "";
        if check(ps, .IDENTIFIER) {
            directive_name = current(ps).text;
            advance(ps);
        }

        if directive_name == "foreign" {
            directive := new_node(.COMPILER_DIRECTIVE_STATEMENT, hash_token.span);
            directive.name = directive_name;
            if current(ps).span.line == start_line && can_start_primary_expression(ps) {
                directive.lhs = parse_expression(ps);
            }
            array_add(*fn.statements, directive);
            while !check(ps, .SEMICOLON) && !check(ps, .EOF) && !ps.has_error && current(ps).span.line == start_line advance(ps);
            if match(ps, .SEMICOLON) return false;
            continue;
        }

        while !check(ps, .OPEN_BRACE) && !check(ps, .EOF) && !ps.has_error {
            if match(ps, .SEMICOLON) return false;
            if current(ps).span.line > start_line break;
            advance(ps);
        }

        if directive_name == "modify" && match(ps, .OPEN_BRACE) {
            directive := new_node(.COMPILER_DIRECTIVE_STATEMENT, hash_token.span);
            directive.name = directive_name;
            directive.body = parse_block_after_open(ps, previous(ps));
            array_add(*fn.statements, directive);
        }
    }

    return true;
}

parse_struct_after_header :: (ps: *Parse_State, name_token: Token) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.STRUCT_DECL] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.STRUCT_DECL] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.STRUCT_DECL] += ps.index - profile_start_index;
        }
    }

    node := new_node(.STRUCT_DECL, name_token.span);
    node.name = name_token.text;

    if match(ps, .OPEN_PAREN) {
        if !check(ps, .CLOSE_PAREN) {
            while !ps.has_error {
                param := parse_param(ps);
                if param array_add(*node.params, param);

                if !match(ps, .COMMA) break;
                if check(ps, .CLOSE_PAREN) break;
            }
        }
        expect(ps, .CLOSE_PAREN, "Expected ')' after struct parameter list");
    }

    skip_struct_header_directives(ps);
    expect(ps, .OPEN_BRACE, "Expected '{' before struct body");
    while !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) {
        if ps.has_error {
            error_node := parser_recover_pending_error(ps, ps.index);
            if error_node array_add(*node.declarations, error_node);
            continue;
        }
        if match(ps, .STRUCT) || match(ps, .UNION) {
            nested := parse_struct_after_header(ps, previous(ps));
            if nested array_add(*node.declarations, nested);
            continue;
        }

        if match(ps, .AT) {
            skip_struct_annotation(ps);
            continue;
        }

        if check(ps, .HASH) && check_at(ps, 1, .IDENTIFIER) && ps.tokens[ps.index + 1].text == "as" {
            field := parse_hash_as_using_struct_field(ps);
            if field array_add(*node.declarations, field);
            continue;
        }

        if check(ps, .HASH) && check_at(ps, 1, .IDENTIFIER) && ps.tokens[ps.index + 1].text == "overlay" {
            field := parse_overlay_struct_field(ps);
            if field array_add(*node.declarations, field);
            continue;
        }

        if match(ps, .HASH) {
            directive := parse_compiler_directive_statement(ps, previous(ps));
            if directive array_add(*node.declarations, directive);
            continue;
        }

        if match(ps, .USING) {
            field := parse_using_struct_field(ps, previous(ps));
            if field array_add(*node.declarations, field);
            continue;
        }

        field_name := expect_name_token(ps, "Expected struct field name");

        if check(ps, .DOT) || check(ps, .OPEN_BRACKET) {
            skip_struct_body_statement(ps);
            continue;
        }

        if match(ps, .COLON_COLON) {
            nested: *Node = null;
            if match(ps, .STRUCT) || match(ps, .UNION) nested = parse_struct_after_header(ps, field_name);
            else if match(ps, .ENUM) || match(ps, .ENUM_FLAGS) nested = parse_enum_after_header(ps, field_name);
            else {
                skip_declaration_modifiers(ps);
                if check(ps, .OPEN_PAREN) && is_function_header_ahead(ps) nested = parse_function_after_header(ps, field_name);
            }
            if !nested {
                nested = new_node(.CONST_DECL, field_name.span);
                nested.name = field_name.text;
                nested.rhs = parse_expression(ps);
                match(ps, .SEMICOLON);
            }

            if nested array_add(*node.declarations, nested);
            continue;
        }

        if match(ps, .COLON_EQUAL) {
            field := new_node(.STRUCT_FIELD, field_name.span);
            field.name = field_name.text;
            field.rhs = parse_expression(ps);
            match(ps, .SEMICOLON);
            array_add(*node.declarations, field);
            continue;
        }

        if match(ps, .EQUAL) {
            field := new_node(.STRUCT_FIELD, field_name.span);
            field.name = field_name.text;
            field.rhs = parse_expression(ps);
            match(ps, .SEMICOLON);
            array_add(*node.declarations, field);
            continue;
        }

        field_names: [..] Token;
        array_add(*field_names, field_name);

        while match(ps, .COMMA) {
            next_name := expect_name_token(ps, "Expected struct field name after ','");
            array_add(*field_names, next_name);
        }

        expect(ps, .COLON, "Expected ':' after struct field name");
        field_type := parse_type(ps);
        field_default: *Node = null;
        if match(ps, .EQUAL) || match(ps, .COLON) field_default = parse_expression(ps);
        match(ps, .SEMICOLON);

        for field_names {
            field := new_node(.STRUCT_FIELD, it.span);
            field.name = it.text;
            field.type_node = field_type;
            field.rhs = field_default;
            array_add(*node.declarations, field);
        }
    }
    expect(ps, .CLOSE_BRACE, "Expected '}' after struct body");
    match(ps, .SEMICOLON);
    return node;
}

skip_struct_body_statement :: (ps: *Parse_State) {
    while !check(ps, .SEMICOLON) && !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) && !ps.has_error {
        if match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
            continue;
        }

        advance(ps);
    }

    match(ps, .SEMICOLON);
}

skip_struct_header_directives :: (ps: *Parse_State) {
    while match(ps, .HASH) {
        start_line := previous(ps).span.line;
        directive_name := "";
        if check(ps, .IDENTIFIER) {
            directive_name = current(ps).text;
            advance(ps);
        }

        while !check(ps, .OPEN_BRACE) && !check(ps, .EOF) && !ps.has_error {
            if current(ps).span.line > start_line break;
            advance(ps);
        }

        if directive_name == "modify" && match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
        }
    }
}

skip_struct_compiler_directive :: (ps: *Parse_State) {
    start_line := previous(ps).span.line;

    while !check(ps, .EOF) && !ps.has_error {
        if match(ps, .SEMICOLON) return;
        if match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
            skip_top_level_compiler_directive_else_chain(ps);
            match(ps, .SEMICOLON);
            return;
        }
        if check(ps, .CLOSE_BRACE) return;
        if current(ps).span.line > start_line return;
        advance(ps);
    }
}

skip_struct_annotation :: (ps: *Parse_State) {
    start_line := previous(ps).span.line;
    while !check(ps, .EOF) && !ps.has_error {
        if match(ps, .SEMICOLON) return;
        if current(ps).span.line > start_line return;
        advance(ps);
    }
}

parse_overlay_struct_field :: (ps: *Parse_State) -> *Node {
    expect(ps, .HASH, "Expected '#overlay' in struct field");
    overlay_name := expect(ps, .IDENTIFIER, "Expected 'overlay' after '#'");
    if overlay_name.text != "overlay" {
        set_error(ps, "Expected '#overlay' in struct field", overlay_name.span);
        return null;
    }

    expect(ps, .OPEN_PAREN, "Expected '(' after '#overlay'");
    target := parse_expression(ps);
    expect(ps, .CLOSE_PAREN, "Expected ')' after #overlay target");

    field: *Node;
    if match(ps, .USING) {
        field = parse_using_struct_field(ps, previous(ps));
    } else if check(ps, .HASH) && check_at(ps, 1, .IDENTIFIER) && ps.tokens[ps.index + 1].text == "as" {
        field = parse_hash_as_using_struct_field(ps);
    } else {
        name_token := expect_name_token(ps, "Expected overlaid struct field name");
        expect(ps, .COLON, "Expected ':' after overlaid struct field name");

        field = new_node(.STRUCT_FIELD, name_token.span);
        field.name = name_token.text;
        field.type_node = parse_type(ps);
        if match(ps, .EQUAL) field.rhs = parse_expression(ps);
        match(ps, .SEMICOLON);
    }

    if field != null field.lhs = target;
    return field;
}

parse_hash_as_using_struct_field :: (ps: *Parse_State) -> *Node {
    hash := expect(ps, .HASH, "Expected '#as' in using struct field");
    as_name := expect(ps, .IDENTIFIER, "Expected 'as' after '#'");
    if as_name.text != "as" {
        set_error(ps, "Expected '#as' in using struct field", as_name.span);
        return null;
    }

    if match(ps, .USING) skip_param_modifiers(ps);
    name_token := expect(ps, .IDENTIFIER, "Expected using struct field name");
    expect(ps, .COLON, "Expected ':' after using struct field name");

    field := new_node(.STRUCT_FIELD, name_token.span);
    field.name = name_token.text;
    field.is_using = true;
    field.type_node = parse_type(ps);
    if match(ps, .EQUAL) field.rhs = parse_expression(ps);
    match(ps, .SEMICOLON);
    return field;
}

parse_using_struct_field :: (ps: *Parse_State, using_token: Token) -> *Node {
    skip_param_modifiers(ps);

    if match(ps, .HASH) {
        name := advance(ps);
        if name.text != "as" {
            set_error(ps, "Expected '#as' in using struct field", name.span);
            return null;
        }
    }

    name_token := expect(ps, .IDENTIFIER, "Expected using struct field name");
    if match(ps, .SEMICOLON) {
        field := new_node(.STRUCT_FIELD, name_token.span);
        field.name = name_token.text;
        field.is_using = true;
        type_node := new_node(.TYPE_NAME, name_token.span);
        type_node.name = name_token.text;
        field.type_node = type_node;
        return field;
    }

    expect(ps, .COLON, "Expected ':' after using struct field name");

    field := new_node(.STRUCT_FIELD, name_token.span);
    field.name = name_token.text;
    field.is_using = true;
    field.type_node = parse_type(ps);
    if match(ps, .EQUAL) field.rhs = parse_expression(ps);
    match(ps, .SEMICOLON);
    return field;
}

parse_param :: (ps: *Parse_State) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.PARAM] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.PARAM] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.PARAM] += ps.index - profile_start_index;
        }
    }

    is_using := match(ps, .USING);
    using_exceptions: [..] string;
    skip_param_modifiers(ps, *using_exceptions);
    while match(ps, .HASH) {
        expect(ps, .IDENTIFIER, "Expected parameter directive name after '#'");
        if match(ps, .OPEN_PAREN) skip_balanced_tokens(ps, .OPEN_PAREN, .CLOSE_PAREN);
    }

    dollar_count := 0;
    while match(ps, .DOLLAR) dollar_count += 1;
    name_token := expect(ps, .IDENTIFIER, "Expected parameter name");

    param := new_node(.PARAM, name_token.span);
    param.name = name_token.text;
    param.is_using = is_using;
    param.using_exceptions = using_exceptions;
    for 0..dollar_count-1 param.name = tprint("$%", param.name);

    if match(ps, .COLON_EQUAL) {
        param.rhs = parse_expression(ps);
        return param;
    }

    expect(ps, .COLON, "Expected ':' or ':=' after parameter name");
    // A comma after a callback's return type belongs to this surrounding
    // parameter list. Unparenthesized multi-return callback types are
    // syntactically ambiguous here and must use a parenthesized return list.
    param.type_node = parse_type(ps, allow_unparenthesized_function_return_list = false);
    if match(ps, .EQUAL) param.rhs = parse_expression(ps);
    return param;
}

skip_param_modifiers :: (ps: *Parse_State, using_exceptions: *[..] string = null) {
    while match(ps, .COMMA) {
        modifier := expect(ps, .IDENTIFIER, "Expected parameter modifier name");
        if modifier.text == "except" && match(ps, .OPEN_PAREN) {
            depth := 1;
            while depth > 0 && !check(ps, .EOF) && !ps.has_error {
                token := advance(ps);
                if token.kind == .OPEN_PAREN {
                    depth += 1;
                } else if token.kind == .CLOSE_PAREN {
                    depth -= 1;
                } else if depth == 1 && token.kind == .IDENTIFIER && using_exceptions != null {
                    array_add(using_exceptions, token.text);
                }
            }
        }
    }
}

parse_type :: (ps: *Parse_State, allow_unparenthesized_function_return_list: bool = true) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.TYPE] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.TYPE] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.TYPE] += ps.index - profile_start_index;
        }
    }

    if match(ps, .HASH) {
        hash := previous(ps);
        name := expect(ps, .IDENTIFIER, "Expected type directive name after '#'");
        if name.text == "type" {
            node := parse_type(ps, allow_unparenthesized_function_return_list);
            skip_type_suffix_directives(ps);
            return node;
        }

        node := new_node(.TYPE_NAME, hash.span);
        node.name = tprint("#%", name.text);
        skip_type_suffix_directives(ps);
        return node;
    }

    if check(ps, .OPEN_PAREN) return parse_function_type(ps, allow_unparenthesized_function_return_list);

    if match(ps, .STRUCT) || match(ps, .UNION) return parse_struct_after_header(ps, previous(ps));
    if match(ps, .ENUM) || match(ps, .ENUM_FLAGS) return parse_anonymous_enum_type(ps, previous(ps));

    if match(ps, .DOT_DOT) {
        dot_dot := previous(ps);
        node := new_node(.VARIADIC_TYPE, dot_dot.span);
        node.type_node = parse_type(ps, allow_unparenthesized_function_return_list);
        return node;
    }

    if match(ps, .OPEN_BRACKET) {
        open := previous(ps);
        node := new_node(.ARRAY_TYPE, open.span);

        if match(ps, .DOT_DOT) {
            // Slice type.
        } else if !check(ps, .CLOSE_BRACKET) {
            node.rhs = parse_expression(ps);
        }
        expect(ps, .CLOSE_BRACKET, "Expected ']' after array count");
        node.type_node = parse_type(ps, allow_unparenthesized_function_return_list);
        return node;
    }

    if match(ps, .STAR) {
        star := previous(ps);
        node := new_node(.POINTER_TYPE, star.span);
        node.type_node = parse_type(ps, allow_unparenthesized_function_return_list);
        return node;
    }

    if match(ps, .DOLLAR) {
        name_token := expect(ps, .IDENTIFIER, "Expected polymorphic type name after '$'");
        node := new_node(.POLYMORPHIC_TYPE, name_token.span);
        node.name = tprint("$%", name_token.text);

        if match(ps, .SLASH) {
            slash := previous(ps);
            match(ps, .INTERFACE);

            constraint := new_node(.INTERFACE_TYPE, slash.span);
            if check(ps, .DOT) || check(ps, .OPEN_BRACKET) constraint.type_node = parse_expression(ps);
            else constraint.type_node = parse_type(ps, allow_unparenthesized_function_return_list);
            node.type_node = constraint;
        }

        return node;
    }

    if !check(ps, .IDENTIFIER) && !check(ps, .INTERFACE) {
        span := current(ps).span;
        set_error(ps, "Expected type name", span);
        return new_node(.ERROR, span);
    }

    name_token := expect_name_token(ps, "Expected type name");
    node := new_node(.TYPE_NAME, name_token.span);
    node.name = name_token.text;

    qualified := false;
    while match(ps, .DOT) {
        field := expect(ps, .IDENTIFIER, "Expected type name after '.'");
        if !qualified {
            base_segment := new_node(.TYPE_NAME, name_token.span);
            base_segment.name = name_token.text;
            array_add(*node.args, base_segment);
            qualified = true;
        }
        field_segment := new_node(.TYPE_NAME, field.span);
        field_segment.name = field.text;
        array_add(*node.args, field_segment);
        node.name = tprint("%.%", node.name, field.text);
    }

    if match(ps, .OPEN_PAREN) {
        application := new_node(.TYPE_APPLICATION, node.span);
        application.type_node = node;

        if !check(ps, .CLOSE_PAREN) {
            while !ps.has_error {
                arg := parse_type_application_argument(ps, allow_unparenthesized_function_return_list);
                if arg array_add(*application.args, arg);
                if !match(ps, .COMMA) break;
                if check(ps, .CLOSE_PAREN) break;
            }
        }

        expect(ps, .CLOSE_PAREN, "Expected ')' after type arguments");
        node = application;
    }

    while match(ps, .DOT) {
        field := expect(ps, .IDENTIFIER, "Expected type name after '.'");
        if node.name.count == 0 node.name = field.text;
        else node.name = tprint("%.%", node.name, field.text);
    }

    return node;
}

parse_type_application_argument :: (ps: *Parse_State, allow_unparenthesized_function_return_list: bool = true) -> *Node {
    if check(ps, .INT_LITERAL) || check(ps, .FLOAT_LITERAL) || check(ps, .STRING_LITERAL) || check(ps, .DOT) || check(ps, .DOLLAR) {
        return parse_expression(ps);
    }

    return parse_type(ps, allow_unparenthesized_function_return_list);
}

skip_type_suffix_directives :: (ps: *Parse_State) {
    while match(ps, .HASH) {
        start_line := previous(ps).span.line;
        if check(ps, .IDENTIFIER) advance(ps);

        while !check(ps, .OPEN_BRACE) && !check(ps, .SEMICOLON) && !check(ps, .COMMA) && !check(ps, .CLOSE_PAREN) && !check(ps, .CLOSE_BRACE) && !check(ps, .EQUAL) && !check(ps, .COLON) && !check(ps, .EOF) && !ps.has_error {
            if current(ps).span.line > start_line return;
            if match(ps, .OPEN_PAREN) {
                skip_balanced_tokens(ps, .OPEN_PAREN, .CLOSE_PAREN);
                continue;
            }

            advance(ps);
        }
    }
}

parse_anonymous_enum_type :: (ps: *Parse_State, enum_token: Token) -> *Node {
    node := new_node(.ENUM_DECL, enum_token.span);

    while !check(ps, .OPEN_BRACE) && !check(ps, .EOF) && !ps.has_error {
        advance(ps);
    }

    open := expect(ps, .OPEN_BRACE, "Expected '{' before enum type body");
    node.body = new_node(.BLOCK, open.span);

    while !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) {
        if ps.has_error {
            error_node := parser_recover_pending_error(ps, ps.index);
            if error_node array_add(*node.declarations, error_node);
            continue;
        }
        if check(ps, .IDENTIFIER) {
            value := advance(ps);
            entry := new_node(.CONST_DECL, value.span);
            entry.name = value.text;
            array_add(*node.declarations, entry);
            if match(ps, .EQUAL) entry.rhs = parse_expression(ps);
            match(ps, .SEMICOLON);
            match(ps, .COMMA);
            continue;
        }

        advance(ps);
    }

    expect(ps, .CLOSE_BRACE, "Expected '}' after enum type body");
    return node;
}

parse_return_type :: (ps: *Parse_State) -> *Node {
    if check(ps, .OPEN_PAREN) && is_parenthesized_return_type_list_ahead(ps) return parse_parenthesized_return_type_list(ps);

    first := parse_return_type_item(ps);
    if !match(ps, .COMMA) return first;

    node := new_node(.TYPE_LIST, first.span);
    if first.kind == .PARAM array_add(*node.params, first);
    else array_add(*node.args, first);

    while !ps.has_error {
        next := parse_return_type_item(ps);
        if next {
            if next.kind == .PARAM array_add(*node.params, next);
            else array_add(*node.args, next);
        }
        if !match(ps, .COMMA) break;
    }

    return node;
}

is_parenthesized_return_type_list_ahead :: (ps: *Parse_State) -> bool {
    if !check(ps, .OPEN_PAREN) return false;

    scan := ps.index + 1;
    depth := 1;
    while scan < ps.tokens.count && depth > 0 {
        kind := ps.tokens[scan].kind;
        if kind == .OPEN_PAREN depth += 1;
        else if kind == .CLOSE_PAREN depth -= 1;
        else if depth == 1 && (kind == .COLON || kind == .COLON_EQUAL) return true;
        scan += 1;
    }

    return false;
}

parse_parenthesized_return_type_list :: (ps: *Parse_State) -> *Node {
    open := expect(ps, .OPEN_PAREN, "Expected '(' before return type list");
    node := new_node(.TYPE_LIST, open.span);

    if !check(ps, .CLOSE_PAREN) {
        while !ps.has_error {
            item := parse_return_type_item(ps);
            if item {
                if item.kind == .PARAM array_add(*node.params, item);
                else array_add(*node.args, item);
            }

            if !match(ps, .COMMA) break;
            if check(ps, .CLOSE_PAREN) break;
        }
    }

    expect(ps, .CLOSE_PAREN, "Expected ')' after return type list");
    return node;
}

parse_return_type_item :: (ps: *Parse_State) -> *Node {
    if check(ps, .IDENTIFIER) && (check_at(ps, 1, .COLON) || check_at(ps, 1, .COLON_EQUAL)) return parse_param(ps);
    return parse_type(ps);
}

parse_block :: (ps: *Parse_State) -> *Node {
    open := expect(ps, .OPEN_BRACE, "Expected '{' before block");
    return parse_block_after_open(ps, open);
}

parse_block_after_open :: (ps: *Parse_State, open: Token) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.BLOCK] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.BLOCK] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.BLOCK] += ps.index - profile_start_index;
        }
    }

    block := new_node(.BLOCK, open.span);

    while !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) {
        statement_start := ps.index;
        stmt := parse_statement(ps);
        if stmt array_add(*block.statements, stmt);
        if ps.has_error {
            error_node := parser_recover_pending_error(ps, statement_start);
            if error_node array_add(*block.statements, error_node);
        } else if ps.index == statement_start {
            set_error(ps, "Expected statement", current(ps).span);
            error_node := parser_recover_pending_error(ps, statement_start);
            if error_node array_add(*block.statements, error_node);
        }
    }

    expect(ps, .CLOSE_BRACE, "Expected '}' after block");
    return block;
}

parse_statement :: (ps: *Parse_State) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.STATEMENT] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.STATEMENT] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.STATEMENT] += ps.index - profile_start_index;
        }
    }

    if match(ps, .SEMICOLON) return null;
    if match(ps, .AT) {
        skip_top_level_annotation(ps);
        return null;
    }
    if match(ps, .BACKTICK) return parse_statement(ps);
    if match(ps, .HASH) return parse_compiler_directive_statement(ps, previous(ps));
    if match(ps, .OPEN_BRACE) return parse_block_after_open(ps, previous(ps));
    if match(ps, .RETURN) return parse_return_statement(ps, previous(ps));
    if match(ps, .IF) return parse_if_statement(ps, previous(ps));
    if match(ps, .FOR) return parse_for_statement(ps, previous(ps));
    if match(ps, .WHILE) return parse_while_statement(ps, previous(ps));
    if match(ps, .DEFER) return parse_defer_statement(ps, previous(ps));
    if match(ps, .ELSE) return parse_standalone_else_statement(ps, previous(ps));
    if match(ps, .USING) return parse_using_local_statement(ps);
    if match(ps, .STRUCT) || match(ps, .UNION) return parse_struct_after_header(ps, previous(ps));

    if check(ps, .IDENTIFIER) {
        if check_at(ps, 1, .COLON_COLON) return parse_local_colon_colon_declaration(ps);
        if check_at(ps, 1, .COLON_EQUAL) return parse_local_infer_decl(ps);
        if check_at(ps, 1, .EQUAL) && check_at(ps, 2, .COMMA) return parse_mixed_assignment_declaration(ps);
        if check_at(ps, 1, .COMMA) && is_mixed_assignment_declaration_ahead(ps) return parse_mixed_assignment_declaration(ps);
        if check_at(ps, 1, .COMMA) && is_local_declaration_assignment_list_ahead(ps) return parse_local_declaration_assignment_list(ps);
        if check_at(ps, 1, .COMMA) && is_local_group_constant_declaration_ahead(ps) return parse_local_group_constant_decl(ps);
        if check_at(ps, 1, .COMMA) && is_local_group_declaration_ahead(ps) return parse_local_group_decl(ps);
        if check_at(ps, 1, .COLON) && (check_at(ps, 2, .COMMA) || check_at(ps, 2, .EQUAL)) return parse_local_declaration_assignment(ps);
        if check_at(ps, 1, .COLON) return parse_local_typed_decl(ps);
    }

    lhs := parse_expression_list(ps);
    if lhs == null return null;

    if match(ps, .EQUAL) {
        op := previous(ps);
        node := new_node(.ASSIGNMENT, lhs.span);
        node.lhs = lhs;
        node.op = op.kind;
        node.rhs = parse_expression_list(ps);
        match(ps, .SEMICOLON);
        return node;
    }

    if match(ps, .PLUS_EQUAL) || match(ps, .MINUS_EQUAL) || match(ps, .STAR_EQUAL) || match(ps, .SLASH_EQUAL) || match(ps, .PERCENT_EQUAL) || match(ps, .AMPERSAND_EQUAL) || match(ps, .AND_AND_EQUAL) || match(ps, .CARET_EQUAL) || match(ps, .PIPE_EQUAL) || match(ps, .OR_OR_EQUAL) || match(ps, .LESS_LESS_EQUAL) || match(ps, .GREATER_GREATER_EQUAL) {
        op := previous(ps);
        node := new_node(.COMPOUND_ASSIGNMENT, lhs.span);
        node.lhs = lhs;
        node.op = op.kind;
        node.rhs = parse_expression(ps);
        match(ps, .SEMICOLON);
        return node;
    }

    node := new_node(.EXPRESSION_STATEMENT, lhs.span);
    node.rhs = lhs;
    match(ps, .SEMICOLON);
    return node;
}

parse_standalone_else_statement :: (ps: *Parse_State, else_token: Token) -> *Node {
    node := new_node(.COMPILER_DIRECTIVE_STATEMENT, else_token.span);
    node.name = "else";

    if match(ps, .OPEN_BRACE) {
        node.body = parse_block_after_open(ps, previous(ps));
        return node;
    }

    while !check(ps, .SEMICOLON) && !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) && !ps.has_error {
        advance(ps);
    }

    match(ps, .SEMICOLON);
    return node;
}

parse_using_local_statement :: (ps: *Parse_State) -> *Node {
    exceptions: [..] string;
    skip_param_modifiers(ps, *exceptions);

    if check(ps, .IDENTIFIER) {
        declaration: *Node;
        if check_at(ps, 1, .COLON_EQUAL) declaration = parse_local_infer_decl(ps);
        else if check_at(ps, 1, .COLON) declaration = parse_local_typed_decl(ps);
        if declaration != null {
            declaration.is_using = true;
            declaration.using_exceptions = exceptions;
            return declaration;
        }
    }

    expr := parse_expression_list(ps);
    if expr == null {
        array_free(exceptions);
        return null;
    }

    node := new_node(.EXPRESSION_STATEMENT, expr.span);
    node.is_using = true;
    node.using_exceptions = exceptions;
    node.rhs = expr;
    match(ps, .SEMICOLON);
    return node;
}

is_local_group_declaration_ahead :: (ps: *Parse_State) -> bool {
    scan := ps.index;

    while scan < ps.tokens.count {
        if ps.tokens[scan].kind != .IDENTIFIER return false;
        scan += 1;

        if scan >= ps.tokens.count return false;
        if ps.tokens[scan].kind == .COLON || ps.tokens[scan].kind == .COLON_EQUAL return true;
        if ps.tokens[scan].kind != .COMMA return false;
        scan += 1;
    }

    return false;
}

is_local_group_constant_declaration_ahead :: (ps: *Parse_State) -> bool {
    scan := ps.index;

    while scan < ps.tokens.count {
        if ps.tokens[scan].kind != .IDENTIFIER return false;
        scan += 1;

        if scan >= ps.tokens.count return false;
        if ps.tokens[scan].kind == .COLON_COLON return true;
        if ps.tokens[scan].kind != .COMMA return false;
        scan += 1;
    }

    return false;
}

parse_local_group_constant_decl :: (ps: *Parse_State) -> *Node {
    first_name := expect(ps, .IDENTIFIER, "Expected local constant name");
    node := new_node(.LOCAL_GROUP_DECL, first_name.span);
    names: [..] Token;
    array_add(*names, first_name);

    while match(ps, .COMMA) {
        name := expect(ps, .IDENTIFIER, "Expected local constant name after ','");
        array_add(*names, name);
    }

    expect(ps, .COLON_COLON, "Expected '::' after local constant names");
    value := parse_expression_list(ps);
    node.rhs = value;
    match(ps, .SEMICOLON);

    for names {
        local := new_node(.CONST_DECL, it.span);
        local.name = it.text;
        local.rhs = value;
        array_add(*node.declarations, local);
    }

    return node;
}

is_mixed_assignment_declaration_ahead :: (ps: *Parse_State) -> bool {
    scan := ps.index;
    saw_equal_marker := false;

    while scan < ps.tokens.count {
        kind := ps.tokens[scan].kind;
        if kind == .COLON_EQUAL return saw_equal_marker;
        if kind == .EQUAL saw_equal_marker = true;
        if kind == .SEMICOLON || kind == .CLOSE_BRACE || kind == .EOF return false;
        scan += 1;
    }

    return false;
}

is_local_declaration_assignment_list_ahead :: (ps: *Parse_State) -> bool {
    scan := ps.index;
    saw_colon := false;

    while scan < ps.tokens.count {
        kind := ps.tokens[scan].kind;
        if kind == .COLON {
            next := scan + 1;
            if next < ps.tokens.count && (ps.tokens[next].kind == .COMMA || ps.tokens[next].kind == .EQUAL) {
                saw_colon = true;
            }
        } else if kind == .EQUAL {
            return saw_colon;
        } else if kind == .SEMICOLON || kind == .CLOSE_BRACE || kind == .EOF {
            return false;
        }

        scan += 1;
    }

    return false;
}

parse_local_declaration_assignment_list :: (ps: *Parse_State) -> *Node {
    first_name := expect(ps, .IDENTIFIER, "Expected local name");
    node := new_node(.LOCAL_GROUP_DECL, first_name.span);

    local := new_node(.LOCAL_INFER_DECL, first_name.span);
    local.name = first_name.text;
    array_add(*node.declarations, local);
    match(ps, .COLON);

    while match(ps, .COMMA) {
        name := expect(ps, .IDENTIFIER, "Expected local name after ','");
        local = new_node(.LOCAL_INFER_DECL, name.span);
        local.name = name.text;
        array_add(*node.declarations, local);
        match(ps, .COLON);
    }

    expect(ps, .EQUAL, "Expected '=' after local declaration assignment");
    node.rhs = parse_expression_list(ps);
    match(ps, .SEMICOLON);
    return node;
}

parse_mixed_assignment_declaration :: (ps: *Parse_State) -> *Node {
    first_name := expect(ps, .IDENTIFIER, "Expected assignment name");
    node := new_node(.LOCAL_GROUP_DECL, first_name.span);

    while !check(ps, .COLON_EQUAL) && !check(ps, .SEMICOLON) && !check(ps, .EOF) && !ps.has_error {
        advance(ps);
    }

    if match(ps, .COLON_EQUAL) {
        node.rhs = parse_expression_list(ps);
    }

    match(ps, .SEMICOLON);
    return node;
}

parse_local_declaration_assignment :: (ps: *Parse_State) -> *Node {
    first_name := expect(ps, .IDENTIFIER, "Expected local name");
    expect(ps, .COLON, "Expected ':' after local declaration name");

    node := new_node(.LOCAL_GROUP_DECL, first_name.span);
    local := new_node(.LOCAL_INFER_DECL, first_name.span);
    local.name = first_name.text;
    array_add(*node.declarations, local);

    while !check(ps, .EQUAL) && !check(ps, .SEMICOLON) && !check(ps, .EOF) && !ps.has_error {
        advance(ps);
    }

    expect(ps, .EQUAL, "Expected '=' after local declaration assignment");
    node.rhs = parse_expression_list(ps);
    match(ps, .SEMICOLON);
    return node;
}

parse_local_colon_colon_declaration :: (ps: *Parse_State) -> *Node {
    name_token := expect(ps, .IDENTIFIER, "Expected local declaration name");
    expect(ps, .COLON_COLON, "Expected '::' after local declaration name");

    if match(ps, .STRUCT) || match(ps, .UNION) return parse_struct_after_header(ps, name_token);
    if match(ps, .ENUM) || match(ps, .ENUM_FLAGS) return parse_enum_after_header(ps, name_token);
    skip_declaration_modifiers(ps);
    if check(ps, .OPEN_PAREN) && is_function_header_ahead(ps) return parse_function_after_header(ps, name_token);

    node := new_node(.CONST_DECL, name_token.span);
    node.name = name_token.text;
    node.rhs = parse_expression(ps);
    match(ps, .SEMICOLON);
    return node;
}

parse_if_statement :: (ps: *Parse_State, if_token: Token) -> *Node {
    node := new_node(.IF_STATEMENT, if_token.span);

    scan := ps.index;
    while scan < ps.tokens.count && ps.tokens[scan].kind != .EOF {
        if ps.tokens[scan].kind == .OPEN_BRACE {
            if scan > ps.index && ps.tokens[scan - 1].kind == .EQUAL_EQUAL {
                while !check(ps, .OPEN_BRACE) && !check(ps, .EOF) && !ps.has_error {
                    advance(ps);
                }

                if match(ps, .OPEN_BRACE) {
                    node.body = parse_block_after_open(ps, previous(ps));
                    return node;
                }
            }
            break;
        }
        if ps.tokens[scan].kind == .THEN || ps.tokens[scan].kind == .SEMICOLON || ps.tokens[scan].kind == .CLOSE_BRACE break;
        scan += 1;
    }

    node.lhs = parse_expression(ps);

    if match(ps, .THEN) {
        stmt := parse_statement(ps);
        if stmt {
            block := new_node(.BLOCK, stmt.span);
            array_add(*block.statements, stmt);
            node.body = block;
        }
    } else if match(ps, .OPEN_BRACE) {
        node.body = parse_block_after_open(ps, previous(ps));
    } else {
        stmt := parse_statement(ps);
        if stmt {
            block := new_node(.BLOCK, stmt.span);
            array_add(*block.statements, stmt);
            node.body = block;
        }
    }

    if match(ps, .ELSE) {
        if match(ps, .OPEN_BRACE) {
            node.rhs = parse_block_after_open(ps, previous(ps));
        } else if match(ps, .IF) {
            node.rhs = parse_if_statement(ps, previous(ps));
        } else {
            stmt := parse_statement(ps);
            if stmt {
                block := new_node(.BLOCK, stmt.span);
                array_add(*block.statements, stmt);
                node.rhs = block;
            }
        }
    }

    match(ps, .SEMICOLON);
    return node;
}

parse_for_statement :: (ps: *Parse_State, for_token: Token) -> *Node {
    node := new_node(.FOR_STATEMENT, for_token.span);

    skip_for_modifiers(ps);

    if is_for_iterator_header_ahead(ps) {
        node.name = parse_for_iterator_names(ps, node);
        expect(ps, .COLON, "Expected ':' after for iterator name");
        node.lhs = parse_expression(ps);
    } else {
        node.lhs = parse_expression(ps);
    }

    if match(ps, .OPEN_BRACE) {
        node.body = parse_block_after_open(ps, previous(ps));
    } else {
        stmt := parse_statement(ps);
        if stmt {
            block := new_node(.BLOCK, stmt.span);
            array_add(*block.statements, stmt);
            node.body = block;
        }
    }

    match(ps, .SEMICOLON);
    return node;
}

skip_for_modifiers :: (ps: *Parse_State) {
    while !ps.has_error {
        if match(ps, .COLON) {
            if !check(ps, .OPEN_BRACE) && !check(ps, .IDENTIFIER) advance(ps);
            continue;
        }

        if match(ps, .LESS_EQUAL) || match(ps, .GREATER_EQUAL) || match(ps, .STAR_EQUAL) {
            parse_expression(ps);
            continue;
        }

        if check(ps, .STAR) && !is_pointer_iterator_start(ps) {
            advance(ps);
            continue;
        }

        if match(ps, .LESS) || match(ps, .GREATER) {
            continue;
        }

        break;
    }
}

is_pointer_iterator_start :: (ps: *Parse_State) -> bool {
    if !check(ps, .STAR) || !check_at(ps, 1, .IDENTIFIER) return false;
    return check_at(ps, 2, .COLON) || check_at(ps, 2, .COMMA);
}

is_for_iterator_header_ahead :: (ps: *Parse_State) -> bool {
    scan := ps.index;
    saw_name := false;

    while scan < ps.tokens.count {
        if ps.tokens[scan].kind == .BACKTICK scan += 1;
        if ps.tokens[scan].kind == .STAR {
            scan += 1;
            if scan >= ps.tokens.count || ps.tokens[scan].kind != .IDENTIFIER return false;
        } else if ps.tokens[scan].kind != .IDENTIFIER {
            return false;
        }

        saw_name = true;
        scan += 1;

        if scan >= ps.tokens.count return false;
        if ps.tokens[scan].kind == .COLON return saw_name;
        if ps.tokens[scan].kind != .COMMA return false;
        scan += 1;
    }

    return false;
}

parse_for_iterator_names :: (ps: *Parse_State, for_node: *Node) -> string {
    result := "";

    while !ps.has_error {
        match(ps, .BACKTICK);
        is_pointer := match(ps, .STAR);
        name := expect(ps, .IDENTIFIER, "Expected for iterator name");

        iterator := new_node(.LOCAL_INFER_DECL, name.span);
        iterator.name = name.text;
        array_add(*for_node.params, iterator);

        part := name.text;
        if is_pointer part = tprint("*%", part);
        if result.count == 0 result = part;
        else result = tprint("%,%", result, part);

        if !match(ps, .COMMA) break;
    }

    return result;
}

parse_while_statement :: (ps: *Parse_State, while_token: Token) -> *Node {
    node := new_node(.WHILE_STATEMENT, while_token.span);
    if has_colon_equal_before_statement_body(ps) {
        skip_until_statement_body(ps);
    } else {
        node.lhs = parse_expression(ps);
    }

    if match(ps, .OPEN_BRACE) {
        node.body = parse_block_after_open(ps, previous(ps));
    } else {
        stmt := parse_statement(ps);
        if stmt {
            block := new_node(.BLOCK, stmt.span);
            array_add(*block.statements, stmt);
            node.body = block;
        }
    }

    return node;
}

has_colon_equal_before_statement_body :: (ps: *Parse_State) -> bool {
    scan := ps.index;

    while scan < ps.tokens.count {
        kind := ps.tokens[scan].kind;
        if kind == .OPEN_BRACE || kind == .SEMICOLON || kind == .CLOSE_BRACE || kind == .EOF return false;
        if kind == .COLON_EQUAL return true;
        scan += 1;
    }

    return false;
}

skip_until_statement_body :: (ps: *Parse_State) {
    while !check(ps, .OPEN_BRACE) && !check(ps, .SEMICOLON) && !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) && !ps.has_error {
        advance(ps);
    }
}

parse_defer_statement :: (ps: *Parse_State, defer_token: Token) -> *Node {
    node := new_node(.DEFER_STATEMENT, defer_token.span);

    if match(ps, .OPEN_BRACE) {
        node.body = parse_block_after_open(ps, previous(ps));
        return node;
    }

    stmt := parse_statement(ps);
    if stmt {
        block := new_node(.BLOCK, stmt.span);
        array_add(*block.statements, stmt);
        node.body = block;
    }

    return node;
}

parse_compiler_directive_statement :: (ps: *Parse_State, hash_token: Token) -> *Node {
    node := new_node(.COMPILER_DIRECTIVE_STATEMENT, hash_token.span);
    if !check(ps, .EOF) {
        name := advance(ps);
        node.name = name.text;
    }

    if node.name == "if" {
        node.lhs = parse_expression(ps);
        if match(ps, .THEN) {
            stmt := parse_statement(ps);
            if stmt {
                block := new_node(.BLOCK, stmt.span);
                array_add(*block.statements, stmt);
                node.body = block;
            }
        } else if match(ps, .OPEN_BRACE) {
            node.body = parse_block_after_open(ps, previous(ps));
        } else {
            match(ps, .SEMICOLON);
            return node;
        }

        if match(ps, .ELSE) {
            if match(ps, .OPEN_BRACE) {
                node.rhs = parse_block_after_open(ps, previous(ps));
            } else if match(ps, .HASH) {
                node.rhs = parse_compiler_directive_statement(ps, previous(ps));
            } else if match(ps, .IF) {
                node.rhs = parse_if_statement(ps, previous(ps));
            } else {
                node.rhs = parse_statement(ps);
            }
        }
        return node;
    }

    while check(ps, .COMMA) && check_at(ps, 1, .IDENTIFIER) {
        advance(ps);
        advance(ps);
    }

    // Assembly uses its own grammar inside the braces (register suffixes,
    // mappings such as `name:`, and instruction operands). Keep the balanced
    // body opaque to the Jai parser so an otherwise valid #asm block cannot
    // make semantic analysis fail for the remainder of the source file.
    if node.name == "asm" && match(ps, .OPEN_BRACE) {
        skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
        match(ps, .SEMICOLON);
        return node;
    }

    if match(ps, .ARROW) {
        node.return_type = parse_return_type(ps);
        if match(ps, .OPEN_BRACE) {
            node.body = parse_block_after_open(ps, previous(ps));
        } else if can_start_primary_expression(ps) {
            node.rhs = parse_expression(ps);
        }
        match(ps, .SEMICOLON);
        return node;
    }

    if match(ps, .OPEN_BRACE) {
        node.body = parse_block_after_open(ps, previous(ps));
        match(ps, .SEMICOLON);
        return node;
    }

    if can_start_primary_expression(ps) {
        node.lhs = parse_expression_list(ps);
        match(ps, .SEMICOLON);
        return node;
    }

    while !check(ps, .SEMICOLON) && !check(ps, .CLOSE_BRACE) && !check(ps, .EOF) && !ps.has_error {
        if match(ps, .OPEN_BRACE) {
            skip_balanced_tokens(ps, .OPEN_BRACE, .CLOSE_BRACE);
            continue;
        }
        advance(ps);
    }

    match(ps, .SEMICOLON);
    return node;
}

parse_return_statement :: (ps: *Parse_State, return_token: Token) -> *Node {
    node := new_node(.RETURN_STATEMENT, return_token.span);
    if !check(ps, .SEMICOLON) && !check(ps, .CLOSE_BRACE) {
        node.rhs = parse_return_expression_list(ps);
    }
    match(ps, .SEMICOLON);
    return node;
}

parse_return_expression_list :: (ps: *Parse_State) -> *Node {
    first := parse_call_argument(ps);
    if first == null return null;
    if !match(ps, .COMMA) return first;

    node := new_node(.EXPRESSION_LIST, first.span);
    array_add(*node.args, first);

    while !ps.has_error {
        if check(ps, .SEMICOLON) || check(ps, .CLOSE_BRACE) break;
        next := parse_call_argument(ps);
        if next array_add(*node.args, next);
        if !match(ps, .COMMA) break;
    }

    return node;
}

parse_local_infer_decl :: (ps: *Parse_State) -> *Node {
    name_token := expect(ps, .IDENTIFIER, "Expected local name");
    declaration_token := expect(ps, .COLON_EQUAL, "Expected ':=' after local name");

    node := new_node(.LOCAL_INFER_DECL, name_token.span);
    node.name = name_token.text;

    // During an edit, `name :=` followed by a previously valid declaration on
    // the next line has no initializer yet. Do not steal that declaration as
    // the initializer merely because newlines are otherwise whitespace in Jai.
    if current(ps).span.line > declaration_token.span.line && parser_token_starts_local_declaration(ps) {
        set_error(ps, "Expected initializer after ':='", declaration_token.span);
        node.rhs = new_node(.ERROR, declaration_token.span);
        return node;
    }

    node.rhs = parse_expression(ps);
    match(ps, .SEMICOLON);
    return node;
}

parse_local_typed_decl :: (ps: *Parse_State) -> *Node {
    name_token := expect(ps, .IDENTIFIER, "Expected local name");
    declaration_token := expect(ps, .COLON, "Expected ':' after local name");

    node := new_node(.LOCAL_TYPED_DECL, name_token.span);
    node.name = name_token.text;

    // Preserve a valid following statement while the user has only typed the
    // declaration colon. A genuine multi-line type does not itself begin with
    // another local declaration marker.
    if current(ps).span.line > declaration_token.span.line && parser_token_starts_local_declaration(ps) {
        set_error(ps, "Expected type after ':'", declaration_token.span);
        node.type_node = new_node(.ERROR, declaration_token.span);
        return node;
    }

    node.type_node = parse_type(ps);
    if match(ps, .EQUAL) || match(ps, .COLON) node.rhs = parse_expression(ps);
    match(ps, .SEMICOLON);
    return node;
}

parse_local_group_decl :: (ps: *Parse_State) -> *Node {
    first_name := expect(ps, .IDENTIFIER, "Expected local name");

    node := new_node(.LOCAL_GROUP_DECL, first_name.span);
    names: [..] Token;
    array_add(*names, first_name);

    while match(ps, .COMMA) {
        name := expect(ps, .IDENTIFIER, "Expected local name after ','");
        array_add(*names, name);
    }

    if match(ps, .COLON_EQUAL) {
        value := parse_expression_list(ps);
        node.rhs = value;
        match(ps, .SEMICOLON);

        for names {
            local := new_node(.LOCAL_INFER_DECL, it.span);
            local.name = it.text;
            local.rhs = value;
            array_add(*node.declarations, local);
        }

        return node;
    } else {
        expect(ps, .COLON, "Expected ':' or ':=' after local names");
        value_type := parse_type(ps);

        default_value: *Node = null;
        if match(ps, .EQUAL) default_value = parse_expression(ps);
        node.rhs = default_value;
        match(ps, .SEMICOLON);

        for names {
            local := new_node(.LOCAL_TYPED_DECL, it.span);
            local.name = it.text;
            local.type_node = value_type;
            local.rhs = default_value;
            array_add(*node.declarations, local);
        }
    }

    return node;
}

parse_expression :: (ps: *Parse_State) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.EXPRESSION] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.EXPRESSION] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.EXPRESSION] += ps.index - profile_start_index;
        }
    }

    if check(ps, .IDENTIFIER) && check_at(ps, 1, .FAT_ARROW) return parse_single_name_lambda(ps);
    return parse_binary_expression(ps, 0);
}

parse_single_name_lambda :: (ps: *Parse_State) -> *Node {
    name := expect(ps, .IDENTIFIER, "Expected lambda parameter name");
    expect(ps, .FAT_ARROW, "Expected '=>' after lambda parameter name");

    node := new_node(.LAMBDA_EXPR, name.span);
    param := new_node(.PARAM, name.span);
    param.name = name.text;
    array_add(*node.params, param);
    if match(ps, .OPEN_BRACE) node.body = parse_block_after_open(ps, previous(ps));
    else node.rhs = parse_expression(ps);
    return node;
}

parse_expression_list :: (ps: *Parse_State) -> *Node {
    first := parse_expression(ps);
    if first == null return null;
    if !match(ps, .COMMA) return first;

    node := new_node(.EXPRESSION_LIST, first.span);
    array_add(*node.args, first);

    while !ps.has_error {
        if check(ps, .SEMICOLON) || check(ps, .CLOSE_PAREN) || check(ps, .CLOSE_BRACE) || check(ps, .CLOSE_BRACKET) break;
        next := parse_expression(ps);
        if next array_add(*node.args, next);
        if !match(ps, .COMMA) break;
    }

    return node;
}

parse_binary_expression :: (ps: *Parse_State, min_precedence: int) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.BINARY_EXPRESSION] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.BINARY_EXPRESSION] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.BINARY_EXPRESSION] += ps.index - profile_start_index;
        }
    }

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
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.UNARY_EXPRESSION] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.UNARY_EXPRESSION] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.UNARY_EXPRESSION] += ps.index - profile_start_index;
        }
    }

    if match(ps, .XX) {
        op := previous(ps);
        while match(ps, .COMMA) {
            expect(ps, .IDENTIFIER, "Expected xx modifier after ','");
        }

        node := new_node(.UNARY_EXPR, op.span);
        node.op = op.kind;
        node.rhs = parse_unary_expression(ps);
        return node;
    }

    if match(ps, .PLUS) || match(ps, .MINUS) || match(ps, .STAR) || match(ps, .BANG) || match(ps, .TILDE) || match(ps, .XX) || match(ps, .DOT_DOT) || match(ps, .BACKTICK) || match(ps, .LESS_LESS) || match(ps, .GREATER_GREATER) {
        op := previous(ps);
        node := new_node(.UNARY_EXPR, op.span);
        node.op = op.kind;
        node.rhs = parse_unary_expression(ps);
        return node;
    }

    return parse_postfix_expression(ps);
}

parse_postfix_expression :: (ps: *Parse_State) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.POSTFIX_EXPRESSION] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.POSTFIX_EXPRESSION] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.POSTFIX_EXPRESSION] += ps.index - profile_start_index;
        }
    }

    expr := parse_primary_expression(ps);

    while !ps.has_error {
        if match(ps, .DOT) {
            dot := previous(ps);
            if match(ps, .OPEN_BRACE) {
                expr = parse_struct_literal_after_open(ps, dot, expr);
                continue;
            }

            if match(ps, .OPEN_BRACKET) {
                expr = parse_array_literal_after_open(ps, dot, expr);
                continue;
            }

            if match(ps, .OPEN_PAREN) {
                node := new_node(.CAST_EXPR, dot.span);
                node.lhs = expr;
                node.type_node = parse_type(ps);
                expect(ps, .CLOSE_PAREN, "Expected ')' after postfix cast type");
                expr = node;
                continue;
            }

            if match(ps, .STAR) {
                node := new_node(.UNARY_EXPR, dot.span);
                node.op = .STAR;
                node.rhs = expr;
                expr = node;
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
                previous_was_comma := false;
                while !ps.has_error {
                    // An unfinished call immediately before a valid local
                    // declaration is one of the most common transient editor
                    // states. The declaration cannot be a call argument, so
                    // leave it for the surrounding block's recovery point.
                    if current(ps).span.line > open.span.line && parser_token_starts_local_declaration(ps) {
                        set_error(ps, "Expected ')' after call arguments", open.span);
                        break;
                    }

                    if match(ps, .COMMA) {
                        if previous_was_comma && node.implicit_arg_start < 0 node.implicit_arg_start = node.args.count;
                        previous_was_comma = true;
                        if check(ps, .CLOSE_PAREN) break;
                        continue;
                    }

                    previous_was_comma = false;
                    arg := parse_call_argument(ps);
                    if arg array_add(*node.args, arg);
                    if !match(ps, .COMMA) break;
                    previous_was_comma = true;
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

parse_call_argument :: (ps: *Parse_State) -> *Node {
    if check(ps, .IDENTIFIER) && check_at(ps, 1, .EQUAL) {
        name := advance(ps);
        expect(ps, .EQUAL, "Expected '=' after named argument");

        node := new_node(.NAMED_ARGUMENT_EXPR, name.span);
        node.name = name.text;
        node.rhs = parse_expression(ps);
        return node;
    }

    return parse_expression(ps);
}

parse_primary_expression :: (ps: *Parse_State) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start_index := ps.index;
        profile_start := current_time_monotonic();
        if parser_profile_enabled parser_profile_counts[cast(int) Parser_Profile_Slot.PRIMARY_EXPRESSION] += 1;
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.PRIMARY_EXPRESSION] += to_float64_seconds(current_time_monotonic() - profile_start);
            parser_profile_tokens_consumed[cast(int) Parser_Profile_Slot.PRIMARY_EXPRESSION] += ps.index - profile_start_index;
        }
    }

    if match(ps, .IFX) {
        ifx_token := previous(ps);
        node := new_node(.IFX_EXPR, ifx_token.span);
        node.lhs = parse_expression(ps);
        if match(ps, .OPEN_BRACE) {
            node.rhs = parse_block_after_open(ps, previous(ps));
            expect(ps, .ELSE, "Expected 'else' in ifx expression");
            if match(ps, .OPEN_BRACE) node.body = parse_block_after_open(ps, previous(ps));
            else node.body = parse_expression(ps);
            return node;
        }

        if match(ps, .ELSE) {
            node.body = parse_expression(ps);
            return node;
        }

        if !check(ps, .THEN) {
            node.rhs = parse_expression(ps);
            expect(ps, .ELSE, "Expected 'else' in ifx expression");
            node.body = parse_expression(ps);
            return node;
        }

        expect(ps, .THEN, "Expected 'then' in ifx expression");
        node.rhs = parse_expression(ps);
        if match(ps, .ELSE) node.body = parse_expression(ps);
        return node;
    }

    if match(ps, .HASH) {
        hash_token := previous(ps);
        node := new_node(.COMPILER_DIRECTIVE_EXPR, hash_token.span);
        name := advance(ps);
        node.name = name.text;

        while check(ps, .COMMA) && check_at(ps, 1, .IDENTIFIER) && !check_at(ps, 2, .COLON) && !check_at(ps, 2, .COLON_EQUAL) {
            advance(ps);
            advance(ps);
        }

        if node.name == "string" {
            delimiter := expect(ps, .IDENTIFIER, "Expected #string delimiter");
            while !check(ps, .EOF) && !ps.has_error {
                if current(ps).kind == .IDENTIFIER && current(ps).text == delimiter.text {
                    advance(ps);
                    break;
                }
                advance(ps);
            }
            return node;
        }

        if node.name == "type" {
            node.type_node = parse_type(ps);
            return node;
        }

        if node.name == "ifx" {
            node.lhs = parse_expression(ps);
            if match(ps, .OPEN_BRACE) {
                node.rhs = parse_block_after_open(ps, previous(ps));
                expect(ps, .ELSE, "Expected 'else' in #ifx expression");
                if match(ps, .OPEN_BRACE) node.body = parse_block_after_open(ps, previous(ps));
                else node.body = parse_expression(ps);
                return node;
            }

            if match(ps, .THEN) node.rhs = parse_expression(ps);
            expect(ps, .ELSE, "Expected 'else' in #ifx expression");
            node.body = parse_expression(ps);
            return node;
        }

        if match(ps, .ARROW) {
            node.return_type = parse_return_type(ps);
        }

        if match(ps, .OPEN_BRACE) {
            node.body = parse_block_after_open(ps, previous(ps));
            return node;
        }

        if match(ps, .OPEN_PAREN) {
            if !check(ps, .CLOSE_PAREN) {
                while !ps.has_error {
                    arg := parse_call_argument(ps);
                    if arg array_add(*node.args, arg);
                    if !match(ps, .COMMA) break;
                    if check(ps, .CLOSE_PAREN) break;
                }
            }

            expect(ps, .CLOSE_PAREN, "Expected ')' after compiler directive arguments");
            if can_start_primary_expression(ps) node.rhs = parse_expression(ps);
            return node;
        }

        if can_start_primary_expression(ps) {
            // Directives such as `#run procedure(argument)` apply to the
            // complete following expression, including postfix calls and
            // binary operators, rather than only its first identifier.
            node.rhs = parse_expression(ps);
        }

        return node;
    }

    if match(ps, .CAST) {
        cast_token := previous(ps);
        node := new_node(.CAST_EXPR, cast_token.span);
        while match(ps, .COMMA) {
            expect(ps, .IDENTIFIER, "Expected cast modifier after ','");
        }
        expect(ps, .OPEN_PAREN, "Expected '(' after cast");
        node.type_node = parse_type(ps);
        if match(ps, .COMMA) {
            node.rhs = parse_expression_list(ps);
            expect(ps, .CLOSE_PAREN, "Expected ')' after cast expression");
            return node;
        }
        expect(ps, .CLOSE_PAREN, "Expected ')' after cast type");
        node.rhs = parse_unary_expression(ps);
        return node;
    }

    if match(ps, .DOT) {
        dot := previous(ps);
        if match(ps, .OPEN_BRACE) return parse_struct_literal_after_open(ps, dot, null);
        if match(ps, .OPEN_BRACKET) return parse_array_literal_after_open(ps, dot, null);
        if match(ps, .STAR) {
            node := new_node(.IDENTIFIER_EXPR, dot.span);
            node.name = ".*";
            return node;
        }

        name := expect(ps, .IDENTIFIER, "Expected enum value name after '.'");
        node := new_node(.ENUM_VALUE_EXPR, dot.span);
        node.name = name.text;
        return node;
    }

    if match(ps, .DOLLAR) {
        name := expect(ps, .IDENTIFIER, "Expected polymorphic expression name after '$'");
        node := new_node(.IDENTIFIER_EXPR, name.span);
        node.name = tprint("$%", name.text);
        return node;
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

    if match(ps, .TRIPLE_MINUS) {
        token := previous(ps);
        return new_node(.UNINITIALIZED_EXPR, token.span);
    }

    if check(ps, .STRUCT) || check(ps, .UNION) || check(ps, .ENUM) || check(ps, .ENUM_FLAGS) return parse_type(ps);

    if match(ps, .OPEN_BRACE) return parse_struct_literal_after_open(ps, previous(ps), null);

    if check(ps, .OPEN_BRACKET) return parse_type(ps);

    if match(ps, .OPEN_PAREN) {
        open := previous(ps);
        if is_lambda_after_open(ps) return parse_lambda_after_open(ps, open);

        if check(ps, .DOT) && check_at(ps, 1, .STAR) {
            advance(ps);
            expect(ps, .STAR, "Expected '*' after '(.' in dereference operator");
            expect(ps, .CLOSE_PAREN, "Expected ')' after '(.*' dereference operator");
            node := new_node(.UNARY_EXPR, open.span);
            node.op = .STAR;
            node.rhs = parse_unary_expression(ps);
            return node;
        }

        expr := parse_expression(ps);
        expect(ps, .CLOSE_PAREN, "Expected ')' after expression");
        return expr;
    }

    set_error(ps, "Expected expression", current(ps).span);
    return null;
}

can_start_primary_expression :: (ps: *Parse_State) -> bool {
    kind := current(ps).kind;
    return kind == .STRING_LITERAL ||
           kind == .INT_LITERAL ||
           kind == .FLOAT_LITERAL ||
           kind == .IDENTIFIER ||
           kind == .OPEN_PAREN ||
           kind == .DOT ||
           kind == .DOLLAR ||
           kind == .HASH ||
           kind == .CAST ||
           kind == .OPEN_BRACE ||
           kind == .OPEN_BRACKET;
}

is_lambda_after_open :: (ps: *Parse_State) -> bool {
    scan := ps.index;
    depth := 1;

    while scan < ps.tokens.count && depth > 0 {
        if ps.tokens[scan].kind == .OPEN_PAREN depth += 1;
        else if ps.tokens[scan].kind == .CLOSE_PAREN depth -= 1;
        scan += 1;
    }

    if depth != 0 || scan >= ps.tokens.count return false;
    if ps.tokens[scan].kind == .ARROW || ps.tokens[scan].kind == .FAT_ARROW return true;
    if ps.tokens[scan].kind == .OPEN_BRACE return is_lambda_parameter_list_ahead(ps);
    if ps.tokens[scan].kind == .HASH {
        while scan < ps.tokens.count && ps.tokens[scan].kind == .HASH {
            scan += 1;
            if scan < ps.tokens.count && ps.tokens[scan].kind == .IDENTIFIER scan += 1;
            if scan < ps.tokens.count && ps.tokens[scan].kind == .OPEN_PAREN {
                depth := 1;
                scan += 1;
                while scan < ps.tokens.count && depth > 0 {
                    if ps.tokens[scan].kind == .OPEN_PAREN depth += 1;
                    else if ps.tokens[scan].kind == .CLOSE_PAREN depth -= 1;
                    scan += 1;
                }
            }
        }

        if scan < ps.tokens.count && ps.tokens[scan].kind == .OPEN_BRACE return is_lambda_parameter_list_ahead(ps);
    }
    return false;
}

is_lambda_parameter_list_ahead :: (ps: *Parse_State) -> bool {
    scan := ps.index;
    depth := 1;
    saw_parameter_marker := false;

    if check(ps, .CLOSE_PAREN) return true;

    while scan < ps.tokens.count && depth > 0 {
        kind := ps.tokens[scan].kind;
        if kind == .OPEN_PAREN depth += 1;
        else if kind == .CLOSE_PAREN depth -= 1;
        else if depth == 1 && (kind == .COLON || kind == .COLON_EQUAL || kind == .USING || kind == .DOLLAR) saw_parameter_marker = true;
        scan += 1;
    }

    return saw_parameter_marker;
}

parse_lambda_after_open :: (ps: *Parse_State, open: Token) -> *Node {
    node := new_node(.LAMBDA_EXPR, open.span);

    if !check(ps, .CLOSE_PAREN) {
        while !ps.has_error {
            param := parse_lambda_param(ps);
            if param array_add(*node.params, param);

            if !match(ps, .COMMA) break;
            if check(ps, .CLOSE_PAREN) break;
        }
    }

    expect(ps, .CLOSE_PAREN, "Expected ')' after lambda parameters");
    skip_lambda_modifiers(ps);
    if match(ps, .OPEN_BRACE) {
        node.body = parse_block_after_open(ps, previous(ps));
    } else if match(ps, .FAT_ARROW) {
        node.rhs = parse_expression(ps);
    } else {
        expect(ps, .ARROW, "Expected '->' after lambda parameters");
        node.return_type = parse_return_type(ps);
        if match(ps, .OPEN_BRACE) node.body = parse_block_after_open(ps, previous(ps));
        else node.rhs = parse_expression(ps);
    }
    return node;
}

skip_lambda_modifiers :: (ps: *Parse_State) {
    while match(ps, .HASH) {
        expect(ps, .IDENTIFIER, "Expected lambda modifier after '#'");
        if match(ps, .OPEN_PAREN) skip_balanced_tokens(ps, .OPEN_PAREN, .CLOSE_PAREN);
    }
}

parse_lambda_param :: (ps: *Parse_State) -> *Node {
    if check(ps, .IDENTIFIER) && (check_at(ps, 1, .COMMA) || check_at(ps, 1, .CLOSE_PAREN)) {
        name := advance(ps);
        node := new_node(.PARAM, name.span);
        node.name = name.text;
        return node;
    }

    return parse_param(ps);
}

parse_array_literal_after_open :: (ps: *Parse_State, start_token: Token, type_expr: *Node) -> *Node {
    node := new_node(.ARRAY_LITERAL_EXPR, start_token.span);
    node.type_node = type_expr;

    if !check(ps, .CLOSE_BRACKET) {
        while !ps.has_error {
            element := parse_expression(ps);
            if element array_add(*node.args, element);

            if !match(ps, .COMMA) break;
            if check(ps, .CLOSE_BRACKET) break;
        }
    }

    expect(ps, .CLOSE_BRACKET, "Expected ']' after array literal");
    return node;
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
            } else if is_struct_literal_designator_ahead(ps) {
                entry.lhs = parse_expression(ps);
                expect(ps, .EQUAL, "Expected '=' after struct literal field designator");
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

is_struct_literal_designator_ahead :: (ps: *Parse_State) -> bool {
    scan := ps.index;
    paren_depth := 0;
    bracket_depth := 0;
    brace_depth := 0;

    while scan < ps.tokens.count {
        kind := ps.tokens[scan].kind;
        if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 {
            if kind == .EQUAL return true;
            if kind == .COMMA || kind == .CLOSE_BRACE return false;
        }

        if kind == .OPEN_PAREN paren_depth += 1;
        else if kind == .CLOSE_PAREN paren_depth -= 1;
        else if kind == .OPEN_BRACKET bracket_depth += 1;
        else if kind == .CLOSE_BRACKET bracket_depth -= 1;
        else if kind == .OPEN_BRACE brace_depth += 1;
        else if kind == .CLOSE_BRACE {
            if brace_depth == 0 return false;
            brace_depth -= 1;
        }

        scan += 1;
    }

    return false;
}

parse_function_type :: (ps: *Parse_State, allow_unparenthesized_return_list: bool = true) -> *Node {
    open := expect(ps, .OPEN_PAREN, "Expected '(' before function type parameters");
    node := new_node(.FUNCTION_TYPE, open.span);

    if !check(ps, .CLOSE_PAREN) {
        while !ps.has_error {
            param := parse_function_type_param(ps);
            if param array_add(*node.params, param);

            if !match(ps, .COMMA) break;
            if check(ps, .CLOSE_PAREN) break;
        }
    }

    expect(ps, .CLOSE_PAREN, "Expected ')' after function type parameters");
    if match(ps, .ARROW) {
        if allow_unparenthesized_return_list {
            node.return_type = parse_return_type(ps);
        } else if check(ps, .OPEN_PAREN) && is_parenthesized_return_type_list_ahead(ps) {
            node.return_type = parse_parenthesized_return_type_list(ps);
        } else {
            node.return_type = parse_return_type_item(ps);
        }
    }
    skip_type_suffix_directives(ps);
    return node;
}

parse_function_type_param :: (ps: *Parse_State) -> *Node {
    if check(ps, .IDENTIFIER) && (check_at(ps, 1, .COLON) || check_at(ps, 1, .COLON_EQUAL)) return parse_param(ps);
    if check(ps, .USING) || check(ps, .DOLLAR) return parse_param(ps);

    node := new_node(.PARAM, current(ps).span);
    node.type_node = parse_type(ps);
    return node;
}

binary_precedence :: (kind: Token_Kind) -> int {
    if kind == .STAR || kind == .SLASH || kind == .PERCENT return 20;
    if kind == .PLUS || kind == .MINUS return 10;
    if kind == .LESS_LESS || kind == .GREATER_GREATER || kind == .LESS_LESS_LESS || kind == .GREATER_GREATER_GREATER return 9;
    if kind == .AMPERSAND return 8;
    if kind == .CARET return 7;
    if kind == .PIPE return 6;
    if kind == .EQUAL_EQUAL || kind == .BANG_EQUAL || kind == .LESS || kind == .LESS_EQUAL || kind == .GREATER || kind == .GREATER_EQUAL return 5;
    if kind == .AND_AND return 4;
    if kind == .OR_OR return 3;
    if kind == .DOT_DOT return 2;
    return -1;
}

new_node :: (kind: Node_Kind, span: Source_Span) -> *Node {
    #if PARSER_PROFILE_ENABLED {
        profile_start := current_time_monotonic();
        if parser_profile_enabled {
            parser_profile_counts[cast(int) Parser_Profile_Slot.NEW_NODE] += 1;
            parser_profile_node_counts[cast(int) kind] += 1;
        }
        defer if parser_profile_enabled {
            parser_profile_seconds[cast(int) Parser_Profile_Slot.NEW_NODE] += to_float64_seconds(current_time_monotonic() - profile_start);
        }
    }

    node := New(Node);
    node.kind = kind;
    node.span = span;
    node.implicit_arg_start = -1;
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

advance :: inline (ps: *Parse_State) -> Token {
    if !check(ps, .EOF) ps.index += 1;
    return previous(ps);
}

match :: inline (ps: *Parse_State, kind: Token_Kind) -> bool {
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

expect_name_token :: (ps: *Parse_State, message: string) -> Token {
    if check(ps, .IDENTIFIER) || check(ps, .INTERFACE) return advance(ps);

    token := current(ps);
    set_error(ps, message, token.span);
    return token;
}

parser_token_can_start_statement :: (kind: Token_Kind) -> bool {
    return kind == .IDENTIFIER || kind == .RETURN || kind == .IF || kind == .FOR ||
           kind == .WHILE || kind == .DEFER || kind == .USING || kind == .STRUCT ||
           kind == .UNION || kind == .HASH || kind == .AT || kind == .BACKTICK ||
           kind == .OPEN_BRACE || kind == .CAST || kind == .IFX;
}

parser_token_starts_local_declaration :: (ps: *Parse_State) -> bool {
    if !check(ps, .IDENTIFIER) return false;
    return check_at(ps, 1, .COLON) || check_at(ps, 1, .COLON_EQUAL) ||
           check_at(ps, 1, .COLON_COLON);
}

parser_token_can_start_top_level_item :: (ps: *Parse_State) -> bool {
    kind := current(ps).kind;
    if kind == .HASH || kind == .AT || kind == .USING || kind == .ELSE return true;
    if kind != .IDENTIFIER && kind != .INTERFACE return false;

    // A declaration separator on the same source line is a strong recovery
    // anchor.  This avoids treating an identifier in a broken expression as a
    // new file item merely because it starts a new visual line.
    scan := ps.index + 1;
    line := current(ps).span.line;
    while scan < ps.tokens.count && ps.tokens[scan].span.line == line {
        kind = ps.tokens[scan].kind;
        if kind == .COLON || kind == .COLON_COLON return true;
        if kind == .SEMICOLON || kind == .OPEN_BRACE || kind == .CLOSE_BRACE return false;
        scan += 1;
    }
    return false;
}

parser_recover_pending_error :: (ps: *Parse_State, start_index: int, top_level := false) -> *Node {
    if !ps.has_error return null;

    error_line := ps.error_span.line;
    paren_depth := 0;
    bracket_depth := 0;
    brace_depth := 0;

    while !check(ps, .EOF) {
        kind := current(ps).kind;
        at_boundary_depth := paren_depth == 0 && bracket_depth == 0 && brace_depth == 0;

        if at_boundary_depth {
            if kind == .CLOSE_BRACE break;
            if kind == .SEMICOLON {
                advance(ps);
                break;
            }

            later_line := current(ps).span.line > error_line;
            if later_line {
                if top_level && parser_token_can_start_top_level_item(ps) break;
                if !top_level && parser_token_can_start_statement(kind) break;
            }
        }

        if kind == .OPEN_PAREN {
            paren_depth += 1;
        } else if kind == .CLOSE_PAREN {
            if paren_depth > 0 paren_depth -= 1;
        } else if kind == .OPEN_BRACKET {
            bracket_depth += 1;
        } else if kind == .CLOSE_BRACKET {
            if bracket_depth > 0 bracket_depth -= 1;
        } else if kind == .OPEN_BRACE {
            brace_depth += 1;
        } else if kind == .CLOSE_BRACE {
            if brace_depth > 0 brace_depth -= 1;
        }
        advance(ps);
    }

    start_token := ifx start_index >= 0 && start_index < ps.tokens.count then ps.tokens[start_index] else current(ps);
    end_position := start_token.span.start_position_in_input + max(start_token.span.count, 1);
    if ps.index > start_index && ps.index > 0 {
        end_token := previous(ps);
        end_position = max(end_position, end_token.span.start_position_in_input + end_token.span.count);
    }

    error_node := new_node(.ERROR, start_token.span);
    error_node.span.count = max(end_position - error_node.span.start_position_in_input, 1);

    ps.has_error = false;
    ps.error_message = "";
    ps.error_span = .{};
    return error_node;
}

set_error :: (ps: *Parse_State, message: string, span: Source_Span) {
    if ps.has_error return;
    ps.has_error = true;
    ps.error_message = message;
    ps.error_span = span;

    error := Error.{ message = message, span = span };
    if ps.error_count < ps.errors.count ps.errors[ps.error_count] = error;
    ps.error_count += 1;
}
