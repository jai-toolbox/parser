/*
The lexer is a single-pass scanner over a Jai source string. It keeps a
position cursor plus line and column counters for diagnostics.
Calling next_token first skips whitespace and comments, then looks at the current byte to choose
a small token-consuming routine: identifiers/keywords, numbers, strings, one-character
tokens, or two-character operator tokens.

Each token stores its kind, the original source slice, and the source span where
it began. The lexer does not allocate copied token text; token strings point
back into the input string. When it finds an unexpected character or unterminated
string, it records the first error and lex stops after returning the failed
result.
*/

Lexer :: struct {
    source_code : string;
    position : int;
    line          : int;
    column        : int;
    has_error     : bool;
    error_message : string;
    error_span    : Source_Span;
}

lex :: (input: string) -> Lexer_Result {
    lexer: Lexer;
    init(*lexer, input);
    result: Lexer_Result;
    array_reserve(*result.tokens, input.count / 4);

    while !lexer.has_error {
        token := next_token(*lexer);
        array_add(*result.tokens, token);
        if token.kind == .EOF break;
    }

    if lexer.has_error {
        result.success = false;
        result.error = .{ message = lexer.error_message, span = lexer.error_span };
        return result;
    }

    result.success = true;
    return result;
}

init :: (lexer: *Lexer, input: string) {
    lexer.source_code = input;
    lexer.position = 0;
    lexer.line = 1;
    lexer.column = 1;
    lexer.has_error = false;
    lexer.error_message = "";
    lexer.error_span = .{};
}

remaining :: inline (lexer: *Lexer) -> int {
    return lexer.source_code.count - lexer.position;
}

// get character at current position
peek :: inline (lexer: *Lexer) -> u8 {
    if lexer.position >= lexer.source_code.count return 0;
    return lexer.source_code[lexer.position];
}

peek_at :: inline (lexer: *Lexer, ahead: int) -> u8 {
    idx := lexer.position + ahead;
    if idx >= lexer.source_code.count return 0;
    return lexer.source_code[idx];
}

advance :: inline (lexer: *Lexer) -> u8 {
    if lexer.position >= lexer.source_code.count return 0;
    c := lexer.source_code[lexer.position];
    lexer.position += 1;

    if c == #char "\n" {
        lexer.line += 1;
        lexer.column = 1;
    } else {
        lexer.column += 1;
    }

    return c;
}

advance_n :: (lexer: *Lexer, n: int) {
    for 0..n-1 advance(lexer);
}

make_span :: (start_position_in_input: int, end_position_in_input: int, line: int, column: int) -> Source_Span {
    return .{ start_position_in_input = start_position_in_input, count = end_position_in_input - start_position_in_input, line = line, column = column };
}

span_text :: (input: string, span: Source_Span) -> string {
    result: string;
    result.data = input.data + span.start_position_in_input;
    result.count = span.count;
    return result;
}

set_error :: (lexer: *Lexer, message: string, span: Source_Span) {
    if lexer.has_error return;
    lexer.has_error = true;
    lexer.error_message = message;
    lexer.error_span = span;
}

next_token :: (lexer: *Lexer) -> Token {
    skip_whitespace_and_comments(lexer);

    start_position_in_input := lexer.position;
    start_line := lexer.line;
    start_column := lexer.column;

    if remaining(lexer) == 0 {
        span := make_span(start_position_in_input, start_position_in_input, start_line, start_column);
        return .{ kind = .EOF, text = "", span = span };
    }

    c := peek(lexer);

    if is_identifier_start(c) return consume_identifier_or_keyword_token(lexer);
    if is_digit(c) return consume_number_token(lexer);
    if c == #char "." && is_digit(peek_at(lexer, 1)) return consume_number_token(lexer);
    if c == #char "\"" return consume_string_token(lexer);

    if c == #char "(" return consume_one_character_token(lexer, .OPEN_PAREN);
    if c == #char ")" return consume_one_character_token(lexer, .CLOSE_PAREN);
    if c == #char "{" return consume_one_character_token(lexer, .OPEN_BRACE);
    if c == #char "}" return consume_one_character_token(lexer, .CLOSE_BRACE);
    if c == #char "[" return consume_one_character_token(lexer, .OPEN_BRACKET);
    if c == #char "]" return consume_one_character_token(lexer, .CLOSE_BRACKET);
    if c == #char ";" return consume_one_character_token(lexer, .SEMICOLON);
    if c == #char "," return consume_one_character_token(lexer, .COMMA);
    if c == #char "." {
        if peek_at(lexer, 1) == #char "." return consume_two_character_token(lexer, .DOT_DOT);
        return consume_one_character_token(lexer, .DOT);
    }
    if c == #char "$" return consume_one_character_token(lexer, .DOLLAR);
    if c == #char "#" {
        if starts_with_at_current_position(lexer, "#string") return consume_raw_string_token(lexer);
        return consume_one_character_token(lexer, .HASH);
    }
    if c == #char "`" return consume_one_character_token(lexer, .BACKTICK);
    if c == #char "@" return consume_one_character_token(lexer, .AT);
    if c == #char "?" return consume_one_character_token(lexer, .QUESTION);
    if c == #char "*" {
        if lexer.position > 0 && lexer.source_code[lexer.position - 1] == #char "." && peek_at(lexer, 1) == #char "=" return consume_one_character_token(lexer, .STAR);
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .STAR_EQUAL);
        return consume_one_character_token(lexer, .STAR);
    }
    if c == #char "/" {
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .SLASH_EQUAL);
        return consume_one_character_token(lexer, .SLASH);
    }
    if c == #char "%" {
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .PERCENT_EQUAL);
        return consume_one_character_token(lexer, .PERCENT);
    }
    if c == #char "&" {
        if peek_at(lexer, 1) == #char "&" && peek_at(lexer, 2) == #char "=" return consume_three_character_token(lexer, .AND_AND_EQUAL);
        if peek_at(lexer, 1) == #char "&" return consume_two_character_token(lexer, .AND_AND);
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .AMPERSAND_EQUAL);
        return consume_one_character_token(lexer, .AMPERSAND);
    }

    if c == #char "^" {
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .CARET_EQUAL);
        return consume_one_character_token(lexer, .CARET);
    }

    if c == #char "|" {
        if peek_at(lexer, 1) == #char "|" && peek_at(lexer, 2) == #char "=" return consume_three_character_token(lexer, .OR_OR_EQUAL);
        if peek_at(lexer, 1) == #char "|" return consume_two_character_token(lexer, .OR_OR);
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .PIPE_EQUAL);
        return consume_one_character_token(lexer, .PIPE);
    }
    if c == #char "~" return consume_one_character_token(lexer, .TILDE);

    if c == #char "=" {
        if peek_at(lexer, 1) == #char ">" return consume_two_character_token(lexer, .FAT_ARROW);
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .EQUAL_EQUAL);
        return consume_one_character_token(lexer, .EQUAL);
    }

    if c == #char "!" {
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .BANG_EQUAL);
        return consume_one_character_token(lexer, .BANG);
    }

    if c == #char "<" {
        if peek_at(lexer, 1) == #char "<" && peek_at(lexer, 2) == #char "=" return consume_three_character_token(lexer, .LESS_LESS_EQUAL);
        if peek_at(lexer, 1) == #char "<" && peek_at(lexer, 2) == #char "<" return consume_three_character_token(lexer, .LESS_LESS_LESS);
        if peek_at(lexer, 1) == #char "<" return consume_two_character_token(lexer, .LESS_LESS);
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .LESS_EQUAL);
        return consume_one_character_token(lexer, .LESS);
    }

    if c == #char ">" {
        if peek_at(lexer, 1) == #char ">" && peek_at(lexer, 2) == #char "=" return consume_three_character_token(lexer, .GREATER_GREATER_EQUAL);
        if peek_at(lexer, 1) == #char ">" && peek_at(lexer, 2) == #char ">" return consume_three_character_token(lexer, .GREATER_GREATER_GREATER);
        if peek_at(lexer, 1) == #char ">" return consume_two_character_token(lexer, .GREATER_GREATER);
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .GREATER_EQUAL);
        return consume_one_character_token(lexer, .GREATER);
    }

    if c == #char "+" {
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .PLUS_EQUAL);
        return consume_one_character_token(lexer, .PLUS);
    }

    if c == #char "-" {
        if peek_at(lexer, 1) == #char "-" && peek_at(lexer, 2) == #char "-" return consume_three_character_token(lexer, .TRIPLE_MINUS);
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .MINUS_EQUAL);
        if peek_at(lexer, 1) == #char ">" return consume_two_character_token(lexer, .ARROW);
        return consume_one_character_token(lexer, .MINUS);
    }

    if c == #char ":" {
        if peek_at(lexer, 1) == #char "=" return consume_two_character_token(lexer, .COLON_EQUAL);
        if peek_at(lexer, 1) == #char ":" return consume_two_character_token(lexer, .COLON_COLON);
        return consume_one_character_token(lexer, .COLON);
    }

    // if you got this far then we didn't match anything in the language so it's invalid

    span := make_span(start_position_in_input, start_position_in_input + 1, start_line, start_column);
    set_error(lexer, tprint("Unexpected character '%'", span_text(lexer.source_code, span)), span);
    advance(lexer);
    return .{ kind = .INVALID, text = span_text(lexer.source_code, span), span = span };
}

skip_whitespace_and_comments :: (lexer: *Lexer) {
    while remaining(lexer) > 0 {
        c := peek(lexer);

        if c == #char " " || c == #char "\t" || c == #char "\r" || c == #char "\n" {
            advance(lexer);
            continue;
        }

        if c == #char "/" && peek_at(lexer, 1) == #char "/" {
            start := lexer.position;
            while lexer.position < lexer.source_code.count && lexer.source_code[lexer.position] != #char "\n"
                lexer.position += 1;
            lexer.column += lexer.position - start;
            continue;
        }

        if c == #char "/" && peek_at(lexer, 1) == #char "*" {
            advance_n(lexer, 2);
            depth := 1;
            while remaining(lexer) > 0 && depth > 0 {
                if peek(lexer) == #char "/" && peek_at(lexer, 1) == #char "*" {
                    advance_n(lexer, 2);
                    depth += 1;
                    continue;
                }

                if peek(lexer) == #char "*" && peek_at(lexer, 1) == #char "/" {
                    advance_n(lexer, 2);
                    depth -= 1;
                    continue;
                }
                advance(lexer);
            }
            continue;
        }

        break;
    }
}

consume_one_character_token :: (lexer: *Lexer, kind: Token_Kind) -> Token {
    start_position_in_input := lexer.position;
    start_line := lexer.line;
    start_column := lexer.column;
    advance(lexer);
    span := make_span(start_position_in_input, lexer.position, start_line, start_column);
    return .{ kind = kind, text = span_text(lexer.source_code, span), span = span };
}

consume_two_character_token :: (lexer: *Lexer, kind: Token_Kind) -> Token {
    start_position_in_input := lexer.position;
    start_line := lexer.line;
    start_column := lexer.column;
    advance_n(lexer, 2);
    span := make_span(start_position_in_input, lexer.position, start_line, start_column);
    return .{ kind = kind, text = span_text(lexer.source_code, span), span = span };
}

consume_three_character_token :: (lexer: *Lexer, kind: Token_Kind) -> Token {
    start_position_in_input := lexer.position;
    start_line := lexer.line;
    start_column := lexer.column;
    advance_n(lexer, 3);
    span := make_span(start_position_in_input, lexer.position, start_line, start_column);
    return .{ kind = kind, text = span_text(lexer.source_code, span), span = span };
}

consume_identifier_or_keyword_token :: (lexer: *Lexer) -> Token {
    start_position_in_input := lexer.position;
    start_line := lexer.line;
    start_column := lexer.column;

    advance(lexer);
    while remaining(lexer) > 0 && (is_identifier_continue(peek(lexer)) || peek(lexer) == #char "\\") {
        if peek(lexer) == #char "\\" {
            advance(lexer);
            while remaining(lexer) > 0 && (peek(lexer) == #char " " || peek(lexer) == #char "\t" || peek(lexer) == #char "\r" || peek(lexer) == #char "\n")
                advance(lexer);
            continue;
        }

        advance(lexer);
    }

    span := make_span(start_position_in_input, lexer.position, start_line, start_column);
    text := span_text(lexer.source_code, span);
    kind := Token_Kind.IDENTIFIER;

    if text == "return" kind = .RETURN;
    else if text == "if" kind = .IF;
    else if text == "ifx" kind = .IFX;
    else if text == "then" kind = .THEN;
    else if text == "for" kind = .FOR;
    else if text == "while" kind = .WHILE;
    else if text == "defer" kind = .DEFER;
    else if text == "else" kind = .ELSE;
    else if text == "struct" kind = .STRUCT;
    else if text == "union" kind = .UNION;
    else if text == "enum" kind = .ENUM;
    else if text == "enum_flags" kind = .ENUM_FLAGS;
    else if text == "cast" kind = .CAST;
    else if text == "interface" kind = .INTERFACE;
    else if text == "using" kind = .USING;
    else if text == "xx" kind = .XX;

    return .{ kind = kind, text = text, span = span };
}

consume_number_token :: (lexer: *Lexer) -> Token {
    start_position_in_input := lexer.position;
    start_line := lexer.line;
    start_column := lexer.column;

    if peek(lexer) == #char "0" && (peek_at(lexer, 1) == #char "x" || peek_at(lexer, 1) == #char "X") {
        advance_n(lexer, 2);
        while remaining(lexer) > 0 && (is_hex_digit(peek(lexer)) || peek(lexer) == #char "_")
            advance(lexer);

        span := make_span(start_position_in_input, lexer.position, start_line, start_column);
        return .{ kind = .INT_LITERAL, text = span_text(lexer.source_code, span), span = span };
    }

    if peek(lexer) == #char "0" && (peek_at(lexer, 1) == #char "b" || peek_at(lexer, 1) == #char "B") {
        advance_n(lexer, 2);
        while remaining(lexer) > 0 && (peek(lexer) == #char "0" || peek(lexer) == #char "1" || peek(lexer) == #char "_")
            advance(lexer);

        span := make_span(start_position_in_input, lexer.position, start_line, start_column);
        return .{ kind = .INT_LITERAL, text = span_text(lexer.source_code, span), span = span };
    }

    if peek(lexer) == #char "0" && (peek_at(lexer, 1) == #char "h" || peek_at(lexer, 1) == #char "H") {
        advance_n(lexer, 2);
        while remaining(lexer) > 0 && (is_hex_digit(peek(lexer)) || peek(lexer) == #char "_")
            advance(lexer);

        span := make_span(start_position_in_input, lexer.position, start_line, start_column);
        return .{ kind = .FLOAT_LITERAL, text = span_text(lexer.source_code, span), span = span };
    }

    while remaining(lexer) > 0 && (is_digit(peek(lexer)) || peek(lexer) == #char "_")
        advance(lexer);

    kind := Token_Kind.INT_LITERAL;
    if remaining(lexer) > 0 && peek(lexer) == #char "." && peek_at(lexer, 1) != #char "." {
        kind = .FLOAT_LITERAL;
        advance(lexer);
        while remaining(lexer) > 0 && (is_digit(peek(lexer)) || peek(lexer) == #char "_")
            advance(lexer);
    }

    if remaining(lexer) > 0 && (peek(lexer) == #char "e" || peek(lexer) == #char "E") {
        kind = .FLOAT_LITERAL;
        advance(lexer);
        if remaining(lexer) > 0 && (peek(lexer) == #char "+" || peek(lexer) == #char "-")
            advance(lexer);
        while remaining(lexer) > 0 && (is_digit(peek(lexer)) || peek(lexer) == #char "_")
            advance(lexer);
    }

    span := make_span(start_position_in_input, lexer.position, start_line, start_column);
    return .{ kind = kind, text = span_text(lexer.source_code, span), span = span };
}

is_hex_digit :: inline (c: u8) -> bool {
    return is_digit(c) || (c >= #char "a" && c <= #char "f") || (c >= #char "A" && c <= #char "F");
}

consume_string_token :: (lexer: *Lexer) -> Token {
    start_position_in_input := lexer.position;
    start_line := lexer.line;
    start_column := lexer.column;

    advance(lexer);
    while remaining(lexer) > 0 {
        c := peek(lexer);
        if c == #char "\"" {
            advance(lexer);
            span := make_span(start_position_in_input, lexer.position, start_line, start_column);
            return .{ kind = .STRING_LITERAL, text = span_text(lexer.source_code, span), span = span };
        }
        if c == #char "\\" {
            advance(lexer);
            if remaining(lexer) == 0 break;
        }
        advance(lexer);
    }

    span := make_span(start_position_in_input, lexer.position, start_line, start_column);
    set_error(lexer, "Unterminated string literal", span);
    return .{ kind = .INVALID, text = span_text(lexer.source_code, span), span = span };
}

consume_raw_string_token :: (lexer: *Lexer) -> Token {
    start_position_in_input := lexer.position;
    start_line := lexer.line;
    start_column := lexer.column;

    advance_n(lexer, 7); // #string

    while remaining(lexer) > 0 && (peek(lexer) == #char " " || peek(lexer) == #char "\t")
        advance(lexer);

    delimiter_start := lexer.position;
    while remaining(lexer) > 0 && is_identifier_continue(peek(lexer))
        advance(lexer);

    delimiter_span := make_span(delimiter_start, lexer.position, start_line, start_column);
    delimiter := span_text(lexer.source_code, delimiter_span);

    while remaining(lexer) > 0 && peek(lexer) != #char "\n"
        advance(lexer);
    if remaining(lexer) > 0 advance(lexer);

    while remaining(lexer) > 0 {
        while remaining(lexer) > 0 && (peek(lexer) == #char " " || peek(lexer) == #char "\t")
            advance(lexer);

        if starts_with_at_current_position(lexer, delimiter) {
            advance_n(lexer, delimiter.count);
            span := make_span(start_position_in_input, lexer.position, start_line, start_column);
            return .{ kind = .STRING_LITERAL, text = span_text(lexer.source_code, span), span = span };
        }

        while remaining(lexer) > 0 && peek(lexer) != #char "\n"
            advance(lexer);
        if remaining(lexer) > 0 advance(lexer);
    }

    span := make_span(start_position_in_input, lexer.position, start_line, start_column);
    set_error(lexer, "Unterminated #string literal", span);
    return .{ kind = .INVALID, text = span_text(lexer.source_code, span), span = span };
}

starts_with_at_current_position :: (lexer: *Lexer, text: string) -> bool {
    if remaining(lexer) < text.count return false;

    for i: 0..text.count-1 {
        if lexer.source_code[lexer.position + i] != text[i] return false;
    }

    return true;
}

is_identifier_start :: inline (c: u8) -> bool {
    return (c >= #char "a" && c <= #char "z") ||
           (c >= #char "A" && c <= #char "Z") ||
           c == #char "_";
}

is_identifier_continue :: inline (c: u8) -> bool {
    return is_identifier_start(c) || is_digit(c);
}
