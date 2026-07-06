Source_Span :: struct {
    start_position_in_input : int;
    count  : int;
    line   : int;
    column : int;
}

Error :: struct {
    message : string;
    span    : Source_Span;
}

Token_Kind :: enum {
    EOF;
    INVALID;

    IDENTIFIER;
    INT_LITERAL;
    FLOAT_LITERAL;
    STRING_LITERAL;

    RETURN;
    IF;
    ELSE;
    STRUCT;
    CAST;
    INTERFACE;

    OPEN_PAREN;
    CLOSE_PAREN;
    OPEN_BRACE;
    CLOSE_BRACE;
    OPEN_BRACKET;
    CLOSE_BRACKET;
    COLON;
    SEMICOLON;
    COMMA;
    DOT;
    DOLLAR;

    PLUS;
    MINUS;
    STAR;
    SLASH;
    EQUAL;
    PLUS_EQUAL;
    COLON_EQUAL;
    COLON_COLON;
    ARROW;
}

Token :: struct {
    kind : Token_Kind;
    text : string;
    span : Source_Span;
}

Lexer_Result :: struct {
    tokens  : [..] Token;
    success : bool;
    error   : Error;
}

Node_Kind :: enum {
    FILE;
    FUNCTION;
    PARAM;
    STRUCT_DECL;
    STRUCT_FIELD;
    CONST_DECL;
    TYPE_NAME;
    POINTER_TYPE;
    ARRAY_TYPE;
    POLYMORPHIC_TYPE;
    INTERFACE_TYPE;
    BLOCK;

    LOCAL_INFER_DECL;
    LOCAL_TYPED_DECL;
    ASSIGNMENT;
    COMPOUND_ASSIGNMENT;
    RETURN_STATEMENT;

    IDENTIFIER_EXPR;
    INT_LITERAL_EXPR;
    FLOAT_LITERAL_EXPR;
    STRING_LITERAL_EXPR;
    FIELD_ACCESS_EXPR;
    INDEX_EXPR;
    CALL_EXPR;
    STRUCT_LITERAL_EXPR;
    STRUCT_LITERAL_ENTRY;
    CAST_EXPR;
    UNARY_EXPR;
    BINARY_EXPR;
}

Node :: struct {
    kind : Node_Kind;
    span : Source_Span;

    name : string;
    literal : string;
    op : Token_Kind;

    type_node : *Node;
    return_type : *Node;
    lhs : *Node;
    rhs : *Node;
    callee : *Node;
    body : *Node;

    params : [..] *Node;
    statements : [..] *Node;
    args : [..] *Node;
    declarations : [..] *Node;
}

Parse_Result :: struct {
    root    : *Node;
    success : bool;
    error   : Error;
}
