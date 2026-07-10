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
    IFX;
    THEN;
    FOR;
    WHILE;
    DEFER;
    ELSE;
    STRUCT;
    UNION;
    ENUM;
    ENUM_FLAGS;
    CAST;
    INTERFACE;
    USING;
    XX;

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
    DOT_DOT;
    DOLLAR;
    HASH;
    BACKTICK;
    AT;
    QUESTION;

    PLUS;
    MINUS;
    TRIPLE_MINUS;
    STAR;
    SLASH;
    PERCENT;
    AMPERSAND;
    CARET;
    PIPE;
    AND_AND;
    OR_OR;
    TILDE;
    EQUAL;
    EQUAL_EQUAL;
    BANG;
    BANG_EQUAL;
    LESS;
    LESS_EQUAL;
    LESS_LESS;
    LESS_LESS_LESS;
    GREATER;
    GREATER_EQUAL;
    GREATER_GREATER;
    GREATER_GREATER_GREATER;
    PLUS_EQUAL;
    MINUS_EQUAL;
    STAR_EQUAL;
    SLASH_EQUAL;
    PERCENT_EQUAL;
    AMPERSAND_EQUAL;
    AND_AND_EQUAL;
    CARET_EQUAL;
    PIPE_EQUAL;
    OR_OR_EQUAL;
    LESS_LESS_EQUAL;
    GREATER_GREATER_EQUAL;
    COLON_EQUAL;
    COLON_COLON;
    ARROW;
    FAT_ARROW;
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
    ENUM_DECL;
    STRUCT_FIELD;
    CONST_DECL;
    GLOBAL_TYPED_DECL;
    TYPE_NAME;
    TYPE_APPLICATION;
    TYPE_LIST;
    FUNCTION_TYPE;
    VARIADIC_TYPE;
    POINTER_TYPE;
    ARRAY_TYPE;
    POLYMORPHIC_TYPE;
    INTERFACE_TYPE;
    BLOCK;

    LOCAL_INFER_DECL;
    LOCAL_TYPED_DECL;
    LOCAL_GROUP_DECL;
    ASSIGNMENT;
    COMPOUND_ASSIGNMENT;
    RETURN_STATEMENT;
    IF_STATEMENT;
    FOR_STATEMENT;
    WHILE_STATEMENT;
    DEFER_STATEMENT;
    EXPRESSION_STATEMENT;
    COMPILER_DIRECTIVE_STATEMENT;

    IDENTIFIER_EXPR;
    INT_LITERAL_EXPR;
    FLOAT_LITERAL_EXPR;
    STRING_LITERAL_EXPR;
    UNINITIALIZED_EXPR;
    FIELD_ACCESS_EXPR;
    ENUM_VALUE_EXPR;
    INDEX_EXPR;
    CALL_EXPR;
    NAMED_ARGUMENT_EXPR;
    STRUCT_LITERAL_EXPR;
    STRUCT_LITERAL_ENTRY;
    ARRAY_LITERAL_EXPR;
    CAST_EXPR;
    COMPILER_DIRECTIVE_EXPR;
    IFX_EXPR;
    EXPRESSION_LIST;
    LAMBDA_EXPR;
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
