/*
The semantic pass walks the syntax tree after parsing and builds lightweight
tables for declarations and resolved expression types. It is not a full Jai type
checker. Its job is to answer structural questions that require names to mean
something: which structs exist, which fields they contain, which functions
exist, what local variables are in scope, and what type an expression resolves
to when the current subset has enough information.

Analysis happens in two stages. First, top-level declarations are collected into
struct and function tables. Second, function bodies are walked with a local
scope made from the function parameters and inferred/declared locals. Expression
types are stored in a side table keyed by AST node pointer, so later passes can
ask for the resolved type without changing the AST itself.

Unknown-but-accepted syntax resolves to UNKNOWN. Invalid contradictions, such
as assigning clearly different known types, report the first semantic error.
*/

Resolved_Type_Kind :: enum {
    INVALID;
    UNKNOWN;
    NAMED;
    POINTER;
    ARRAY;
}

Resolved_Type :: struct {
    kind : Resolved_Type_Kind;
    name : string;
    pointee : *Resolved_Type;
    array_count : int;
}

Semantic_Field :: struct {
    name : string;
    value_type : Resolved_Type;
}

Semantic_Struct :: struct {
    name : string;
    fields : [..] Semantic_Field;
    declaration_node : *Node;
}

Semantic_Local :: struct {
    name : string;
    value_type : Resolved_Type;
    declaration_node : *Node;
}

Semantic_Function :: struct {
    name : string;
    params : [..] Semantic_Local;
    return_type : Resolved_Type;
    locals : [..] Semantic_Local;
    declaration_node : *Node;
}

Expression_Type_Binding :: struct {
    node : *Node;
    value_type : Resolved_Type;
}

Semantic_Program :: struct {
    structs : [..] Semantic_Struct;
    functions : [..] Semantic_Function;
    expression_types : [..] Expression_Type_Binding;
}

Semantic_Result :: struct {
    success : bool;
    error : Error;
    program : Semantic_Program;
}

Semantic_State :: struct {
    program : Semantic_Program;
    has_error : bool;
    error_message : string;
    error_span : Source_Span;
}

Function_Scope :: struct {
    function : *Semantic_Function;
    locals : [..] Semantic_Local;
}

analyze :: (root: *Node) -> Semantic_Result {
    state: Semantic_State;
    result: Semantic_Result;

    if root == null {
        state.has_error = true;
        state.error_message = "Cannot analyze null AST root";
    } else if root.kind != .FILE {
        state.has_error = true;
        state.error_message = "Semantic analysis expects a file node";
        state.error_span = root.span;
    } else {
        collect_top_level_declarations(*state, root);
        if !state.has_error analyze_function_bodies(*state);
    }

    if state.has_error {
        result.success = false;
        result.error = .{ message = state.error_message, span = state.error_span };
        result.program = state.program;
        return result;
    }

    result.success = true;
    result.program = state.program;
    return result;
}

analyze_source :: (source_code: string) -> Semantic_Result {
    parse_result := parse(source_code);
    if !parse_result.success {
        result: Semantic_Result;
        result.success = false;
        result.error = parse_result.error;
        return result;
    }

    return analyze(parse_result.root);
}

collect_top_level_declarations :: (state: *Semantic_State, root: *Node) {
    for declaration : root.declarations {
        if state.has_error return;

        if declaration.kind == {
            case .STRUCT_DECL;
                collect_struct_declaration(state, declaration);

            case .FUNCTION;
                collect_function_signature(state, declaration);
        }
    }
}

collect_struct_declaration :: (state: *Semantic_State, node: *Node) {
    if find_struct(state.program, node.name) != null {
        set_error(state, tprint("Duplicate struct declaration '%'", node.name), node.span);
        return;
    }

    semantic_struct: Semantic_Struct;
    semantic_struct.name = node.name;
    semantic_struct.declaration_node = node;

    for field_node : node.declarations {
        field: Semantic_Field;
        field.name = field_node.name;
        field.value_type = resolve_type_node(field_node.type_node);
        array_add(*semantic_struct.fields, field);
    }

    array_add(*state.program.structs, semantic_struct);
}

collect_function_signature :: (state: *Semantic_State, node: *Node) {
    if find_function(state.program, node.name) != null {
        set_error(state, tprint("Duplicate function declaration '%'", node.name), node.span);
        return;
    }

    semantic_function: Semantic_Function;
    semantic_function.name = node.name;
    semantic_function.return_type = resolve_type_node(node.return_type);
    semantic_function.declaration_node = node;

    for param_node : node.params {
        param: Semantic_Local;
        param.name = param_node.name;
        param.value_type = resolve_type_node(param_node.type_node);
        param.declaration_node = param_node;
        array_add(*semantic_function.params, param);
    }

    array_add(*state.program.functions, semantic_function);
}

analyze_function_bodies :: (state: *Semantic_State) {
    for * semantic_function : state.program.functions {
        if state.has_error return;
        analyze_function_body(state, semantic_function);
    }
}

analyze_function_body :: (state: *Semantic_State, semantic_function: *Semantic_Function) {
    scope: Function_Scope;
    scope.function = semantic_function;

    for param : semantic_function.params {
        array_add(*scope.locals, param);
    }

    body := semantic_function.declaration_node.body;
    if body == null return;

    for statement : body.statements {
        if state.has_error return;
        analyze_statement(state, *scope, statement);
    }
}

analyze_statement :: (state: *Semantic_State, scope: *Function_Scope, statement: *Node) {
    if statement.kind == {
        case .LOCAL_INFER_DECL;
            value_type := resolve_expression_type(state, scope, statement.rhs, unknown_type());
            add_local(state, scope, statement.name, value_type, statement);

        case .LOCAL_TYPED_DECL;
            declared_type := resolve_type_node(statement.type_node);
            add_local(state, scope, statement.name, declared_type, statement);
            rhs_type := resolve_expression_type(state, scope, statement.rhs, declared_type);
            require_assignable(state, declared_type, rhs_type, statement.span);

        case .ASSIGNMENT;
            lhs_type := resolve_expression_type(state, scope, statement.lhs, unknown_type());
            rhs_type := resolve_expression_type(state, scope, statement.rhs, lhs_type);
            require_assignable(state, lhs_type, rhs_type, statement.span);

        case .COMPOUND_ASSIGNMENT;
            lhs_type := resolve_expression_type(state, scope, statement.lhs, unknown_type());
            rhs_type := resolve_expression_type(state, scope, statement.rhs, lhs_type);
            resolve_binary_type(state, lhs_type, rhs_type, statement.op, statement.span);

        case .RETURN_STATEMENT;
            return_value_type := resolve_expression_type(state, scope, statement.rhs, semantic_function_return_type(scope));
            require_assignable(state, semantic_function_return_type(scope), return_value_type, statement.span);
    }
}

resolve_expression_type :: (state: *Semantic_State, scope: *Function_Scope, expression: *Node) -> Resolved_Type {
    return resolve_expression_type(state, scope, expression, unknown_type());
}

resolve_expression_type :: (state: *Semantic_State, scope: *Function_Scope, expression: *Node, expected_type: Resolved_Type) -> Resolved_Type {
    if expression == null return invalid_type();

    result := invalid_type();

    if expression.kind == {
        case .IDENTIFIER_EXPR;
            local := find_local(scope, expression.name);
            if local != null {
                result = local.value_type;
            } else if find_function(state.program, expression.name) != null {
                result = unknown_type();
            } else {
                result = unknown_type();
            }

        case .INT_LITERAL_EXPR;
            result = named_type("s32");

        case .FLOAT_LITERAL_EXPR;
            result = named_type("float32");

        case .STRING_LITERAL_EXPR;
            result = named_type("string");

        case .FIELD_ACCESS_EXPR;
            lhs_type := resolve_expression_type(state, scope, expression.lhs);
            result = resolve_field_access_type(state, lhs_type, expression.name, expression.span);

        case .INDEX_EXPR;
            lhs_type := resolve_expression_type(state, scope, expression.lhs);
            index_type := resolve_expression_type(state, scope, expression.rhs);
            if is_unknown_type(lhs_type) || is_unknown_type(index_type) {
                result = unknown_type();
            } else if lhs_type.kind == .ARRAY && lhs_type.pointee != null {
                result = lhs_type.pointee.*;
            } else {
                result = unknown_type();
            }

        case .CALL_EXPR;
            result = resolve_call_type(state, scope, expression);

        case .STRUCT_LITERAL_EXPR;
            if expression.type_node != null {
                result = resolve_struct_literal_type(state, scope, expression.type_node);
            } else if !is_unknown_type(expected_type) {
                result = expected_type;
            } else {
                result = unknown_type();
            }

            for entry : expression.args {
                resolve_expression_type(state, scope, entry.rhs);
            }

        case .CAST_EXPR;
            result = resolve_type_node(expression.type_node);
            resolve_expression_type(state, scope, expression.rhs);

        case .UNARY_EXPR;
            result = resolve_expression_type(state, scope, expression.rhs, expected_type);

        case .BINARY_EXPR;
            lhs_type := resolve_expression_type(state, scope, expression.lhs);
            rhs_type := resolve_expression_type(state, scope, expression.rhs);
            result = resolve_binary_type(state, lhs_type, rhs_type, expression.op, expression.span);
    }

    bind_expression_type(state, expression, result);
    return result;
}

resolve_call_type :: (state: *Semantic_State, scope: *Function_Scope, call: *Node) -> Resolved_Type {
    if call.callee == null return invalid_type();

    if call.callee.kind != .IDENTIFIER_EXPR {
        resolve_expression_type(state, scope, call.callee);
        for arg : call.args resolve_expression_type(state, scope, arg);
        return unknown_type();
    }

    function := find_function(state.program, call.callee.name);
    if function == null {
        for arg : call.args resolve_expression_type(state, scope, arg);
        return unknown_type();
    }

    for i : 0..call.args.count-1 {
        expected_arg_type := unknown_type();
        if i < function.params.count expected_arg_type = function.params[i].value_type;

        arg_type := resolve_expression_type(state, scope, call.args[i], expected_arg_type);
        if i < function.params.count require_assignable(state, expected_arg_type, arg_type, call.args[i].span);
    }

    bind_expression_type(state, call.callee, unknown_type());
    return function.return_type;
}

resolve_field_access_type :: (state: *Semantic_State, owner_type: Resolved_Type, field_name: string, span: Source_Span) -> Resolved_Type {
    actual_owner_type := dereference_type(owner_type);
    if is_unknown_type(actual_owner_type) return unknown_type();
    if actual_owner_type.kind != .NAMED {
        set_error(state, tprint("Cannot access field '%' on non-struct type", field_name), span);
        return invalid_type();
    }

    semantic_struct := find_struct(state.program, actual_owner_type.name);
    if semantic_struct == null {
        return unknown_type();
    }

    for field : semantic_struct.fields {
        if field.name == field_name return field.value_type;
    }

    set_error(state, tprint("Type '%' has no field '%'", actual_owner_type.name, field_name), span);
    return invalid_type();
}

resolve_struct_literal_type :: (state: *Semantic_State, scope: *Function_Scope, type_expression: *Node) -> Resolved_Type {
    if type_expression.kind == .IDENTIFIER_EXPR return named_type(type_expression.name);
    resolved := resolve_expression_type(state, scope, type_expression);
    if resolved.kind == .NAMED return resolved;
    return unknown_type();
}

resolve_binary_type :: (state: *Semantic_State, lhs_type: Resolved_Type, rhs_type: Resolved_Type, op: Token_Kind, span: Source_Span) -> Resolved_Type {
    if is_unknown_type(lhs_type) || is_unknown_type(rhs_type) return unknown_type();

    if same_type(lhs_type, rhs_type) return lhs_type;

    if is_named_type(lhs_type, "Vector3") && is_named_type(rhs_type, "float32") return lhs_type;
    if is_named_type(lhs_type, "float32") && is_named_type(rhs_type, "Vector3") return rhs_type;

    set_error(state, tprint("Cannot apply binary operator to '%' and '%'", type_to_string(lhs_type), type_to_string(rhs_type)), span);
    return invalid_type();
}

resolve_type_node :: (type_node: *Node) -> Resolved_Type {
    if type_node == null return invalid_type();

    if type_node.kind == {
        case .TYPE_NAME;
            return named_type(type_node.name);

        case .POINTER_TYPE;
            pointee := New(Resolved_Type);
            pointee.* = resolve_type_node(type_node.type_node);
            return .{ kind = .POINTER, pointee = pointee };

        case .ARRAY_TYPE;
            element := New(Resolved_Type);
            element.* = resolve_type_node(type_node.type_node);
            count := 0;
            if type_node.rhs != null && type_node.rhs.kind == .INT_LITERAL_EXPR {
                parsed_count, ok, _ := string_to_int(type_node.rhs.literal);
                if ok count = parsed_count;
            }
            return .{ kind = .ARRAY, pointee = element, array_count = count };

        case .POLYMORPHIC_TYPE;
            return unknown_type();

        case .INTERFACE_TYPE;
            return resolve_type_node(type_node.type_node);
    }

    return unknown_type();
}

add_local :: (state: *Semantic_State, scope: *Function_Scope, name: string, value_type: Resolved_Type, declaration_node: *Node) {
    if find_local(scope, name) != null {
        set_error(state, tprint("Duplicate local declaration '%'", name), declaration_node.span);
        return;
    }

    local: Semantic_Local;
    local.name = name;
    local.value_type = value_type;
    local.declaration_node = declaration_node;
    array_add(*scope.locals, local);
    array_add(*scope.function.locals, local);
}

bind_expression_type :: (state: *Semantic_State, node: *Node, value_type: Resolved_Type) {
    if node == null return;

    for * binding : state.program.expression_types {
        if binding.node == node {
            binding.value_type = value_type;
            return;
        }
    }

    binding: Expression_Type_Binding;
    binding.node = node;
    binding.value_type = value_type;
    array_add(*state.program.expression_types, binding);
}

find_struct :: (program: Semantic_Program, name: string) -> *Semantic_Struct {
    for * semantic_struct : program.structs {
        if semantic_struct.name == name return semantic_struct;
    }
    return null;
}

find_function :: (program: Semantic_Program, name: string) -> *Semantic_Function {
    for * semantic_function : program.functions {
        if semantic_function.name == name return semantic_function;
    }
    return null;
}

find_local :: (scope: *Function_Scope, name: string) -> *Semantic_Local {
    for * local : scope.locals {
        if local.name == name return local;
    }
    return null;
}

semantic_function_return_type :: (scope: *Function_Scope) -> Resolved_Type {
    if scope.function == null return invalid_type();
    return scope.function.return_type;
}

require_assignable :: (state: *Semantic_State, destination_type: Resolved_Type, source_type: Resolved_Type, span: Source_Span) {
    if state.has_error return;
    if is_unknown_type(destination_type) || is_unknown_type(source_type) return;
    if same_type(destination_type, source_type) return;

    set_error(state, tprint("Cannot assign '%' to '%'", type_to_string(source_type), type_to_string(destination_type)), span);
}

same_type :: (a: Resolved_Type, b: Resolved_Type) -> bool {
    if a.kind != b.kind return false;
    if a.kind == {
        case .INVALID; return true;
        case .UNKNOWN; return true;
        case .NAMED; return a.name == b.name;
        case .POINTER;
            if a.pointee == null || b.pointee == null return a.pointee == b.pointee;
            return same_type(a.pointee.*, b.pointee.*);
        case .ARRAY;
            if a.array_count != b.array_count return false;
            if a.pointee == null || b.pointee == null return a.pointee == b.pointee;
            return same_type(a.pointee.*, b.pointee.*);
    }
    return false;
}

dereference_type :: (value_type: Resolved_Type) -> Resolved_Type {
    if value_type.kind == .POINTER && value_type.pointee != null return value_type.pointee.*;
    return value_type;
}

named_type :: (name: string) -> Resolved_Type {
    return .{ kind = .NAMED, name = name };
}

unknown_type :: () -> Resolved_Type {
    return .{ kind = .UNKNOWN };
}

invalid_type :: () -> Resolved_Type {
    return .{ kind = .INVALID };
}

is_unknown_type :: (value_type: Resolved_Type) -> bool {
    return value_type.kind == .UNKNOWN || value_type.kind == .INVALID;
}

is_named_type :: (value_type: Resolved_Type, name: string) -> bool {
    return value_type.kind == .NAMED && value_type.name == name;
}

type_to_string :: (value_type: Resolved_Type) -> string {
    if value_type.kind == {
        case .INVALID; return "<invalid>";
        case .UNKNOWN; return "<unknown>";
        case .NAMED; return value_type.name;
        case .POINTER;
            if value_type.pointee == null return "*<invalid>";
            return tprint("*%", type_to_string(value_type.pointee.*));
        case .ARRAY;
            if value_type.pointee == null return "[]<invalid>";
            return tprint("[%] %", value_type.array_count, type_to_string(value_type.pointee.*));
    }

    return "<invalid>";
}

set_error :: (state: *Semantic_State, message: string, span: Source_Span) {
    if state.has_error return;
    state.has_error = true;
    state.error_message = message;
    state.error_span = span;
}
