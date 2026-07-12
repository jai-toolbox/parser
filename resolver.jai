/*
The resolver is a lightweight symbol identity pass for editor-style tools.
It is intentionally separate from semantic.jai: semantic analysis answers type
questions, while this pass answers "which declaration does this name refer to?"

The first supported subset is one parsed file with file scope, function scopes,
block scopes, top-level declarations, parameters, locals, and identifier/type
name references. Unknown names are allowed and recorded with a null symbol.
*/

Resolver_Symbol_Kind :: enum {
    GLOBAL;
    FUNCTION;
    STRUCT;
    ENUM;
    PARAM;
    LOCAL;
    FIELD;
}

Resolver_Symbol :: struct {
    name: string;
    kind: Resolver_Symbol_Kind;
    declaration_node: *Node;
    file_path: string;
    scope: *Resolver_Scope;
}

Resolver_Reference :: struct {
    name: string;
    node: *Node;
    symbol: *Resolver_Symbol;
    file_path: string;
    scope: *Resolver_Scope;
}

Resolver_Scope :: struct {
    parent: *Resolver_Scope;
    symbols: [..] *Resolver_Symbol;
    symbols_by_name: Table(string, *Resolver_Symbol);
}

Resolver_File_Scope :: struct {
    path: string;
    scope: *Resolver_Scope;
}

Resolver_Result :: struct {
    root_scope: *Resolver_Scope;
    file_scopes: [..] Resolver_File_Scope;
    symbols: [..] *Resolver_Symbol;
    references: [..] Resolver_Reference;
}

Resolver_Input_File :: struct {
    path: string;
    root: *Node;
}

Resolver_State :: struct {
    result: Resolver_Result;
    current_scope: *Resolver_Scope;
    file_path: string;
}

resolve_symbols :: (root: *Node) -> Resolver_Result {
    state: Resolver_State;
    state.result.root_scope = new_resolver_scope(null);
    state.current_scope = state.result.root_scope;

    if root == null || root.kind != .FILE return state.result;

    for declaration: root.declarations {
        add_declaration_symbol(*state, declaration, true);
    }

    for declaration: root.declarations {
        resolve_top_level_declaration(*state, declaration);
    }

    return state.result;
}

resolve_project_symbols :: (files: [] Resolver_Input_File) -> Resolver_Result {
    state: Resolver_State;
    state.result.root_scope = new_resolver_scope(null);
    state.current_scope = state.result.root_scope;

    for file: files {
        if file.root == null || file.root.kind != .FILE continue;
        file_scope: Resolver_File_Scope;
        file_scope.path = file.path;
        file_scope.scope = new_resolver_scope(state.result.root_scope);
        array_add(*state.result.file_scopes, file_scope);

        state.file_path = file.path;
        state.current_scope = file_scope.scope;
        for declaration: file.root.declarations {
            add_declaration_symbol(*state, declaration, true);
        }

        export_file_scope_declarations_to_project(*state, file.root, file_scope.scope);
    }

    for file: files {
        if file.root == null || file.root.kind != .FILE continue;
        file_scope := find_file_scope(state.result, file.path);
        if file_scope == null continue;

        state.file_path = file.path;
        state.current_scope = file_scope;
        for declaration: file.root.declarations {
            resolve_top_level_declaration(*state, declaration);
        }
    }

    return state.result;
}

resolve_project_symbols_for_file :: (files: [] Resolver_Input_File, target_path: string) -> Resolver_Result {
    state: Resolver_State;
    state.result.root_scope = new_resolver_scope(null);
    state.current_scope = state.result.root_scope;

    for file: files {
        if file.root == null || file.root.kind != .FILE continue;
        file_scope: Resolver_File_Scope;
        file_scope.path = file.path;
        file_scope.scope = new_resolver_scope(state.result.root_scope);
        array_add(*state.result.file_scopes, file_scope);

        state.file_path = file.path;
        state.current_scope = file_scope.scope;
        for declaration: file.root.declarations {
            add_declaration_symbol(*state, declaration, true);
        }

        export_file_scope_declarations_to_project(*state, file.root, file_scope.scope);
    }

    for file: files {
        if file.path != target_path continue;
        if file.root == null || file.root.kind != .FILE continue;
        file_scope := find_file_scope(state.result, file.path);
        if file_scope == null continue;

        state.file_path = file.path;
        state.current_scope = file_scope;
        for declaration: file.root.declarations {
            resolve_top_level_declaration(*state, declaration);
        }

        break;
    }

    return state.result;
}

export_file_scope_declarations_to_project :: (state: *Resolver_State, root: *Node, file_scope: *Resolver_Scope) {
    if root == null || file_scope == null return;

    for declaration: root.declarations {
        export_declaration_symbol_to_project(state, declaration, file_scope);
    }
}

export_declaration_symbol_to_project :: (state: *Resolver_State, declaration: *Node, file_scope: *Resolver_Scope) {
    if declaration == null return;

    if declaration.kind == .LOCAL_GROUP_DECL {
        for child: declaration.declarations export_declaration_symbol_to_project(state, child, file_scope);
        return;
    }

    if !is_declaration_symbol_kind(declaration.kind) return;

    symbol := find_symbol_in_scope(file_scope, declaration.name);
    if symbol == null return;
    if find_symbol_in_scope(state.result.root_scope, declaration.name) != null return;
    array_add(*state.result.root_scope.symbols, symbol);
    table_add(*state.result.root_scope.symbols_by_name, symbol.name, symbol);
}

new_resolver_scope :: (parent: *Resolver_Scope) -> *Resolver_Scope {
    scope := New(Resolver_Scope);
    scope.parent = parent;
    return scope;
}

push_scope :: (state: *Resolver_State) {
    state.current_scope = new_resolver_scope(state.current_scope);
}

pop_scope :: (state: *Resolver_State) {
    if state.current_scope != null && state.current_scope.parent != null {
        state.current_scope = state.current_scope.parent;
    }
}

add_symbol :: (state: *Resolver_State, scope: *Resolver_Scope, name: string, kind: Resolver_Symbol_Kind, declaration_node: *Node) -> *Resolver_Symbol {
    if scope == null || name.count == 0 return null;

    existing := find_symbol_in_scope(scope, name);
    if existing != null return existing;

    symbol := New(Resolver_Symbol);
    symbol.name = name;
    symbol.kind = kind;
    symbol.declaration_node = declaration_node;
    symbol.file_path = state.file_path;
    symbol.scope = scope;
    array_add(*scope.symbols, symbol);
    table_add(*scope.symbols_by_name, symbol.name, symbol);
    array_add(*state.result.symbols, symbol);
    return symbol;
}

add_declaration_symbol :: (state: *Resolver_State, declaration: *Node, top_level: bool) {
    if declaration == null return;

    if declaration.kind == .LOCAL_GROUP_DECL {
        for child: declaration.declarations add_declaration_symbol(state, child, top_level);
        return;
    }

    kind := Resolver_Symbol_Kind.LOCAL;
    if top_level {
        if declaration.kind == .FUNCTION kind = .FUNCTION;
        else if declaration.kind == .STRUCT_DECL kind = .STRUCT;
        else if declaration.kind == .ENUM_DECL kind = .ENUM;
        else kind = .GLOBAL;
    } else {
        if declaration.kind == .PARAM kind = .PARAM;
        else if declaration.kind == .STRUCT_FIELD kind = .FIELD;
        else kind = .LOCAL;
    }

    if is_declaration_symbol_kind(declaration.kind) {
        add_symbol(state, state.current_scope, declaration.name, kind, declaration);
    }
}

is_declaration_symbol_kind :: (kind: Node_Kind) -> bool {
    return kind == .FUNCTION ||
           kind == .PARAM ||
           kind == .STRUCT_DECL ||
           kind == .ENUM_DECL ||
           kind == .STRUCT_FIELD ||
           kind == .CONST_DECL ||
           kind == .GLOBAL_TYPED_DECL ||
           kind == .LOCAL_INFER_DECL ||
           kind == .LOCAL_TYPED_DECL;
}

resolve_top_level_declaration :: (state: *Resolver_State, declaration: *Node) {
    if declaration == null return;

    if declaration.kind == .LOCAL_GROUP_DECL {
        resolve_expression(state, declaration.rhs);
        for child: declaration.declarations resolve_top_level_declaration(state, child);
        return;
    }

    if declaration.kind == {
        case .FUNCTION;
            resolve_function_declaration(state, declaration);

        case .STRUCT_DECL;
            resolve_struct_declaration(state, declaration);

        case .ENUM_DECL;
            for child: declaration.declarations resolve_expression(state, child.rhs);

        case .GLOBAL_TYPED_DECL;
            resolve_type_reference(state, declaration.type_node);
            resolve_expression(state, declaration.rhs);

        case .CONST_DECL;
            resolve_expression(state, declaration.rhs);

        case .LOCAL_INFER_DECL;
            resolve_expression(state, declaration.rhs);
    }
}

resolve_function_declaration :: (state: *Resolver_State, fn: *Node) {
    push_scope(state);
    defer pop_scope(state);

    for param: fn.params {
        add_declaration_symbol(state, param, false);
        resolve_type_reference(state, param.type_node);
        resolve_expression(state, param.rhs);
    }

    resolve_type_reference(state, fn.return_type);
    resolve_block_contents(state, fn.body, create_scope = true);
}

resolve_struct_declaration :: (state: *Resolver_State, node: *Node) {
    push_scope(state);
    defer pop_scope(state);

    for field: node.declarations {
        if field.kind == .STRUCT_FIELD {
            add_declaration_symbol(state, field, false);
            resolve_type_reference(state, field.type_node);
            resolve_expression(state, field.rhs);
        } else {
            add_declaration_symbol(state, field, true);
            resolve_top_level_declaration(state, field);
        }
    }
}

resolve_block_contents :: (state: *Resolver_State, block: *Node, create_scope: bool) {
    if block == null return;

    if create_scope push_scope(state);
    defer if create_scope pop_scope(state);

    for statement: block.statements {
        resolve_statement(state, statement);
    }
}

resolve_statement_body :: (state: *Resolver_State, body: *Node, create_scope: bool) {
    if body == null return;

    if body.kind == .BLOCK {
        resolve_block_contents(state, body, create_scope);
    } else {
        if create_scope push_scope(state);
        resolve_statement(state, body);
        if create_scope pop_scope(state);
    }
}

resolve_statement :: (state: *Resolver_State, statement: *Node) {
    if statement == null return;

    if statement.kind == {
        case .BLOCK;
            resolve_block_contents(state, statement, create_scope = true);

        case .FUNCTION;
            add_declaration_symbol(state, statement, false);
            resolve_function_declaration(state, statement);

        case .LOCAL_GROUP_DECL;
            resolve_expression(state, statement.rhs);
            for child: statement.declarations add_declaration_symbol(state, child, false);

        case .LOCAL_INFER_DECL;
            resolve_expression(state, statement.rhs);
            add_declaration_symbol(state, statement, false);

        case .LOCAL_TYPED_DECL;
            resolve_type_reference(state, statement.type_node);
            resolve_expression(state, statement.rhs);
            add_declaration_symbol(state, statement, false);

        case .CONST_DECL;
            resolve_expression(state, statement.rhs);
            add_declaration_symbol(state, statement, false);

        case .ASSIGNMENT;
            resolve_expression(state, statement.lhs);
            resolve_expression(state, statement.rhs);

        case .COMPOUND_ASSIGNMENT;
            resolve_expression(state, statement.lhs);
            resolve_expression(state, statement.rhs);

        case .RETURN_STATEMENT;
            resolve_expression(state, statement.rhs);

        case .EXPRESSION_STATEMENT;
            resolve_expression(state, statement.rhs);

        case .IF_STATEMENT;
            resolve_expression(state, statement.lhs);
            resolve_statement_body(state, statement.body, create_scope = true);
            if statement.rhs != null {
                if statement.rhs.kind == .BLOCK resolve_block_contents(state, statement.rhs, create_scope = true);
                else resolve_statement(state, statement.rhs);
            }

        case .FOR_STATEMENT;
            resolve_expression(state, statement.lhs);
            resolve_statement_body(state, statement.body, create_scope = true);

        case .WHILE_STATEMENT;
            resolve_expression(state, statement.lhs);
            resolve_statement_body(state, statement.body, create_scope = true);

        case .DEFER_STATEMENT;
            resolve_statement_body(state, statement.body, create_scope = true);

        case .COMPILER_DIRECTIVE_STATEMENT;
            resolve_statement_body(state, statement.body, create_scope = true);

        case;
            resolve_expression(state, statement);
    }
}

resolve_expression :: (state: *Resolver_State, expression: *Node) {
    if expression == null return;

    if expression.kind == {
        case .IDENTIFIER_EXPR;
            add_reference(state, expression, expression.name);

        case .TYPE_NAME;
            add_reference(state, expression, expression.name);

        case .FIELD_ACCESS_EXPR;
            resolve_expression(state, expression.lhs);

        case .ENUM_VALUE_EXPR;
            return;

        case .INDEX_EXPR;
            resolve_expression(state, expression.lhs);
            resolve_expression(state, expression.rhs);

        case .CALL_EXPR;
            resolve_expression(state, expression.callee);
            for arg: expression.args resolve_expression(state, arg);

        case .NAMED_ARGUMENT_EXPR;
            resolve_expression(state, expression.rhs);

        case .STRUCT_LITERAL_EXPR;
            resolve_expression(state, expression.type_node);
            for arg: expression.args resolve_expression(state, arg);

        case .STRUCT_LITERAL_ENTRY;
            resolve_expression(state, expression.lhs);
            resolve_expression(state, expression.rhs);

        case .ARRAY_LITERAL_EXPR;
            resolve_type_reference(state, expression.type_node);
            for arg: expression.args resolve_expression(state, arg);

        case .CAST_EXPR;
            resolve_type_reference(state, expression.type_node);
            resolve_expression(state, expression.rhs);

        case .COMPILER_DIRECTIVE_EXPR;
            resolve_expression(state, expression.lhs);
            resolve_expression(state, expression.rhs);
            resolve_block_contents(state, expression.body, create_scope = true);
            for arg: expression.args resolve_expression(state, arg);

        case .IFX_EXPR;
            resolve_expression(state, expression.lhs);
            resolve_expression(state, expression.rhs);
            if expression.body != null {
                if expression.body.kind == .BLOCK resolve_block_contents(state, expression.body, create_scope = true);
                else resolve_expression(state, expression.body);
            }

        case .EXPRESSION_LIST;
            for arg: expression.args resolve_expression(state, arg);

        case .LAMBDA_EXPR;
            push_scope(state);
            for param: expression.params {
                add_declaration_symbol(state, param, false);
                resolve_type_reference(state, param.type_node);
                resolve_expression(state, param.rhs);
            }
            resolve_type_reference(state, expression.return_type);
            resolve_block_contents(state, expression.body, create_scope = true);
            resolve_expression(state, expression.rhs);
            pop_scope(state);

        case .UNARY_EXPR;
            resolve_expression(state, expression.rhs);

        case .BINARY_EXPR;
            resolve_expression(state, expression.lhs);
            resolve_expression(state, expression.rhs);

        case .TYPE_APPLICATION;
            resolve_type_reference(state, expression.type_node);
            for arg: expression.args resolve_expression(state, arg);

        case .TYPE_LIST;
            for param: expression.params resolve_type_reference(state, param);

        case .FUNCTION_TYPE;
            for param: expression.params resolve_type_reference(state, param);
            resolve_type_reference(state, expression.return_type);

        case .VARIADIC_TYPE;
            resolve_type_reference(state, expression.type_node);

        case .POINTER_TYPE;
            resolve_type_reference(state, expression.type_node);

        case .ARRAY_TYPE;
            resolve_expression(state, expression.rhs);
            resolve_type_reference(state, expression.type_node);

        case .POLYMORPHIC_TYPE;
            resolve_type_reference(state, expression.type_node);

        case .INTERFACE_TYPE;
            resolve_type_reference(state, expression.type_node);

        case;
    }
}

resolve_type_reference :: (state: *Resolver_State, type_node: *Node) {
    resolve_expression(state, type_node);
}

add_reference :: (state: *Resolver_State, node: *Node, name: string) {
    reference: Resolver_Reference;
    reference.name = name;
    reference.node = node;
    reference.symbol = find_symbol(state.current_scope, name);
    reference.file_path = state.file_path;
    reference.scope = state.current_scope;
    array_add(*state.result.references, reference);
}

find_symbol :: (scope: *Resolver_Scope, name: string) -> *Resolver_Symbol {
    current := scope;
    while current != null {
        symbol := find_symbol_in_scope(current, name);
        if symbol != null return symbol;
        current = current.parent;
    }
    return null;
}

find_symbol_in_scope :: (scope: *Resolver_Scope, name: string) -> *Resolver_Symbol {
    if scope == null return null;

    found, symbol := table_find(*scope.symbols_by_name, name);
    if !found return null;
    return symbol;
}

find_file_scope :: (result: Resolver_Result, file_path: string) -> *Resolver_Scope {
    for file_scope: result.file_scopes {
        if file_scope.path == file_path return file_scope.scope;
    }
    return null;
}

span_contains_position :: (span: Source_Span, line: int, column: int) -> bool {
    if span.line != line return false;
    if column < span.column return false;
    return column < span.column + span.count;
}

find_symbol_at_position :: (result: Resolver_Result, line: int, column: int) -> *Resolver_Symbol {
    for symbol: result.symbols {
        if symbol != null && symbol.declaration_node != null && span_contains_position(symbol.declaration_node.span, line, column) {
            return symbol;
        }
    }

    for reference: result.references {
        if reference.node != null && span_contains_position(reference.node.span, line, column) {
            return reference.symbol;
        }
    }

    return null;
}

find_symbol_at_position_in_file :: (result: Resolver_Result, file_path: string, line: int, column: int) -> *Resolver_Symbol {
    for symbol: result.symbols {
        if symbol == null || symbol.file_path != file_path continue;
        if symbol.declaration_node != null && span_contains_position(symbol.declaration_node.span, line, column) {
            return symbol;
        }
    }

    for reference: result.references {
        if reference.file_path != file_path continue;
        if reference.node != null && span_contains_position(reference.node.span, line, column) {
            return reference.symbol;
        }
    }

    return null;
}

reference_count_for_symbol :: (result: Resolver_Result, symbol: *Resolver_Symbol) -> int {
    if symbol == null return 0;

    count := 0;
    for reference: result.references {
        if reference.symbol == symbol count += 1;
    }
    return count;
}
