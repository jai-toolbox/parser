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
    TYPE_PARAM;
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
    id: Resolver_Symbol_Id;
    scope_rank: int;
}

Resolver_Symbol_Id :: struct {
    file_path: string;
    start: int;
    count: int;
}

Resolver_Symbol_Bucket :: struct {
    name: string;
    symbols: [..] *Resolver_Symbol;
}

Resolver_Reference :: struct {
    name: string;
    node: *Node;
    symbol: *Resolver_Symbol;
    symbol_bucket: *Resolver_Symbol_Bucket;
    file_path: string;
    scope: *Resolver_Scope;
}

Resolver_Scope :: struct {
    parent: *Resolver_Scope;
    symbols: [..] *Resolver_Symbol;
    symbols_by_name: Table(string, *Resolver_Symbol);
    symbol_buckets: [..] *Resolver_Symbol_Bucket;
    symbol_bucket_by_name: Table(string, *Resolver_Symbol_Bucket);
}

Resolver_File_Scope :: struct {
    path: string;
    scope: *Resolver_Scope;
}

Resolver_Directory_Scope :: struct {
    path: string;
    scope: *Resolver_Scope;
}

Resolver_Result :: struct {
    root_scope: *Resolver_Scope;
    scopes: [..] *Resolver_Scope;
    directory_scopes: [..] Resolver_Directory_Scope;
    file_scopes: [..] Resolver_File_Scope;
    symbols: [..] *Resolver_Symbol;
    references: [..] Resolver_Reference;
}

Resolver_Input_File :: struct {
    path: string;
    root: *Node;
    scope_rank: int;
}

Resolver_State :: struct {
    result: Resolver_Result;
    current_scope: *Resolver_Scope;
    file_path: string;
    file_scope_rank: int;
}

resolve_symbols :: (root: *Node) -> Resolver_Result {
    state: Resolver_State;
    state.result.root_scope = new_resolver_scope(*state, null);
    state.current_scope = state.result.root_scope;

    if root == null || root.kind != .FILE return state.result;

    for declaration: root.declarations {
        add_declaration_symbol(*state, declaration, true);
    }

    for declaration: root.declarations {
        resolve_top_level_declaration(*state, declaration);
    }

    for statement: root.statements {
        resolve_statement(*state, statement);
    }

    return state.result;
}

resolve_project_symbols :: (files: [] Resolver_Input_File) -> Resolver_Result {
    state: Resolver_State;
    state.result.root_scope = new_resolver_scope(*state, null);
    state.current_scope = state.result.root_scope;

    for file: files {
        if file.root == null || file.root.kind != .FILE continue;
        directory_scope := get_or_create_directory_scope(*state, resolver_directory_for_path(file.path));

        file_scope: Resolver_File_Scope;
        file_scope.path = file.path;
        file_scope.scope = directory_scope;
        array_add(*state.result.file_scopes, file_scope);

        state.file_path = file.path;
        state.file_scope_rank = file.scope_rank;
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
        state.file_scope_rank = file.scope_rank;
        state.current_scope = file_scope;
        for declaration: file.root.declarations {
            resolve_top_level_declaration(*state, declaration);
        }
    }

    return state.result;
}

resolve_project_symbols_for_file :: (files: [] Resolver_Input_File, target_path: string) -> Resolver_Result {
    target_paths: [..] string;
    array_add(*target_paths, target_path);
    return resolve_project_symbols_for_files(files, target_paths);
}

resolve_project_symbols_for_files :: (files: [] Resolver_Input_File, target_paths: [] string) -> Resolver_Result {
    state: Resolver_State;
    state.result.root_scope = new_resolver_scope(*state, null);
    state.current_scope = state.result.root_scope;

    // The caller's DefMap owns dependency declarations. This query builds the
    // lexical/body resolver only for requested files, matching rust-analyzer's
    // split between module name resolution and per-body SourceAnalyzer state.
    for file: files {
        if !path_is_in_resolver_list(file.path, target_paths) continue;
        if file.root == null || file.root.kind != .FILE continue;
        directory_scope := get_or_create_directory_scope(*state, resolver_directory_for_path(file.path));

        file_scope: Resolver_File_Scope;
        file_scope.path = file.path;
        file_scope.scope = directory_scope;
        array_add(*state.result.file_scopes, file_scope);

        state.file_path = file.path;
        state.file_scope_rank = file.scope_rank;
        state.current_scope = file_scope.scope;
        for declaration: file.root.declarations {
            add_declaration_symbol(*state, declaration, true);
        }

        export_file_scope_declarations_to_project(*state, file.root, file_scope.scope);
    }

    for file: files {
        if !path_is_in_resolver_list(file.path, target_paths) continue;
        if file.root == null || file.root.kind != .FILE continue;
        file_scope := find_file_scope(state.result, file.path);
        if file_scope == null continue;

        state.file_path = file.path;
        state.file_scope_rank = file.scope_rank;
        state.current_scope = file_scope;
        for declaration: file.root.declarations {
            resolve_top_level_declaration(*state, declaration);
        }

    }

    return state.result;
}

path_is_in_resolver_list :: (path: string, paths: [] string) -> bool {
    for candidate: paths {
        if candidate == path return true;
    }

    return false;
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

    symbol := find_symbol_for_declaration_in_scope(file_scope, declaration.name, declaration);
    if symbol == null return;
    resolver_scope_add_existing_symbol(state.result.root_scope, symbol);
}

new_resolver_scope :: (state: *Resolver_State, parent: *Resolver_Scope) -> *Resolver_Scope {
    scope := New(Resolver_Scope);
    scope.parent = parent;
    array_add(*state.result.scopes, scope);
    return scope;
}

resolver_path_char_is_separator :: (c: u8) -> bool {
    return c == #char "/" || c == #char "\\";
}

resolver_directory_for_path :: (path: string) -> string {
    i := path.count - 1;
    while i >= 0 {
        if resolver_path_char_is_separator(path[i]) {
            result: string;
            result.data = path.data;
            result.count = i;
            return result;
        }
        i -= 1;
    }
    return "";
}

find_directory_scope :: (result: Resolver_Result, directory_path: string) -> *Resolver_Scope {
    for directory_scope: result.directory_scopes {
        if directory_scope.path == directory_path return directory_scope.scope;
    }
    return null;
}

get_or_create_directory_scope :: (state: *Resolver_State, directory_path: string) -> *Resolver_Scope {
    existing := find_directory_scope(state.result, directory_path);
    if existing != null return existing;

    directory_scope: Resolver_Directory_Scope;
    directory_scope.path = directory_path;
    directory_scope.scope = new_resolver_scope(state, state.result.root_scope);
    array_add(*state.result.directory_scopes, directory_scope);
    return directory_scope.scope;
}

push_scope :: (state: *Resolver_State) {
    state.current_scope = new_resolver_scope(state, state.current_scope);
}

pop_scope :: (state: *Resolver_State) {
    if state.current_scope != null && state.current_scope.parent != null {
        state.current_scope = state.current_scope.parent;
    }
}

add_symbol :: (state: *Resolver_State, scope: *Resolver_Scope, name: string, kind: Resolver_Symbol_Kind, declaration_node: *Node) -> *Resolver_Symbol {
    if scope == null || name.count == 0 return null;

    bucket := find_symbol_bucket_in_scope(scope, name);
    if bucket != null {
        for existing: bucket.symbols {
            if existing.declaration_node == declaration_node return existing;
        }

        // Only procedures form overload sets. All other repeated spellings in
        // one lexical scope keep the first declaration, matching ordinary
        // shadowing and duplicate-declaration recovery.
        if kind != .FUNCTION || bucket.symbols.count == 0 || bucket.symbols[0].kind != .FUNCTION {
            return ifx bucket.symbols.count > 0 then bucket.symbols[0] else null;
        }
    }

    symbol := New(Resolver_Symbol);
    symbol.name = name;
    symbol.kind = kind;
    symbol.declaration_node = declaration_node;
    symbol.file_path = state.file_path;
    symbol.scope = scope;
    symbol.scope_rank = state.file_scope_rank;
    symbol.id.file_path = state.file_path;
    if declaration_node != null {
        symbol.id.start = declaration_node.span.start_position_in_input;
        symbol.id.count = declaration_node.span.count;
    }
    array_add(*scope.symbols, symbol);
    if bucket == null {
        bucket = New(Resolver_Symbol_Bucket);
        bucket.name = symbol.name;
        array_add(*scope.symbol_buckets, bucket);
        table_add(*scope.symbol_bucket_by_name, bucket.name, bucket);
        table_add(*scope.symbols_by_name, symbol.name, symbol);
    }
    array_add(*bucket.symbols, symbol);
    array_add(*state.result.symbols, symbol);
    return symbol;
}

resolver_scope_add_existing_symbol :: (scope: *Resolver_Scope, symbol: *Resolver_Symbol) {
    if scope == null || symbol == null || symbol.name.count == 0 return;

    bucket := find_symbol_bucket_in_scope(scope, symbol.name);
    if bucket != null {
        for existing: bucket.symbols if existing == symbol return;
        if bucket.symbols.count > 0 {
            if symbol.scope_rank > bucket.symbols[0].scope_rank return;
            if symbol.scope_rank < bucket.symbols[0].scope_rank {
                array_reset(*bucket.symbols);
                array_add(*bucket.symbols, symbol);
                table_set(*scope.symbols_by_name, symbol.name, symbol);
                array_add(*scope.symbols, symbol);
                return;
            }
        }
        if symbol.kind != .FUNCTION || bucket.symbols.count == 0 || bucket.symbols[0].kind != .FUNCTION return;
        array_add(*bucket.symbols, symbol);
        return;
    }

    bucket = New(Resolver_Symbol_Bucket);
    bucket.name = symbol.name;
    array_add(*bucket.symbols, symbol);
    array_add(*scope.symbol_buckets, bucket);
    table_add(*scope.symbol_bucket_by_name, bucket.name, bucket);
    array_add(*scope.symbols, symbol);
    table_add(*scope.symbols_by_name, symbol.name, symbol);
}

resolver_scope_add_symbol_alias :: (scope: *Resolver_Scope, alias: string, symbol: *Resolver_Symbol) {
    if scope == null || symbol == null || alias.count == 0 return;
    if find_symbol_bucket_in_scope(scope, alias) != null return;

    bucket := New(Resolver_Symbol_Bucket);
    bucket.name = alias;
    array_add(*bucket.symbols, symbol);
    array_add(*scope.symbol_buckets, bucket);
    table_add(*scope.symbol_bucket_by_name, alias, bucket);
    table_add(*scope.symbols_by_name, alias, symbol);
}

resolver_polymorphic_type_name :: (name: string) -> string {
    result := name;
    while result.count > 0 && result[0] == #char "$" {
        result.data += 1;
        result.count -= 1;
    }
    return result;
}

collect_polymorphic_type_declarations :: (state: *Resolver_State, node: *Node) {
    if node == null return;

    if node.kind == .POLYMORPHIC_TYPE && node.name.count > 0 {
        name := resolver_polymorphic_type_name(node.name);
        if name.count > 0 && find_symbol_in_scope(state.current_scope, name) == null {
            symbol := add_symbol(state, state.current_scope, name, .TYPE_PARAM, node);
            if node.name != name resolver_scope_add_symbol_alias(state.current_scope, node.name, symbol);
        }
    }

    collect_polymorphic_type_declarations(state, node.type_node);
    collect_polymorphic_type_declarations(state, node.return_type);
    for param: node.params {
        if param != null && param.kind == .PARAM {
            collect_polymorphic_type_declarations(state, param.type_node);
        } else {
            collect_polymorphic_type_declarations(state, param);
        }
    }
    for arg: node.args collect_polymorphic_type_declarations(state, arg);
}

add_declaration_symbol :: (state: *Resolver_State, declaration: *Node, top_level: bool) {
    if declaration == null return;

    if declaration.kind == .LOCAL_GROUP_DECL {
        for child: declaration.declarations add_declaration_symbol(state, child, top_level);
        return;
    }

    original_name := declaration.name;
    symbol_name := original_name;
    polymorphic_parameter := declaration.kind == .PARAM && original_name.count > 0 && original_name[0] == #char "$";
    if polymorphic_parameter symbol_name = resolver_polymorphic_type_name(original_name);

    kind := Resolver_Symbol_Kind.LOCAL;
    if top_level {
        if declaration.kind == .FUNCTION kind = .FUNCTION;
        else if declaration.kind == .STRUCT_DECL kind = .STRUCT;
        else if declaration.kind == .ENUM_DECL kind = .ENUM;
        else kind = .GLOBAL;
    } else {
        if declaration.kind == .FUNCTION kind = .FUNCTION;
        else if declaration.kind == .STRUCT_DECL kind = .STRUCT;
        else if declaration.kind == .ENUM_DECL kind = .ENUM;
        else if declaration.kind == .PARAM kind = .PARAM;
        else if declaration.kind == .STRUCT_FIELD kind = .FIELD;
        else kind = .LOCAL;
    }

    if is_declaration_symbol_kind(declaration.kind) {
        symbol := add_symbol(state, state.current_scope, symbol_name, kind, declaration);
        if original_name != symbol_name resolver_scope_add_symbol_alias(state.current_scope, original_name, symbol);
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

    for param: fn.params collect_polymorphic_type_declarations(state, param.type_node);
    collect_polymorphic_type_declarations(state, fn.return_type);

    for param: fn.params {
        add_declaration_symbol(state, param, false);
        resolve_type_reference(state, param.type_node);
        resolve_expression(state, param.rhs);
    }

    resolve_type_reference(state, fn.return_type);
    for directive: fn.statements resolve_statement(state, directive);
    resolve_block_contents(state, fn.body, create_scope = true);
}

resolve_struct_declaration :: (state: *Resolver_State, node: *Node) {
    push_scope(state);
    defer pop_scope(state);

    for param: node.params {
        add_declaration_symbol(state, param, false);
        resolve_type_reference(state, param.type_node);
        resolve_expression(state, param.rhs);
    }

    for field: node.declarations {
        if field.kind == .STRUCT_FIELD {
            add_declaration_symbol(state, field, false);
            resolve_type_reference(state, field.type_node);
            resolve_expression(state, field.lhs);
            resolve_expression(state, field.rhs);
        } else if field.kind == .COMPILER_DIRECTIVE_STATEMENT {
            resolve_statement(state, field);
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
            push_scope(state);
            for iterator: statement.params add_declaration_symbol(state, iterator, false);
            resolve_statement_body(state, statement.body, create_scope = false);
            pop_scope(state);

        case .WHILE_STATEMENT;
            resolve_expression(state, statement.lhs);
            resolve_statement_body(state, statement.body, create_scope = true);

        case .DEFER_STATEMENT;
            resolve_statement_body(state, statement.body, create_scope = true);

        case .COMPILER_DIRECTIVE_STATEMENT;
            resolve_type_reference(state, statement.return_type);
            resolve_expression(state, statement.lhs);
            resolve_statement_body(state, statement.body, create_scope = true);
            if statement.rhs != null {
                if statement.rhs.kind == .BLOCK resolve_block_contents(state, statement.rhs, create_scope = true);
                else resolve_statement(state, statement.rhs);
            }

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
            for param: expression.params collect_polymorphic_type_declarations(state, param.type_node);
            collect_polymorphic_type_declarations(state, expression.return_type);
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
            for param: expression.params {
                if param != null && param.kind == .PARAM {
                    resolve_type_reference(state, param.type_node);
                    resolve_expression(state, param.rhs);
                } else {
                    resolve_type_reference(state, param);
                }
            }
            resolve_type_reference(state, expression.return_type);

        case .VARIADIC_TYPE;
            resolve_type_reference(state, expression.type_node);

        case .POINTER_TYPE;
            resolve_type_reference(state, expression.type_node);

        case .ARRAY_TYPE;
            resolve_expression(state, expression.rhs);
            resolve_type_reference(state, expression.type_node);

        case .POLYMORPHIC_TYPE;
            if expression.name.count > 0 {
                symbol := find_symbol(state.current_scope, expression.name);
                if symbol == null || symbol.declaration_node != expression add_reference(state, expression, expression.name);
            }
            resolve_type_reference(state, expression.type_node);

        case .INTERFACE_TYPE;
            resolve_type_reference(state, expression.type_node);

        case;
    }
}

is_builtin_type_name :: (name: string) -> bool {
    if name == "void" return true;
    if name == "bool" return true;
    if name == "string" return true;
    if name == "int" return true;
    if name == "s8" return true;
    if name == "s16" return true;
    if name == "s32" return true;
    if name == "s64" return true;
    if name == "u8" return true;
    if name == "u16" return true;
    if name == "u32" return true;
    if name == "u64" return true;
    if name == "float" return true;
    if name == "float32" return true;
    if name == "float64" return true;
    if name == "sized_int" return true;
    if name == "type" return true;
    if name == "Any" return true;
    return false;
}

resolve_type_reference :: (state: *Resolver_State, type_node: *Node) {
    if type_node == null return;
    if type_node.kind == .TYPE_NAME && is_builtin_type_name(type_node.name) return;
    if type_node.kind == .TYPE_NAME && type_node.args.count > 0 {
        // Qualified types retain one NameRef-like node per path segment. The
        // combined TYPE_NAME remains available to type inference, while each
        // segment can independently inherit its declaration's semantic role.
        for segment: type_node.args resolve_expression(state, segment);
        return;
    }
    resolve_expression(state, type_node);
}

add_reference :: (state: *Resolver_State, node: *Node, name: string) {
    reference: Resolver_Reference;
    reference.name = name;
    reference.node = node;
    reference.symbol_bucket = find_symbol_bucket(state.current_scope, name);
    if reference.symbol_bucket != null && reference.symbol_bucket.symbols.count > 0 {
        reference.symbol = reference.symbol_bucket.symbols[0];
    }
    reference.file_path = state.file_path;
    reference.scope = state.current_scope;
    array_add(*state.result.references, reference);
}

find_symbol :: (scope: *Resolver_Scope, name: string) -> *Resolver_Symbol {
    bucket := find_symbol_bucket(scope, name);
    if bucket == null || bucket.symbols.count == 0 return null;
    return bucket.symbols[0];
}

find_symbol_bucket :: (scope: *Resolver_Scope, name: string) -> *Resolver_Symbol_Bucket {
    current := scope;
    while current != null {
        bucket := find_symbol_bucket_in_scope(current, name);
        if bucket != null return bucket;
        current = current.parent;
    }
    return null;
}

find_symbol_bucket_in_scope :: (scope: *Resolver_Scope, name: string) -> *Resolver_Symbol_Bucket {
    if scope == null return null;
    found, bucket := table_find(*scope.symbol_bucket_by_name, name);
    if !found return null;
    return bucket;
}

find_symbol_in_scope :: (scope: *Resolver_Scope, name: string) -> *Resolver_Symbol {
    bucket := find_symbol_bucket_in_scope(scope, name);
    if bucket == null || bucket.symbols.count == 0 return null;
    return bucket.symbols[0];
}

find_symbol_for_declaration_in_scope :: (scope: *Resolver_Scope, name: string, declaration_node: *Node) -> *Resolver_Symbol {
    bucket := find_symbol_bucket_in_scope(scope, name);
    if bucket == null return null;
    for symbol: bucket.symbols {
        if symbol != null && symbol.declaration_node == declaration_node return symbol;
    }
    return null;
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

resolver_result_deinit :: (result: *Resolver_Result) {
    if result == null return;

    for symbol: result.symbols {
        if symbol != null free(symbol);
    }
    for scope: result.scopes {
        if scope == null continue;
        for bucket: scope.symbol_buckets {
            if bucket == null continue;
            array_free(bucket.symbols);
            free(bucket);
        }
        array_free(scope.symbols);
        deinit(*scope.symbols_by_name);
        array_free(scope.symbol_buckets);
        deinit(*scope.symbol_bucket_by_name);
        free(scope);
    }

    array_free(result.directory_scopes);
    array_free(result.file_scopes);
    array_free(result.symbols);
    array_free(result.references);
    array_free(result.scopes);
    result.* = .{};
}
