:- module(imports, [resolve_imports/4]).
:- use_module('../parsing/lexer').
:- use_module('../parsing/parser').
:- use_module('../main').


resolved_import(import_chain(Chain), TypeInfo, ASTNode) :-
    length(Chain, L), L > 2,
    append(PathList, [Name], Chain),
    atomic_list_concat(PathList, '/', Path),
    resolved_import(import_chain([Path, Name]), TypeInfo, ASTNode).

resolved_import(import_chain(["std", Name]), TypeInfo, import(["std"], Name)) :- 
    read_file_to_codes('runtime/std.types.frn', Codes, []),
    tokenize(Codes, Tokens),
    parse_type_info(Tokens, TypeInfo).

resolved_import(import_chain([Path, Name]), TypeInfo, ASTNode) :-
    dif(Path, "std"),
    atom_concat(Path, '.frn', FileName),
    parse_fern_source(FileName, typeinfo(TypeInfo), AST),
    member(ASTNode, AST),
    ASTNode = def(Name, _).

is_import(import_chain(_)).

resolve_imports(typeinfo(InTypeInfo), InAST, typeinfo(DeDupedOutTypeInfo), OutAST) :-
    partition(is_import, InAST, Imports, RestAST),
    maplist(resolved_import, Imports, NewTypeInfo, NewASTNodes),
    flatten(NewTypeInfo, FlatNewTypeInfo),
    append(InTypeInfo, FlatNewTypeInfo, OutTypeInfo),
    sort(OutTypeInfo, DeDupedOutTypeInfo),
    append(NewASTNodes, RestAST, OutAST).
