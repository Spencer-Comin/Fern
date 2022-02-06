:- module(imports, [resolve_imports/4]).
:- use_module('../parsing/lexer').
:- use_module('../parsing/parser').


resolved_import(import_chain(["std", Name]), TypeInfo, import(["std"], Name)) :- 
    read_file_to_codes('runtime/std.types.frn', Codes, []),
    tokenize(Codes, Tokens),
    parse_type_info(Tokens, TypeInfo).

is_import(import_chain(_)).

resolve_imports(typeinfo(InTypeInfo), InAST, typeinfo(DeDupedOutTypeInfo), OutAST) :-
    partition(is_import, InAST, Imports, RestAST),
    maplist(resolved_import, Imports, NewTypeInfo, NewASTNodes),
    flatten(NewTypeInfo, FlatNewTypeInfo),
    append(InTypeInfo, FlatNewTypeInfo, OutTypeInfo),
    sort(OutTypeInfo, DeDupedOutTypeInfo),
    append(NewASTNodes, RestAST, OutAST).
