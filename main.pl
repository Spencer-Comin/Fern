:- module(main, [parse_fern_source/3, fern_compile/1]).
:- use_module(parsing/lexer).
:- use_module(parsing/parser).
:- use_module(codegen/codegen).
:- use_module(semantics/typecheck).
:- use_module(semantics/imports).


fern_compile(File) :-
    parse_fern_source(File, TypedAST),
    prep_codegen,
    maplist(jit, TypedAST).

fern_ir(File) :-
    parse_fern_source(File, TypedAST),
    prep_codegen,
    maplist(llvm_print, TypedAST).

parse_fern_source(File, CompleteTypeInfo, ResolvedAST) :-
    (read_file_to_codes(File, Codes, []) ;
        throw(code_error)),
    (tokenize(Codes, Tokens) ;
        throw(lexical_error)),
    (parse(Tokens, TypeInfo, AST) ;
        throw(syntax_error)),
    (resolve_imports(TypeInfo, AST, CompleteTypeInfo, ResolvedAST) ;
        throw(import_error)).

parse_fern_source(File, TypedAST) :-
    parse_fern_source(File, CompleteTypeInfo, ResolvedAST),
    (typecheck(CompleteTypeInfo, ResolvedAST, TypedAST) ;
        print_term((ast(ResolvedAST), CompleteTypeInfo), []), throw(type_error)).
