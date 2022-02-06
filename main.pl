:- use_module(parsing/lexer).
:- use_module(parsing/parser).
:- use_module(codegen/codegen).
:- use_module(semantics/typecheck).
:- use_module(semantics/imports).


fern_compile(File) :-
    parse_fern_source(File, TypedAST),
    maplist(jit, TypedAST).

fern_ir(File) :-
    parse_fern_source(File, TypedAST),
    maplist(llvm_print, TypedAST).

parse_fern_source(File, TypedAST) :-
    (read_file_to_codes(File, Codes, []) ;
        throw(code_error)),
    (tokenize(Codes, Tokens) ;
        throw(lexical_error)),
    (parse(Tokens, TypeInfo, AST) ;
        throw(syntax_error)),
    (resolve_imports(TypeInfo, AST, CompleteTypeInfo, ResolvedAST) ;
        throw(import_error)),
    (typecheck(CompleteTypeInfo, ResolvedAST, TypedAST) ;
        throw(type_error)),
    abolish_module_tables(codegen),
    reset_generator.
