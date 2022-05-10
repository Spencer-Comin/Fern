:- module(main, [parse_fern_source/3, fern_run/1]).
:- use_module(parsing/lexer).
:- use_module(parsing/parser).
:- use_module(codegen/codegen).
:- use_module(semantics/typecheck).
:- use_module(semantics/imports).


fern_run(File) :-
    parse_fern_source(File, TypedAST),
    prep_codegen,
    compile(TypedAST),
    jit_call("main").

fern_ir(File) :-
    parse_fern_source(File, TypedAST),
    prep_codegen,
    llvm_print(TypedAST).

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
    catch(typecheck(CompleteTypeInfo, ResolvedAST, TypedAST),
          TypeError,
          handle_type_error(CompleteTypeInfo, ResolvedAST, TypeError)).

handle_type_error(TypeInfo, AST, Error) :-
    Error = type_error(_),
    print_term(Error, [indent_arguments(true)]), nl,
    print_term(AST, [indent_arguments(true)]), nl,
    print_term(TypeInfo, [indent_arguments(true)]), nl,
    throw(Error).
