:- module(main, [parse_fern_source/4, fern_run/1, fern_compile/2]).
:- use_module(parsing/lexer).
:- use_module(parsing/parser).
:- use_module(codegen/codegen).
:- use_module(semantics/typecheck).
:- use_module(semantics/imports).

fern_mode(release).

fern_run(File) :-
    fern_mode(M),
    parse_fern_source(File, TypedAST, M),
    prep_codegen,
    compile(TypedAST),
    jit_call("main").

fern_compile(SourceFile, OutFile) :-
    fern_mode(M),
    parse_fern_source(SourceFile, TypedAST, M),
    prep_codegen,
    compile(TypedAST),
    dump_obj_file(OutFile).

fern_ir(File) :-
    fern_mode(M),
    parse_fern_source(File, TypedAST, M),
    prep_codegen,
    llvm_print(TypedAST).

parse_fern_source(File, CompleteTypeInfo, ResolvedAST, release) :-
    (read_file_to_codes(File, Codes, []) ; throw(code_error)),
    (tokenize(Codes, Tokens) ; throw(lexical_error)),
    (parse(Tokens, TypeInfo, AST) ; throw(syntax_error)),
    (resolve_imports(TypeInfo, AST, CompleteTypeInfo, ResolvedAST) ; throw(import_error)).

parse_fern_source(File, CompleteTypeInfo, ResolvedAST, debug) :-
    (time(read_file_to_codes(File, Codes, [])), format("read file successfully\n") ;
        throw(code_error)),
    (time(tokenize(Codes, Tokens)), format("tokenized successfully\n") ;
        throw(lexical_error)),
    (time(parse(Tokens, TypeInfo, AST)), format("parsed successfully\n") ;
        throw(syntax_error)),
    (time(resolve_imports(TypeInfo, AST, CompleteTypeInfo, ResolvedAST)), format("resolved imports successfully\n") ;
        throw(import_error)),
    flush_output.

parse_fern_source(File, TypedAST, release) :-
    parse_fern_source(File, CompleteTypeInfo, ResolvedAST, release),
    typecheck(CompleteTypeInfo, ResolvedAST, TypedAST).

parse_fern_source(File, TypedAST, debug) :-
    parse_fern_source(File, CompleteTypeInfo, ResolvedAST, debug),
    time(typecheck(CompleteTypeInfo, ResolvedAST, TypedAST)),
    format("typechecked successfully\n").
