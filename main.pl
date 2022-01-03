:- use_module(parsing/lexer).
:- use_module(parsing/parser).
:- use_module(codegen/codegen).
:- use_module(semantics/typecheck).

fern_compile(File) :-
    read_file_to_codes(File, Codes, []),
    tokenize(Codes, Tokens), !,
    parse(Tokens, TypeInfo, AST), !,
    % trace,
    typecheck(TypeInfo, AST, TypedAST),
    % notrace, nodebug,
    % print_term(TypedAST, []), nl,
    reset_generator,
    % trace,
    maplist(jit, TypedAST),
    % notrace, nodebug,
    !.

fern_ir(File) :-
    read_file_to_codes(File, Codes, []),
    tokenize(Codes, Tokens), !,
    parse(Tokens, TypeInfo, AST), !,
    % trace,
    typecheck(TypeInfo, AST, TypedAST),
    % notrace, nodebug,
    print_term(TypedAST, []), nl,
    reset_generator,
    % trace,
    maplist(llvm_print, TypedAST),
    % notrace, nodebug.
    !.
