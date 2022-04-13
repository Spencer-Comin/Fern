:- module(codegen, [
    generate/2,
    llvm_print/1,
    compile/1,
    jit_call/1,
    prep_codegen/0,
    llvm_type_print/1,
    type_assign/2
]).
:- use_module('../semantics/types').
:- use_foreign_library(foreign('codegen/libcodegen.dylib')).
% :- table typegen/2.
:- discontiguous generate/2, generate/3.


prep_codegen :- abolish_module_tables(codegen), reset_generator.

% compile(+AST_Node)

compile(def(Name, Func): T) :-
    generate(def(Name, Func): T, _).

compile(declare(Name): T) :-
    generate(declare(Name): T, _, _).

% llvm_print(+AST_Node)

llvm_print(def(Name, Func): T) :-
    generate(def(Name, Func): T, Node),
    nl, nl, print_function(Node).

llvm_print(declare(Name): T) :-
    generate(declare(Name): T, _, Node),
    nl, print_function(Node).

llvm_print(Expression: T) :-
    throw(llvm_print_error(Expression, T)),
    llvm_print(def("__anon_expr", function([], Expression: T)): morphism("End", T)).

llvm_type_print(T) :-
    typegen(T, LLVM_T),
    print_type(LLVM_T).


% generate(+AST_Node, -LLVM_Node)

% def
generate(def(Name, function(Params, Body)): T, Node) :-
    typegen(T, LLVM_T),
    codegen_func_head(Name, Params, LLVM_T, Node, NameArgPairs),
    list_to_assoc(NameArgPairs, ArgValues),
    generate(Body, ArgValues, BodyNode),
    codegen_func_body(Node, BodyNode).

% binary
generate(binary(Op, Left, Right): T, ArgValues, Node) :-
    generate(Left, ArgValues, LeftNode),
    generate(Right, ArgValues, RightNode),
    ((subtype(T, "Float"), codegen_fbinary(LeftNode, RightNode, Op, Node)) ;
    (subtype(T, "Int"), codegen_binary(LeftNode, RightNode, Op, Node))).

% number
generate(literal(number(Num)): T, _, Node) :-
    (bitwidth(T, N), subtype(T, "UInt"), codegen_int(N, Num, false, Node)) ;
    (bitwidth(T, N), subtype(T, "Int"), codegen_int(N, Num, true, Node)) ;
    (subtype(T, "Float"), codegen_float(Num, Node)).

% nil
generate(literal(nil): "End", _ArgValues, 0).

% var
generate(var(Name): _, ArgValues, Node) :-
    get_assoc(Name, ArgValues, Node).

% call
generate(call(Name, Arg): _, ArgValues, Node) :-
    generate(Arg, ArgValues, ArgNode),
    codegen_func_call(Name, ArgNode, Node).

% if
generate(if(Cond, Then, Else): _, ArgValues, Node) :-
    generate(Cond, ArgValues, CondNode),
    codegen_if_cond(CondNode, ElseBlock, MergeBlock),
    generate(Then, ArgValues, ThenNode),
    codegen_start_if_else(ElseBlock, MergeBlock, ThenBlock),
    generate(Else, ArgValues, ElseNode),
    codegen_if_merge(MergeBlock, ThenBlock, ThenNode, ElseNode, Node).

% declare
generate(declare(Name): T, _ArgValues, Node) :-
    typegen(T, LLVM_T),
    codegen_declaration(Name, LLVM_T, Node).

generate(reference(AST): _, ArgValues, Node) :-
    generate(AST, ArgValues, AST_Node),
    codegen_reference(AST_Node, _, Node).

generate(dereference(AST): T, ArgValues, Node) :-
    typegen(T, LLVM_T),
    generate(AST, ArgValues, AST_Node),
    codegen_reference(Node, LLVM_T, AST_Node).

generate(heap_copy(AST): _, ArgValues, Node) :-
    generate(AST, ArgValues, AST_Node),
    codegen_heap_copy(AST_Node, Node).

generate(struct(ASTNodes): T, ArgValues, Node) :-
    maplist(generate_(ArgValues), ASTNodes, MemberNodes),
    typegen(T, LLVM_T),
    codegen_struct(MemberNodes, LLVM_T, Node).

generate(BadNode, _, _) :-
    throw(bad_generate(BadNode)).

generate_(ArgValues, AST, Node) :- generate(AST, ArgValues, Node).

% typegen
typegen(morphism(X, Y), LLVM_T) :-
    typegen(X, LLVM_X),
    typegen(Y, LLVM_Y),
    build_morphism(LLVM_X, LLVM_Y, LLVM_T).

typegen(typeproduct(Xs), LLVM_T) :-
    maplist(typegen, Xs, LLVM_Xs),
    build_type_product(LLVM_Xs, LLVM_T).

typegen(typeref(X), LLVM_T) :-
    typegen(X, LLVM_X),
    build_reference(LLVM_X, LLVM_T). 

typegen(T, LLVM_T) :-
    % fix this
    string(T),
    get_llvm_type(T, LLVM_T).

type_assign(Name, Type) :-
    typegen(Type, LLVM_T),
    assign_llvm_type(Name, LLVM_T).
