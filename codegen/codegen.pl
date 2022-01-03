:- module(codegen, [
    generate/2,
    llvm_print/1,
    jit/1,
    reset_generator/0,
    llvm_type_print/1,
    typedef/2,
    type_assign/2
]).
:- use_module('../semantics/types').
:- use_foreign_library(foreign('codegen/libcodegen.dylib')).
:- table typegen/2.
:- discontiguous generate/2, generate/3.

% jit(+AST_Node)

jit(typed(def(Name, function(Params, Body)), T)) :-
    generate(typed(def(Name, function(Params, Body)), T), _).

jit(typed(Expression, T)) :-
    jit(typed(def("__anon_expr", function([], typed(Expression, T))), morphism("End", T))),
    jit_call("__anon_expr"), !.


% llvm_print(+AST_Node)

llvm_print(typed(def(Name, function(Params, Body)), T)) :-
    generate(typed(def(Name, function(Params, Body)), T), Node),
    print_function(Node), !.

llvm_print(Expression) :-
    generate(Expression, Node),
    print_expression(Node), nl.

llvm_type_print(T) :-
    typegen(T, LLVM_T),
    print_type(LLVM_T).


% generate(+AST_Node, -LLVM_Node)
% TODO: add reference and dereference node generators
% TODO: modify to add type specification

generate(def(Name, function(Params, Body)), Node) :-
    codegen_func_head(Name, Params, Node),
    generate(Body, BodyNode),
    codegen_func_body(Node, BodyNode).

generate(typed(def(Name, function(Params, Body)), T), Node) :-
    typegen(T, LLVM_T),
    codegen_func_head(Name, Params, LLVM_T, Node, NameArgPairs),
    list_to_assoc(NameArgPairs, ArgValues),
    generate(Body, ArgValues, BodyNode),
    codegen_func_body(Node, BodyNode).

generate(def(Name, Expression), Node) :-
    generate(def(Name, function([], Expression)), Node).

generate(binary(Op, Left, Right), Node) :-
    generate(Left, LeftNode),
    generate(Right, RightNode),
    codegen_binary(LeftNode, RightNode, Op, Node).

generate(typed(binary(Op, Left, Right), T), ArgValues, Node) :-
    generate(Left, ArgValues, LeftNode),
    generate(Right, ArgValues, RightNode),
    ((subtype(T, "Float"), codegen_fbinary(LeftNode, RightNode, Op, Node)) ;
    (subtype(T, "Int"), codegen_binary(LeftNode, RightNode, Op, Node))).

generate(literal(number(Num)), Node) :-
    codegen_number(Num, Node).

generate(typed(literal(number(Num)), T), _, Node) :-
    (bitwidth(T, N), subtype(T, "UInt"), codegen_int(N, Num, false, Node)) ;
    (bitwidth(T, N), subtype(T, "Int"), codegen_int(N, Num, true, Node)) ;
    (subtype(T, "Float"), codegen_float(Num, Node)).

generate(var(Name), Node) :-
    codegen_var(Name, Node).

generate(typed(var(Name), _), ArgValues, Node) :-
    get_assoc(Name, ArgValues, Node).

generate(call(Name, Args), Node) :-
    maplist(generate, Args, ArgNodes), !,
    (codegen_func_call(Name, ArgNodes, Node) ; throw("bad call")).

generate(typed(call(Name, Args), _), ArgValues, Node) :-
    maplist(generate_(ArgValues), Args, ArgNodes),  % this is wrong, arguments to generate are in wrong order
    codegen_func_call(Name, ArgNodes, Node).

generate(if(Cond, Then, Else), Node) :-
    generate(Cond, CondNode),
    codegen_if_cond(CondNode, ElseBlock, MergeBlock),
    generate(Then, ThenNode),
    codegen_start_if_else(ElseBlock, MergeBlock, ThenBlock),
    generate(Else, ElseNode),
    codegen_if_merge(MergeBlock, ThenBlock, ThenNode, ElseNode, Node).

generate(typed(if(Cond, Then, Else), _), ArgValues, Node) :-
    generate(Cond, ArgValues, CondNode),
    codegen_if_cond(CondNode, ElseBlock, MergeBlock),
    generate(Then, ArgValues, ThenNode),
    codegen_start_if_else(ElseBlock, MergeBlock, ThenBlock),
    generate(Else, ArgValues, ElseNode),
    codegen_if_merge(MergeBlock, ThenBlock, ThenNode, ElseNode, Node).

generate_(ArgValues, AST, Node) :- generate(AST, ArgValues, Node).

% typegen
typegen(T, LLVM_T) :-
    % fix this
    string(T),
    get_llvm_type(T, LLVM_T).

typegen(morphism(X, Y), LLVM_T) :-
    typegen(X, LLVM_X),
    typegen(Y, LLVM_Y),
    build_morphism(LLVM_X, LLVM_Y, LLVM_T).

typegen(typeproduct(Xs), LLVM_T) :-
    maplist(typegen, Xs, LLVM_Xs),
    build_type_product(LLVM_Xs, LLVM_T).

typegen(ref(X), LLVM_T) :-
    typegen(X, LLVM_X),
    build_reference(ref(LLVM_X), LLVM_T).

typedef(Name, Type) :-
    typegen(Type, LLVM_T),
    set_llvm_type(Name, LLVM_T).

type_assign(Name, Type) :-
    typegen(Type, LLVM_T),
    assign_llvm_type(Name, LLVM_T).
    