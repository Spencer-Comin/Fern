:- module(codegen, [
    generate_statement/4,
    llvm_print/1,
    compile/1,
    jit_call/1,
    prep_codegen/0,
    llvm_type_print/1,
    type_assign/2,
    dump_obj_file/1
]).
:- use_module('../semantics/types').
:- use_foreign_library(foreign('codegen/libcodegen.dylib')).
:- table typegen/2.
:- discontiguous generate/2, generate/3.


prep_codegen :- abolish_module_tables(codegen), reset_generator.

compile(AST) :-
    empty_assoc(InGlobals),
    foldl(generate_statement, AST, _Nodes, InGlobals, _OutGlobals).

llvm_print(AST) :-
    compile(AST),
    print_current_module.

llvm_print(def(Name, Func): T, InGlobals, OutGlobals) :-
    generate_statement(def(Name, Func): T, Node, InGlobals, OutGlobals),
    nl, nl, print_function(Node).

llvm_print(declare(Name): T, InGlobals, OutGlobals) :-
    generate_statement(declare(Name): T, func_ptr(Func, _LLVM_Type), InGlobals, OutGlobals),
    nl, print_function(Func).

llvm_type_print(T) :-
    typegen(T, LLVM_T),
    print_type(LLVM_T).

% generate_statement(+Statement, -LLVMNode, +InGlobals, -OutGlobals)

% def
generate_statement(def(Name, function(Params, Body)): T, Func, InGlobals, OutGlobals) :-
    typegen(T, LLVM_T),
    maplist(with_type, ParamNames, ParamTypes, Params),
    maplist(typegen, ParamTypes, LLVM_ParamTypes),
    codegen_func_head(Name, ParamNames, LLVM_ParamTypes, LLVM_T, Func, Node, NamesArgs),
    zip_to_pairs(NamesArgs, NameArgPairs),
    put_assoc(Name, InGlobals, Node, OutGlobals),
    assoc_to_list(OutGlobals, GlobalPairs),
    append(GlobalPairs, NameArgPairs, NamedPairs),
    list_to_assoc(NamedPairs, NamedValues),
    generate(Body, NamedValues, BodyNode),
    codegen_func_body(Func, BodyNode).

% declare
generate_statement(declare(Name): T, Node, InGlobals, OutGlobals) :-
    typegen(T, LLVM_T),
    codegen_declaration(Name, LLVM_T, Node),
    put_assoc(Name, InGlobals, Node, OutGlobals).

% binary
generate(binary(Op, Left, Right): "Bool", ArgValues, Node) :-
    generate(Left, ArgValues, LeftNode),
    generate(Right, ArgValues, RightNode),
    Left = _ : T,
    ((T <: "Float", codegen_fbinary(LeftNode, RightNode, Op, Node)) ;
    (T <: "Int", codegen_binary(LeftNode, RightNode, Op, Node))).

generate(binary(Op, Left, Right): T, ArgValues, Node) :-
    generate(Left, ArgValues, LeftNode),
    generate(Right, ArgValues, RightNode),
    ((T <: "Float", codegen_fbinary(LeftNode, RightNode, Op, Node)) ;
    (T <: "Int", codegen_binary(LeftNode, RightNode, Op, Node))).

% number
generate(literal(number(Num)): T, _, Node) :-
    (bitwidth(T, N), T <: "UInt", codegen_int(N, Num, false, Node)) ;
    (bitwidth(T, N), T <: "Int", codegen_int(N, Num, true, Node)) ;
    (T <: "Float", codegen_float(Num, Node)).

% nil
generate(literal(nil): "End", _ArgValues, 0).

% var
generate(var(Name): _, ArgValues, Node) :-
    get_assoc(Name, ArgValues, Node).

% call
generate(call(Func, Arg): T, ArgValues, Node) :-
    generate(Func, ArgValues, FuncNode),
    generate(Arg, ArgValues, ArgNode),
    typegen(T, LLVM_T),
    codegen_func_call(FuncNode, LLVM_T, ArgNode, Node).

% if
generate(if(Cond, Then, Else): _, ArgValues, Node) :-
    generate(Cond, ArgValues, CondNode),
    codegen_if_cond(CondNode, ElseBlock, MergeBlock),
    generate(Then, ArgValues, ThenNode),
    codegen_start_if_else(ElseBlock, MergeBlock, ThenBlock),
    generate(Else, ArgValues, ElseNode),
    codegen_if_merge(MergeBlock, ThenBlock, ThenNode, ElseNode, Node).

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

generate(cast(AST): T, ArgValues, Node) :-
    generate(AST, ArgValues, FromNode),
    typegen(T, LLVM_T),
    codegen_cast(FromNode, LLVM_T, Node).

generate(lambda(Params, Captures, Body): T, ArgValues, Node) :-
    typegen(T, LLVM_T),
    maplist(with_type, ParamNames, ParamTypes, Params),
    maplist(typegen, ParamTypes, LLVM_ParamTypes),
    maplist(with_type, CaptureNames, CaptureTypes, Captures),
    maplist(typegen, CaptureTypes, LLVM_CaptureTypes),
    codegen_lambda_head(ParamNames, LLVM_ParamTypes, CaptureNames, LLVM_CaptureTypes, LLVM_T, Lambda, NamesArgs, SavePoint, Trampoline),
    zip_to_pairs(NamesArgs, NameArgPairs),
    assoc_to_list(ArgValues, GlobalPairs),
    exclude(key_in(CaptureNames), GlobalPairs, UnshadowedPairs),
    append(UnshadowedPairs, NameArgPairs, NamedPairs),
    list_to_assoc(NamedPairs, NamedValues),
    generate(Body, NamedValues, BodyNode),
    maplist(get_value(ArgValues), CaptureNames, CaptureValues),
    codegen_lambda_body(Lambda, LLVM_T, BodyNode, Trampoline, SavePoint, CaptureValues, LLVM_CaptureTypes, Node).

generate(BadNode, ArgValues, LLVMNode) :-
    throw(bad_generate(BadNode, ArgValues, LLVMNode)).

generate_(ArgValues, AST, Node) :- generate(AST, ArgValues, Node).

% typegen
typegen(X => Y, LLVM_T) :-
    typegen(X, LLVM_X),
    typegen(Y, LLVM_Y),
    build_morphism(LLVM_X, LLVM_Y, LLVM_T).

typegen(*Xs, LLVM_T) :-
    maplist(typegen, Xs, LLVM_Xs),
    build_type_product(LLVM_Xs, LLVM_T).

typegen(&X, LLVM_T) :-
    typegen(X, LLVM_X),
    build_reference(LLVM_X, LLVM_T). 

typegen(T, LLVM_T) :-
    % fix this
    string(T),
    get_llvm_type(T, LLVM_T).

type_assign(Name, Type) :-
    typegen(Type, LLVM_T),
    assign_llvm_type(Name, LLVM_T).

% helpers
zip_to_pairs([], []).
zip_to_pairs([A,B|T], [A-B|R]) :- zip_to_pairs(T, R).

key_in(Names, Key-_) :- member(Key, Names).

get_value(Assoc, Key, Value) :- get_assoc(Key, Assoc, Value).
