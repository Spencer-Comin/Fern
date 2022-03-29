:- module(typecheck, [typecheck/3]).
:- use_module(types).
:- use_module('../codegen/codegen').

% check_type_statements(+Statements, -Assigns).
check_type_statements([], Assigns) :-
    empty_assoc(Assigns).

check_type_statements([type_assignment(Type, Names)|R], Assigns) :-
    check_type_statements(R, OldAssigns),
    foldl(assign_type(Type), Names, OldAssigns, Assigns).

assign_type(Type, Name, OldAssigns, NewAssigns) :-
    put_assoc(Name, OldAssigns, Type, NewAssigns).

typecheck(typeinfo(TypeStatements), AST, AnnotatedAST) :-
    check_type_statements(TypeStatements, Assigns),
    maplist(typecheck(Assigns), AST, AnnotatedAST).

% TODO: fix pointer type checking

% def
typecheck(Assigns,
          def(Name, function(Params, Body)),
          typed(def(Name, function(Params, ResolvedBody)), Type)) :-
    
    get_assoc(Name, Assigns, Type),
    Type = morphism(SourceType, TargetType),
    length(Params, ParamCount),
    decompose(SourceType, ParamCount, ParamTypes),
    foldl(assign_type, ParamTypes, Params, Assigns, AugmentedAssigns),
    typecheck(AugmentedAssigns, Body, AnnotatedBody),
    resolve_pointers(AnnotatedBody, TargetType, PartialResolvedBody),
    (resolve_heap_copy(PartialResolvedBody, ResolvedBody)
        ; ResolvedBody = PartialResolvedBody).

typecheck(Assigns,
          def(Name, Expression),
          typed(def(Name, AnnotatedExpression), Type)) :-
    get_assoc(Name, Assigns, Type),
    typecheck(Assigns, Expression, AnnotatedExpression).

% binary
% TODO: add case for pointer arithmetic
typecheck(Assigns,
          binary(Op, Left, Right),
          typed(binary(Op, AnnotatedLeft, AnnotatedRight), BinaryType)) :-
    typecheck(Assigns, Left, AnnotatedLeft),
    typecheck(Assigns, Right, AnnotatedRight),
    typed(_, RightType) = AnnotatedRight,
    typed(_, LeftType) = AnnotatedLeft,
    binary_type(Op, LeftType, RightType, BinaryType).

% number
% TODO: switch to check that the number fits within the max and min value of the type
typecheck(_Assigns,
          literal(number(Num)),
          typed(literal(number(Num)), Type)) :-
    bounds(Type, Min, Max),
    Num =< Max, Num >= Min.

% var
typecheck(Assigns,
          var(Name),
          typed(var(Name), Type)) :-
    get_assoc(Name, Assigns, Type).

% call
typecheck(Assigns,
          call(Name, Args),
          typed(call(Name, ResolvedArgs), TargetType)) :- 
    get_assoc(Name, Assigns, morphism(SourceType, TargetType)),
    maplist(typecheck(Assigns), Args, AnnotatedArgs),
    length(Args, ArgCount),    
    decompose(SourceType, ArgCount, SourceTypes),
    maplist(resolve_pointers, AnnotatedArgs, SourceTypes, ResolvedArgs).

% if
typecheck(Assigns,
          if(Cond, Then, Else),
          typed(if(AnnotatedCond, AnnotatedThen, AnnotatedElse), Type)) :-
    typecheck(Assigns, Cond, AnnotatedCond),
    typecheck(Assigns, Then, AnnotatedThen),
    typecheck(Assigns, Else, AnnotatedElse),
    AnnotatedCond = typed(_, "Bool"),
    AnnotatedThen = typed(_, Type),
    AnnotatedElse = typed(_, Type).

% import
typecheck(Assigns,
          import(_, Name),
          typed(declare(Name), Type)) :-
    get_assoc(Name, Assigns, Type).

% resolvers

% given node of type T while looking for T, good
resolve_pointers(typed(Node, T), T, typed(Node, T)).
% given node of type T while looking for ref(T), nest in reference
resolve_pointers(typed(Node, T), typeref(T), typed(reference(typed(Node, T)), typeref(T))).
% given node of type ref(T) while looking for T, nest in dereference
resolve_pointers(typed(Node, typeref(T)), T, typed(dereference(typed(Node, typeref(T))), T)).

% given referenced node that needs to be on heap, copy to heap
resolve_heap_copy(typed(reference(N), T), typed(heap_copy(N), T)).
