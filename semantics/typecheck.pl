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

% def (multiple params)
typecheck(Assigns,
          def(Name, function(Params, Body)),
          typed(def(Name, function(Params, ResolvedBody)), Type)) :-
    length(Params, L), L > 1,
    get_assoc(Name, Assigns, Type),
    Type = morphism(SourceType, TargetType),
    (SourceType = typeproduct(SourceTypes) ; SourceType = typeref(typeproduct(SourceTypes))),
    foldl(assign_type, SourceTypes, Params, Assigns, AugmentedAssigns),
    typecheck(AugmentedAssigns, Body, AnnotatedBody),
    resolve_pointers(AnnotatedBody, TargetType, PartialResolvedBody),
    (resolve_heap_copy(PartialResolvedBody, ResolvedBody)
        ; ResolvedBody = PartialResolvedBody).

% def (single param)
typecheck(Assigns,
          def(Name, function([Param], Body)),
          typed(def(Name, function([Param], ResolvedBody)), Type)) :-
    get_assoc(Name, Assigns, Type),
    Type = morphism(SourceType, TargetType),
    assign_type(SourceType, Param, Assigns, AugmentedAssigns),
    typecheck(AugmentedAssigns, Body, AnnotatedBody),
    resolve_pointers(AnnotatedBody, TargetType, PartialResolvedBody),
    (resolve_heap_copy(PartialResolvedBody, ResolvedBody)
        ; ResolvedBody = PartialResolvedBody).

% def (no param)
typecheck(Assigns,
          def(Name, function([], Body)),
          typed(def(Name, function([], ResolvedBody)), Type)) :-
    get_assoc(Name, Assigns, Type),
    Type = morphism("End", TargetType),
    typecheck(Assigns, Body, AnnotatedBody),
    resolve_pointers(AnnotatedBody, TargetType, PartialResolvedBody),
    resolve_heap_copy(PartialResolvedBody, ResolvedBody).

% struct
typecheck(Assigns,
          struct(Expressions),
          typed(struct(TypedExpressions), typeproduct(Types))) :-
    maplist(typecheck(Assigns), Expressions, TypedExpressions),
    maplist(arg(2), TypedExpressions, Types).

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

typecheck(_Assigns,
          literal(nil),
          typed(literal(nil), "End")).

% var
typecheck(Assigns,
          var(Name),
          typed(var(Name), Type)) :-
    get_assoc(Name, Assigns, Type).

% call (multiple arguments)
typecheck(Assigns,
          call(Name, Arg),
          typed(call(Name, ResolvedArg), TargetType)) :- 
    Arg = struct(_),
    get_assoc(Name, Assigns, morphism(SourceType, TargetType)),
    (SourceType = typeproduct(SourceTypes) ; SourceType = typeref(typeproduct(SourceTypes))),
    % typecheck the struct argument and children
    typecheck(Assigns, Arg, typed(struct(AnnotatedArgs), _)),
    % resolve pointers on each individual child of the struct and recompose
    maplist(resolve_pointers, AnnotatedArgs, SourceTypes, ResolvedArgs),
    maplist(arg(2), ResolvedArgs, ResolvedTypes),
    AnnotatedArg = typed(struct(ResolvedArgs), typeproduct(ResolvedTypes)),
    % resolve pointer on recomposed struct
    resolve_pointers(AnnotatedArg, SourceType, ResolvedArg).

% call (single argument)
typecheck(Assigns,
          call(Name, Arg),
          typed(call(Name, ResolvedArg), TargetType)) :-
    Arg \= struct(_),
    get_assoc(Name, Assigns, morphism(SourceType, TargetType)),
    typecheck(Assigns, Arg, AnnotatedArg),
    resolve_pointers(AnnotatedArg, SourceType, ResolvedArg).

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
% otherwise nothing needs to be done
resolve_heap_copy(X, X) :- X \= typed(reference(_), _).
