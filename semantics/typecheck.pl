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
          def(Name, function(Params, ResolvedBody)): Type) :-
    length(Params, L), L > 1,
    get_assoc(Name, Assigns, Type),
    Type = SourceType => TargetType,
    (SourceType = *SourceTypes ; SourceType = &(*SourceTypes)),
    foldl(assign_type, SourceTypes, Params, Assigns, AugmentedAssigns),
    typecheck(AugmentedAssigns, Body, AnnotatedBody),
    reconcile(AnnotatedBody, TargetType, PartialResolvedBody),
    (resolve_heap_copy(PartialResolvedBody, ResolvedBody)
        ; ResolvedBody = PartialResolvedBody).

% def (single param)
typecheck(Assigns,
          def(Name, function([Param], Body)),
          def(Name, function([Param], ResolvedBody)): Type) :-
    get_assoc(Name, Assigns, Type),
    Type = SourceType => TargetType,
    assign_type(SourceType, Param, Assigns, AugmentedAssigns),
    typecheck(AugmentedAssigns, Body, AnnotatedBody),
    reconcile(AnnotatedBody, TargetType, PartialResolvedBody),
    (resolve_heap_copy(PartialResolvedBody, ResolvedBody)
        ; ResolvedBody = PartialResolvedBody).

% def (no param)
typecheck(Assigns,
          def(Name, function([], Body)),
          def(Name, function([], ResolvedBody)): Type) :-
    get_assoc(Name, Assigns, Type),
    Type = "End" => TargetType,
    typecheck(Assigns, Body, AnnotatedBody),
    reconcile(AnnotatedBody, TargetType, PartialResolvedBody),
    resolve_heap_copy(PartialResolvedBody, ResolvedBody).

typecheck(Assigns,
          def(Name, Body),
          TypedBody) :-
    Body \= function(_, _),
    typecheck(Assigns, def(Name, function([], Body)), TypedBody).

% struct
typecheck(Assigns,
          struct(Expressions),
          struct(TypedExpressions): *Types) :-
    maplist(typecheck(Assigns), Expressions, TypedExpressions),
    maplist(arg(2), TypedExpressions, Types).

% binary
% TODO: add case for pointer arithmetic
typecheck(Assigns,
          binary(Op, Left, Right),
          binary(Op, ReconciledLeft, ReconciledRight): BinaryType) :-
    typecheck(Assigns, Left, AnnotatedLeft),
    typecheck(Assigns, Right, AnnotatedRight),
    _: RightType = AnnotatedRight,
    _: LeftType = AnnotatedLeft,
    binary_type(Op, LeftType, RightType, BinaryType),
    biggest(LeftType, RightType, BigType),
    reconcile(AnnotatedLeft,  BigType, ReconciledLeft),
    reconcile(AnnotatedRight, BigType, ReconciledRight).

% number
typecheck(_Assigns,
          literal(int(Num)),
          literal(number(Num)): Type) :-
    bounds(Type, Min, Max),
    Num =< Max, Num >= Min.

typecheck(_Assigns,
          literal(fp(Num)),
          literal(number(Num)): Type) :-
    Type <: "Float",
    bounds(Type, Min, Max),
    Num =< Max, Num >= Min.

typecheck(_Assigns,
          literal(nil),
          literal(nil): "End").

% var
typecheck(Assigns,
          var(Name),
          var(Name): Type) :-
    get_assoc(Name, Assigns, Type).

% call (multiple arguments)
typecheck(Assigns,
          call(Func, Arg),
          call(ResolvedFunc, ResolvedArg): TargetType) :- 
    Arg = struct(_),
    typecheck(Assigns, Func, AnnotatedFunc),
    reconcile(AnnotatedFunc, SourceType => TargetType, ResolvedFunc),
    (SourceType = *SourceTypes ; SourceType = &(*SourceTypes)),
    % typecheck the struct argument and children
    typecheck(Assigns, Arg, struct(AnnotatedArgs): _),
    % resolve pointers on each individual child of the struct and recompose
    maplist(reconcile, AnnotatedArgs, SourceTypes, ResolvedArgs),
    maplist(arg(2), ResolvedArgs, ResolvedTypes),
    AnnotatedArg = struct(ResolvedArgs): *ResolvedTypes,
    % resolve pointer on recomposed struct
    reconcile(AnnotatedArg, SourceType, ResolvedArg).

% call (single argument)
typecheck(Assigns,
          call(Func, Arg),
          call(ResolvedFunc, ResolvedArg): TargetType) :-
    Arg \= struct(_),
    typecheck(Assigns, Func, ResolvedFunc),
    ResolvedFunc = _: SourceType => TargetType,
    typecheck(Assigns, Arg, AnnotatedArg),
    reconcile(AnnotatedArg, SourceType, ResolvedArg).

% if
typecheck(Assigns,
          if(Cond, Then, Else),
          if(AnnotatedCond, ReconciledThen, ReconciledElse): Type) :-
    typecheck(Assigns, Cond, AnnotatedCond),
    typecheck(Assigns, Then, AnnotatedThen),
    typecheck(Assigns, Else, AnnotatedElse),
    AnnotatedCond = _: "Bool",
    AnnotatedThen = _: ThenType, promotable(ThenType, Type),
    AnnotatedElse = _: ElseType, promotable(ElseType, Type),
    reconcile(AnnotatedThen, Type, ReconciledThen),
    reconcile(AnnotatedElse, Type, ReconciledElse).

% import
typecheck(Assigns,
          import(_, Name),
          declare(Name): Type) :-
    get_assoc(Name, Assigns, Type).

% error
typecheck(_, BadNode, _) :- throw(type_error(BadNode)).

% resolvers

% given node of type T while looking for T, good
reconcile(Node: T, T, Node: T).
reconcile(Node: T, U, cast(Node: T): U) :- nonvar(T), nonvar(U), promotable(T, U).
% given node of type T while looking for ref(T), nest in reference
reconcile(Node: T, &T, reference(Node: T): &T).
reconcile(Node: T, &U, reference(cast(Node: T): U): &U) :- nonvar(T), nonvar(U), promotable(T, U).
% given node of type ref(T) while looking for T, nest in dereference
reconcile(Node: &T, T, dereference(Node: &T): T).
reconcile(Node: &T, U, dereference(cast(Node: &T): &U): U) :- nonvar(T), nonvar(U), promotable(T, U).

% given referenced node that needs to be on heap, copy to heap
resolve_heap_copy(reference(N): T, heap_copy(N): T).
% otherwise nothing needs to be done
resolve_heap_copy(X, X) :- X \= reference(_): _.
