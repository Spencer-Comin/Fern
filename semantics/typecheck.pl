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
          def(Name, function(TypedParams, ResolvedBody)): F) :-
    length(Params, L), L > 1,
    get_assoc(Name, Assigns, F),
    F = S => T,
    S = *Ss,
    foldl(assign_type, Ss, Params, Assigns, AugmentedAssigns),
    maplist(with_type, Params, Ss, TypedParams),
    typecheck(AugmentedAssigns, Body, ResolvedBody),
    has_type(ResolvedBody, T).

% def (single param)
typecheck(Assigns,
          def(Name, function([Param], Body)),
          def(Name, function([Param: S], ResolvedBody)): F) :-
    get_assoc(Name, Assigns, F),
    F = S => T,
    assign_type(S, Param, Assigns, AugmentedAssigns),
    typecheck(AugmentedAssigns, Body, ResolvedBody),
    has_type(ResolvedBody, T).

% def (no param)
typecheck(Assigns,
          def(Name, function([], Body)),
          def(Name, function([], ResolvedBody)): F) :-
    get_assoc(Name, Assigns, F),
    F = "End" => T,
    typecheck(Assigns, Body, ResolvedBody),
    has_type(ResolvedBody, T).

typecheck(Assigns,
          def(Name, Body),
          TypedBody) :-
    Body \= function(_, _),
    typecheck(Assigns, def(Name, function([], Body)), TypedBody).

% TODO: figure multiple params and no param cases after figuring out single param
% lambda (single param)
typecheck(Assigns,
          function([Param], Body),
          lambda([Param: S], Captures, ResolvedBody): F) :-
    freeze(F,
        (F = S => T,
        assign_type(S, Param, Assigns, AugmentedAssigns),
        typecheck(AugmentedAssigns, Body, ResolvedBody),
        has_type(ResolvedBody, T),
        captures(Assigns, ResolvedBody, Vars),
        subtract(Vars, [Param: S], Captures))).

typecheck(Assigns,
          function(Params, Body),
          lambda(TypedParams, Captures, ResolvedBody): F) :-
    freeze(F,
        (F = S => T,
        S = *Ss,
        foldl(assign_type, Ss, Params, Assigns, AugmentedAssigns),
        maplist(with_type, Params, Ss, TypedParams),
        typecheck(AugmentedAssigns, Body, ResolvedBody),
        has_type(ResolvedBody, T),
        captures(Assigns, ResolvedBody, Vars),
        subtract(Vars, TypedParams, Captures))).

typecheck(Assigns,
          function([], Body),
          lambda([], Captures, ResolvedBody): F) :-
    freeze(F,
        (F = "End" => T,
        typecheck(Assigns, Body, ResolvedBody),
        has_type(ResolvedBody, T),
        captures(Assigns, ResolvedBody, Captures))).

% struct
typecheck(Assigns,
          struct(Expressions),
          struct(TypedExpressions): *Ts) :-
    maplist(typecheck(Assigns), Expressions, TypedExpressions),
    maplist(arg(2), TypedExpressions, Ts).

% binary
% TODO: add case for pointer arithmetic
typecheck(Assigns,
          binary(Op, Left, Right),
          binary(Op, ResolvedLeft, ResolvedRight): T) :-
    arithmetic_op(Op),
    typecheck(Assigns, Left, ResolvedLeft),
    typecheck(Assigns, Right, ResolvedRight),
    has_type(ResolvedLeft, T),
    has_type(ResolvedRight, T),
    T <: "Number".

typecheck(Assigns,
          binary(Op, Left, Right),
          binary(Op, ResolvedLeft, ResolvedRight): "Bool") :-
    comparison_op(Op),
    typecheck(Assigns, Left, ResolvedLeft),
    typecheck(Assigns, Right, ResolvedRight),
    has_type(ResolvedLeft, T),
    has_type(ResolvedRight, T),
    T <: "Number".

typecheck(Assigns,
          reference(Body),
          reference(ResolvedBody): &T) :-
    typecheck(Assigns, Body, ResolvedBody),
    has_type(ResolvedBody, T).

typecheck(Assigns,
          dereference(Body),
          dereference(ResolvedBody): T) :-
    typecheck(Assigns, Body, ResolvedBody),
    has_type(ResolvedBody, &T).

% number
typecheck(_Assigns,
          literal(int(Num)),
          literal(number(Num)): T) :-
    freeze(T, T <: "Int").

typecheck(_Assigns,
          literal(fp(Num)),
          literal(number(Num)): T) :-
    freeze(T, T <: "Float").

typecheck(_Assigns,
          literal(nil),
          literal(nil): "End").

% var
typecheck(Assigns,
          var(Name),
          var(Name): T) :-
    get_assoc(Name, Assigns, T).

% call
typecheck(Assigns,
          call(Func, Arg),
          call(ResolvedFunc, ResolvedArg): T) :-
    typecheck(Assigns, Arg, ResolvedArg),
    has_type(ResolvedArg, S),
    freeze(T, has_type(ResolvedFunc, S => T)),
    typecheck(Assigns, Func, ResolvedFunc).

% if
typecheck(Assigns,
          if(Cond, Then, Else),
          if(ResolvedCond, ResolvedThen, ResolvedElse): T) :-
    has_type(ResolvedCond, "Bool"),
    has_type(ResolvedThen, T),
    has_type(ResolvedElse, T),
    typecheck(Assigns, Cond, ResolvedCond),
    typecheck(Assigns, Then, ResolvedThen),
    typecheck(Assigns, Else, ResolvedElse).

% import
typecheck(Assigns,
          import(_, Name),
          declare(Name): T) :-
    get_assoc(Name, Assigns, T).

% error
typecheck(_, BadNode, _) :- throw(type_error(BadNode)).

% binary ops
arithmetic_op(+).
arithmetic_op(-).
arithmetic_op(*).
arithmetic_op(/).

comparison_op(<).
comparison_op(>).

% captures(+Assigns, +Node, -Captures).
captures(_, literal(_): _, []).
captures(Assigns, var(Name): T, Capture) :- get_assoc(Name, Assigns, T) -> Capture = [Name: T] ; Capture = [].
captures(_, lambda(_, Captures, _): _, Captures).  % TODO: subtract parent params from child captures

captures(Assigns, struct(Nodes): _, Captures) :-
    maplist(captures(Assigns), Nodes, CaptureList),
    ord_union(CaptureList, Captures).

captures(Assigns, binary(_, Left, Right): _, Captures) :-
    captures(Assigns, Left, LeftCaptures),
    captures(Assigns, Right, RightCaptures),
    ord_union(LeftCaptures, RightCaptures, Captures).

captures(Assigns, call(Func, Arg): _, Captures) :-
    captures(Assigns, Func, FuncCaptures),
    captures(Assigns, Arg, ArgCaptures),
    ord_union(FuncCaptures, ArgCaptures, Captures).

captures(Assigns, if(Cond, Then, Else): _, Captures) :-
    captures(Assigns, Cond, CondCaptures),
    captures(Assigns, Then, ThenCaptures),
    captures(Assigns, Else, ElseCaptures),
    ord_union([CondCaptures, ThenCaptures, ElseCaptures], Captures).
