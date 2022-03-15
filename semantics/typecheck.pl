:- module(typecheck, [typecheck/3]).
:- use_module(types).
:- use_module('../codegen/codegen').

% check_type_statements(+Statements, -Assigns, -Typedefs).
check_type_statements([], Assigns, Typedefs) :-
    empty_assoc(Assigns), empty_assoc(Typedefs).

check_type_statements([type_assignment(Type, Names)|R], Assigns, Typedefs) :-
    check_type_statements(R, OldAssigns, Typedefs),
    foldl(assign_type(Type), Names, OldAssigns, Assigns).

check_type_statements([typedef(Name, Type)|R], Assigns, Typedefs) :-
    put_assoc(Name, OldTypedefs, Type, Typedefs),
    check_type_statements(R, Assigns, OldTypedefs).

assign_type(Type, Name, OldAssigns, NewAssigns) :-
    put_assoc(Name, OldAssigns, Type, NewAssigns).

typecheck(typeinfo(TypeStatements), AST, AnnotatedAST) :-
    check_type_statements(TypeStatements, Assigns, Typedefs),
    maplist(typecheck(Assigns, Typedefs), AST, AnnotatedAST).

% TODO: fix pointer type checking
% TODO: make sure all typedefs are resolved appropriately

% def
typecheck(Assigns, Typedefs,
          def(Name, function(Params, Body)),
          typed(def(Name, function(Params, ResolvedBody)), Type)) :-
    
    get_assoc(Name, Assigns, Type),
    Type = morphism(ListedSourceType, ListedTargetType),
    resolve_typedef(ListedSourceType, Typedefs, SourceType),
    resolve_typedef(ListedTargetType, Typedefs, TargetType),
    length(Params, ParamCount),
    decompose(SourceType, ParamCount, ParamTypes),
    foldl(assign_type, ParamTypes, Params, Assigns, AugmentedAssigns),
    typecheck(AugmentedAssigns, Typedefs, Body, AnnotatedBody),
    resolve_pointers(AnnotatedBody, TargetType, PartialResolvedBody),
    (resolve_heap_copy(PartialResolvedBody, ResolvedBody)
        ; ResolvedBody = PartialResolvedBody).

typecheck(Assigns, Typedefs,
          def(Name, Expression),
          typed(def(Name, AnnotatedExpression), Type)) :-
    get_assoc(Name, Assigns, Type),
    typecheck(Assigns, Typedefs, Expression, AnnotatedExpression).

% binary
% TODO: add case for pointer arithmetic
typecheck(Assigns, Typedefs,
          binary(Op, Left, Right),
          typed(binary(Op, AnnotatedLeft, AnnotatedRight), BinaryType)) :-
    typecheck(Assigns, Typedefs, Left, AnnotatedLeft),
    typecheck(Assigns, Typedefs, Right, AnnotatedRight),
    typed(_, RightType) = AnnotatedRight,
    typed(_, LeftType) = AnnotatedLeft,
    binary_type(Op, LeftType, RightType, BinaryType).

% number
% TODO: switch to check that the number fits within the max and min value of the type
typecheck(_Assigns, _Typedefs,
          literal(number(Num)),
          typed(literal(number(Num)), Type)) :-
    bounds(Type, Min, Max),
    Num =< Max, Num >= Min.

% var
typecheck(Assigns, _Typedefs,
          var(Name),
          typed(var(Name), Type)) :-
    get_assoc(Name, Assigns, Type).

% call
typecheck(Assigns, Typedefs,
          call(Name, Args),
          typed(call(Name, ResolvedArgs), TargetType)) :- 
    get_assoc(Name, Assigns, morphism(SourceType, TargetType)),
    maplist(typecheck(Assigns, Typedefs), Args, AnnotatedArgs),
    length(Args, ArgCount),    
    decompose(SourceType, ArgCount, SourceTypes),
    maplist(resolve_pointers, AnnotatedArgs, SourceTypes, ResolvedArgs).

% if
typecheck(Assigns, Typedefs,
          if(Cond, Then, Else),
          typed(if(AnnotatedCond, AnnotatedThen, AnnotatedElse), Type)) :-
    typecheck(Assigns, Typedefs, Cond, AnnotatedCond),
    typecheck(Assigns, Typedefs, Then, AnnotatedThen),
    typecheck(Assigns, Typedefs, Else, AnnotatedElse),
    AnnotatedCond = typed(_, "Bool"),
    AnnotatedThen = typed(_, Type),
    AnnotatedElse = typed(_, Type).

% import
typecheck(Assigns, _Typedefs,
          import(_, Name),
          typed(declare(Name), Type)) :-
    get_assoc(Name, Assigns, Type).

% resolvers

% only resolves one level of typedefs
resolve_typedef(Type, Typedefs, BaseType) :-
    get_assoc(Type, Typedefs, BaseType) ;
    BaseType = Type.

% given node of type T while looking for T, good
resolve_pointers(typed(Node, T), T, typed(Node, T)).
% given node of type T while looking for ref(T), nest in reference
resolve_pointers(typed(Node, T), typeref(T), typed(reference(typed(Node, T)), typeref(T))).
% given node of type ref(T) while looking for T, nest in dereference
resolve_pointers(typed(Node, typeref(T)), T, typed(dereference(typed(Node, typeref(T))), T)).

% given referenced node that needs to be on heap, copy to heap
resolve_heap_copy(typed(reference(N), T), typed(heap_copy(N), T)).
