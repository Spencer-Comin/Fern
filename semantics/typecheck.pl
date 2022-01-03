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
    % trace,
    maplist(typecheck(Assigns, Typedefs), AST, AnnotatedAST).
    % notrace, nodebug.

% TODO: fix pointer type checking
% TODO: make sure all typedefs are resolved appropriately

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
    resolve_pointers(AnnotatedBody, TargetType, ResolvedBody).

typecheck(Assigns, Typedefs,
          def(Name, Expression),
          typed(def(Name, AnnotatedExpression), Type)) :-
    get_assoc(Name, Assigns, Type),
    typecheck(Assigns, Typedefs, Expression, AnnotatedExpression).

% TODO: add case for pointer arithmetic
typecheck(Assigns, Typedefs,
          binary(Op, Left, Right),
          typed(binary(Op, AnnotatedLeft, AnnotatedRight), BinaryType)) :-
    typecheck(Assigns, Typedefs, Left, AnnotatedLeft),
    typecheck(Assigns, Typedefs, Right, AnnotatedRight),
    typed(_, RightType) = AnnotatedRight,
    typed(_, LeftType) = AnnotatedLeft,
    binary_type(Op, LeftType, RightType, BinaryType).

% TODO: switch to check that the number fits within the max and min value of the type
typecheck(_Assigns, _Typedefs,
          literal(number(Num)),
          typed(literal(number(Num)), Type)) :-
    bitwidth(Type, W),
    msb(Num) < W.

typecheck(Assigns, _Typedefs,
          var(Name),
          typed(var(Name), Type)) :-
    get_assoc(Name, Assigns, Type).

typecheck(Assigns, Typedefs,
          call(Name, Args),
          typed(call(Name, ResolvedArgs), TargetType)) :- 
    get_assoc(Name, Assigns, morphism(SourceType, TargetType)),
    maplist(typecheck(Assigns, Typedefs), Args, AnnotatedArgs),
    length(Args, ArgCount),    
    decompose(SourceType, ArgCount, SourceTypes),
    maplist(resolve_pointers, AnnotatedArgs, SourceTypes, ResolvedArgs).

typecheck(Assigns, Typedefs,
          if(Cond, Then, Else),
          typed(if(AnnotatedCond, AnnotatedThen, AnnotatedElse), Type)) :-
    typecheck(Assigns, Typedefs, Cond, AnnotatedCond),
    typecheck(Assigns, Typedefs, Then, AnnotatedThen),
    typecheck(Assigns, Typedefs, Else, AnnotatedElse),
    AnnotatedCond = typed(_, "Bool"),
    AnnotatedThen = typed(_, Type),
    AnnotatedElse = typed(_, Type).

resolve_typedef(Type, Typedefs, BaseType) :-
    get_assoc(Type, Typedefs, LowerType) ->
    resolve_typedef(LowerType, Typedefs, BaseType);
    BaseType = Type.

% given node of type T while looking for T, good
resolve_pointers(typed(Node, T), T, typed(Node, T)).
% given node of type T while looking for ref(T), nest in reference
resolve_pointers(typed(Node, T), ref(T), typed(reference(typed(Node, T)), ref(T))).
% given node of type ref(T) while looking for T, nest in dereference
resolve_pointers(typed(Node, ref(T)), T, typed(deference(typed(Node, ref(T))), T)).
