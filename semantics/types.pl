:- module(types, [decompose/3, binary_type/4, bitwidth/2, subtype/2]).
:- discontiguous subtype/2.
:- discontiguous bitwidth/2.
:- discontiguous type/1.
:- discontiguous equivalent/2.
:- table equivalent/2.
:- table binary_type/2.

%% BUILT IN TYPES

% base types
type("Number").
type("End").  % terminal category (maybe rename?)
type("IO").  % currently equivalent to &End (void*)
type("Bool").
type("Reference").

% derived types
subtype("Int", "Number").
subtype("Int8",  "Int"). bitwidth("Int8",  8).
subtype("Int16", "Int"). bitwidth("Int16", 16).
subtype("Int32", "Int"). bitwidth("Int32", 32).
subtype("Int64", "Int"). bitwidth("Int64", 64).

subtype("UInt", "Int").
subtype("UInt8",  "UInt"). bitwidth("UInt8",  8).
subtype("UInt16", "UInt"). bitwidth("UInt16", 16).
subtype("UInt32", "UInt"). bitwidth("UInt32", 32).
subtype("UInt64", "UInt"). bitwidth("UInt64", 64).

subtype("Float", "Number").
subtype("Float16",  "Float"). bitwidth("Float16",  16).
subtype("Float32",  "Float"). bitwidth("Float32",  32).
subtype("Float64",  "Float"). bitwidth("Float64",  64).
subtype("Float128", "Float"). bitwidth("Float128", 128).

equivalent("Byte", "UInt8").


%% COMPOSITE TYPE RULES

% equivalencies
equivalent(X, X). % identity
equivalent(X, Z) :- equivalent(X,  Y), equivalent(Y, Z). % transitivity
equivalent(X, Y) :- equivalent(Y, X), !. % commutativity
type(X) :- equivalent(X, Y), type(Y).
% bitwidth(T, S) :- bitwidth(Q, S), equivalent(T, Q).

% isomorphicitiy
isomorphic(morphism("Unit", Y), Y) :- !. % global element
isomorphic(morphism(X, Y), morphism(FX, FY)) :- isomorphic(X, FX), isomorphic(Y, FY).
isomorphic(X, Y) :- equivalent(X, Y). % equivalency is a subset of isomorphicity
isomorphic(X, Y) :- bitwidth(X, S), bitwidth(Y, S). % bytewise convertible
isomorphic(X, Y) :- isomorphic(Y, X), !. % commutativity

% definite types
definite(T) :- \+ indefinite(T).

indefinite(T) :- subtype(_, T).
indefinite(morphism(X, Y)) :- indefinite(X) ; indefinite(Y).
indefinite(typeproduct(X, Y)) :- indefinite(X) ; indefinite(Y).
indefinite(typesum(X, Y)) :- indefinite(X) ; indefinite(Y).
indefinite(ref(X)) :- indefinite(X).

% X -> Y
type(morphism(X, Y)) :- type(X), type(Y).
bitwidth(morphism(_, _), 8). % assuming 8B function pointers

% X Y Z ...
type(typeproduct(Xs)) :- maplist(type, Xs).
bitwidth(typroduct(Xs), S) :-
    maplist(bitwidth, Xs, bitwidths),
    sum_list(bitwidths, S).
equivalent(typeproduct([X]), X).

% X | Y | Z | ...
type(typesum(Xs)) :- maplist(type, Xs).
bitwidth(typesum(Xs), S) :-
    maplist(bitwidth, Xs, bitwidths),
    max_list(bitwidths, Max),
    S is Max + 1.  % Add one byte to tag the union
equivalent(typesum([X]), X).

% ,X
type(ref(X)) :- type(X).
subtype(ref(_), "Reference").
subtype(ref(X), ref(Y)) :- subtype(X, Y).
bitwidth(ref(_), 8). % assuming 8B pointers
% offset pointers
with_offset(ref(X), ref(Y, N)) :- resolve_offset(X, N, Y).
resolve_offset(_, Off, _) :- Off < 0, fail.
resolve_offset(X, 0, X).
resolve_offset(typeproduct(Xs), Off, typeproduct(Ys)) :-
    append(Prefix, Ys, Xs),
    length(Prefix, Off).

% X: label
equivalent(annotated(X, _), X).

% cast checking
fits_in(X, Y) :- bitwidth(X, SX), bitwidth(Y, SY), SX =< SY.


%% TYPE INFERENCE
decompose(T, 1, [T]).
decompose("End", 0, []).
decompose(typeproduct(Xs), N, Xs) :- length(Xs, N).

% supports(Type, Operation)
supports("Number", +).
supports("Number", -).
supports("Number", *).
supports("Number", /).

% binary_type(Op, LeftType, RightType, BinaryType)
binary_type(Op, LeftType, RightType, Type) :-
    arithmetic_op(Op),
    (
        subtype(LeftType, "Int"), subtype(RightType, "Int")
    ;
        subtype(LeftType, "Float"), subtype(RightType, "Float")
    ),
    bitwidth(LeftType, Leftbitwidth),
    bitwidth(RightType, Rightbitwidth),
    (Leftbitwidth >= Rightbitwidth ->
        Type = LeftType ;
        Type = RightType).

binary_type(Op, LeftType, RightType, "Bool") :-
    comparison_op(Op),
    subtype(LeftType, "Number"),
    subtype(RightType, "Number").

arithmetic_op(+).
arithmetic_op(-).
arithmetic_op(*).
arithmetic_op(/).

comparison_op(<).
comparison_op(>).