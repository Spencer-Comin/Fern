:- module(types, [
    op(200, fx, &),     % typeref
    op(700, xfx, <:),   % subtype
    op(200, fy, *),     % product
    op(550, xfy, =>),   % function
    (<:)/2,
    binary_type/4,
    bounds/3,
    bitwidth/2,
    promotable/2,
    biggest/3
    ]).
:- discontiguous (<:)/2.
:- discontiguous subtypes/2.
:- discontiguous bitwidth/2.
:- discontiguous bounds/3.
:- discontiguous type/1.

%% BUILT IN TYPES

% base types
type("Number").
type("End").  % terminal category (maybe rename?)
type("IO").  % currently equivalent to &End (void*)
type("Bool").
type("Reference").

% derived types
subtypes("Int", "Number").
subtypes("Int8",  "Int"). bitwidth("Int8",  8).  bounds("Int8",  -128, 127).
subtypes("Int16", "Int"). bitwidth("Int16", 16). bounds("Int16", -32768, 32767).
subtypes("Int32", "Int"). bitwidth("Int32", 32). bounds("Int32", -2147483648, 2147483647).
subtypes("Int64", "Int"). bitwidth("Int64", 64). bounds("Int64", -9223372036854775808, 9223372036854775807).

subtypes("UInt", "Int").
subtypes("UInt8",  "UInt"). bitwidth("UInt8",  8).  bounds("UInt8",  0, 0xff).
subtypes("UInt16", "UInt"). bitwidth("UInt16", 16). bounds("UInt16", 0, 0xffff).
subtypes("UInt32", "UInt"). bitwidth("UInt32", 32). bounds("UInt32", 0, 0xffff_ffff).
subtypes("UInt64", "UInt"). bitwidth("UInt64", 64). bounds("UInt64", 0, 0xffff_ffff_ffff_ffff).

subtypes("Float", "Number").
subtypes("Float16",  "Float"). bitwidth("Float16",  16).  bounds("Float16", -100, 100).
subtypes("Float32",  "Float"). bitwidth("Float32",  32).  bounds("Float32", -10000, 10000).
subtypes("Float64",  "Float"). bitwidth("Float64",  64).  bounds("Float64", -100000000, 100000000).
subtypes("Float128", "Float"). bitwidth("Float128", 128). bounds("Float128", -10000000000000000, 10000000000000000).

X <: X.
X <: Z :-
    subtypes(X, Y),
    Y <: Z.

% promotable

biggest(X, Y, Bigger) :-
    bounds(X, Xmin, Xmax),
    bounds(Y, Ymin, Ymax),
    (Xmin >= Ymin, Xmax =< Ymax ->
        Bigger = Y ;
        Bigger = X).

promotable(X, Y) :-
    bounds(X, Xmin, Xmax),
    bounds(Y, Ymin, Ymax),
    Xmin >= Ymin,
    Xmax =< Ymax.

%% COMPOSITE TYPE RULES

% X -> Y
type(X => Y) :- type(X), type(Y).
bitwidth(_ => _, 64). % assuming 64 bit function pointers

% X Y Z ...
type(*Xs) :- maplist(type, Xs).
bitwidth(*Xs, S) :-
    maplist(bitwidth, Xs, bitwidths),
    sum_list(bitwidths, S).

% X | Y | Z | ...
% Not implemented yet!
type(typesum(Xs)) :- maplist(type, Xs).
bitwidth(typesum(Xs), S) :-
    maplist(bitwidth, Xs, bitwidths),
    max_list(bitwidths, Max),
    S is Max + 1.  % Add one byte to tag the union

% &X
type(&X) :- type(X).
&_ <: "Reference".
&X <: &Y :- X <: Y.
bitwidth(&_, 64). % assuming 64 bit pointers

% binary_type(Op, LeftType, RightType, BinaryType)
binary_type(Op, LeftType, RightType, Type) :-
    arithmetic_op(Op),
    (
        LeftType <: "Int", RightType <: "Int"
    ;
        LeftType <: "Float", RightType <: "Float"
    ),
    (promotable(RightType, LeftType) ->
        Type = LeftType ;
        Type = RightType).

binary_type(Op, LeftType, RightType, "Bool") :-
    comparison_op(Op),
    LeftType <: "Number",
    RightType <: "Number".

arithmetic_op(+).
arithmetic_op(-).
arithmetic_op(*).
arithmetic_op(/).

comparison_op(<).
comparison_op(>).
