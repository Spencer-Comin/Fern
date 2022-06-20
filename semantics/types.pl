:- module(types, [
    op(200, fx, &),     % typeref
    op(700, xfx, <:),   % subtype
    op(200, fy, *),     % product
    op(550, xfy, =>),   % function
    (<:)/2,
    bitwidth/2,
    with_type/3,
    has_type/2
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
subtypes("Int64", "Int"). bitwidth("Int64", 64). bounds("Int64", -9223372036854775808, 9223372036854775807).
subtypes("Int32", "Int64"). bitwidth("Int32", 32). bounds("Int32", -2147483648, 2147483647).
subtypes("Int16", "Int32"). bitwidth("Int16", 16). bounds("Int16", -32768, 32767).
subtypes("Int8",  "Int16"). bitwidth("Int8",  8).  bounds("Int8",  -128, 127).

subtypes("UInt", "Int").
subtypes("UInt64", "UInt"). bitwidth("UInt64", 64). bounds("UInt64", 0, 0xffff_ffff_ffff_ffff).
subtypes("UInt32", "UInt64"). subtypes("UInt32", "Int64"). bitwidth("UInt32", 32). bounds("UInt32", 0, 0xffff_ffff).
subtypes("UInt16", "UInt32"). subtypes("UInt16", "Int32"). bitwidth("UInt16", 16). bounds("UInt16", 0, 0xffff).
subtypes("UInt8",  "UInt16"). subtypes("UInt8",  "Int16"). bitwidth("UInt8",  8).  bounds("UInt8",  0, 0xff).


subtypes("Float", "Number").
subtypes("Float128", "Float"). bitwidth("Float128", 128). bounds("Float128", -10000000000000000, 10000000000000000).
subtypes("Float64",  "Float128"). bitwidth("Float64",  64).  bounds("Float64", -100000000, 100000000).
subtypes("Float32",  "Float64"). bitwidth("Float32",  32).  bounds("Float32", -10000, 10000).
subtypes("Float16",  "Float32"). bitwidth("Float16",  16).  bounds("Float16", -100, 100).

X <: X.
X <: Z :-
    subtypes(X, Y),
    Y <: Z.

type(X) :- X <: Z, X \= Z, type(Z).

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
    S is Max + 8.  % Add one byte to tag the union

% &X
type(&X) :- type(X).
&_ <: "Reference".
&X <: &Y :- X <: Y.
bitwidth(&_, 64). % assuming 64 bit pointers

% type helpers
with_type(E, T, E:T).
has_type(_:T, T).
