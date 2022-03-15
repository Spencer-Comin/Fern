:- module(parser, [parse/2, parse/3, parse_type_info/2]).
:- use_module(lexer).


parse(Tokens, AST) :-
    phrase(program(TypeInfo, AST), Tokens),
    print_term(TypeInfo, [indent_arguments(true)]).

parse(Tokens, TypeInfo, AST) :-
    phrase(program(TypeInfo, AST), Tokens).

parse_type_info(Tokens, TypeInfo) :-
    phrase(type_statements(TypeInfo), Tokens).

program(T, S) --> typeblock(T), statements(S), {!}.
statements([S|T]) --> statement(S), statements(T).
statements([]) --> [].

statement(E) --> expression(E), [semicolon_t(_)].
statement(D) --> definition(D).
statement(I) --> import_name(I), [semicolon_t(_)].

% TODO: add concatenation operator to build product objects
expression(C) --> conditional(C).

conditional(if(If, Then, Else)) --> comparison(If), conditional_centre(Then), conditional(Else).
conditional(T) --> comparison(T).
conditional_centre(E) --> [question_t(_)], expression(E), [colon_t(_)].

comparison(binary(Op, Left, Right)) --> term(Left), comparison_tail(Op, Right).
comparison(F) --> term(F).
comparison_tail(NextOp, binary(Op, Left, Right)) --> comparison_op(NextOp), term(Left), comparison_tail(Op, Right).
comparison_tail(Op, F) --> comparison_op(Op), term(F).
comparison_op(<) --> [lt_t(_)].
comparison_op(>) --> [gt_t(_)].

term(binary(Op, Left, Right)) --> factor(Left), term_tail(Op, Right).
term(F) --> factor(F).
term_tail(NextOp, binary(Op, Left, Right)) --> add_op(NextOp), factor(Left), term_tail(Op, Right).
term_tail(Op, F) --> add_op(Op), factor(F).
add_op(+) --> [plus_t(_)].
add_op(-) --> [minus_t(_)].

factor(binary(Op, Left, Right)) --> primary(Left), factor_tail(Op, Right).
factor(F) --> primary(F).
factor_tail(NextOp, binary(Op, Left, Right)) --> mul_op(NextOp), primary(Left), factor_tail(Op, Right).
factor_tail(Op, P) --> mul_op(Op), primary(P).
mul_op(*) --> [star_t(_)].
mul_op(/) --> [divide_t(_)].

primary(literal(number(Value))) --> [literal_t(number(Value), _)].
primary(var(Name)) --> var(Name).
primary(call(Name, Args)) --> function_call(Name, Args).
primary(E) --> [l_paren_t(_)], expression(E), [r_paren_t(_)].

var(Name) --> [identifier_t(Name, _)].

% foo := (params) -> expr;
definition(def(Name, Obj)) --> [identifier_t(Name, _)], [walrus_t(_)], def_object(Obj), [semicolon_t(_)].
def_object(F) --> function(F).
def_object(E) --> expression(E).

function(function(Params, Body)) --> [l_paren_t(_)], params(Params), [r_paren_t(_)], [arrow_t(_)], expression(Body).

params([P|T]) --> param(P), [comma_t(_)], params(T).
params([P]) --> param(P).
params([]) --> [].
param(P) --> [identifier_t(P, _)].

function_call(Name, Args) --> [identifier_t(Name, _)], [l_paren_t(_)], f_args(Args), [r_paren_t(_)].

f_args([A|T]) --> f_arg(A), [comma_t(_)], f_args(T).
f_args([A]) --> f_arg(A).
f_args([]) --> [].
f_arg(E) --> expression(E).

import_name(import_chain([Namespace|Imports])) --> [identifier_t(Namespace, _)], [namespace_t(_)], import_name(import_chain(Imports)).
import_name(import_chain([Name])) --> [identifier_t(Name, _)].

% type stuff
% typeblock(typeinfo(definitions(D), assignments(A))) --> [l_type_t(_)], type_definitions(D), type_assignments(A), [r_type_t(_)].
% typeblock(typeinfo(definitions([]), assignments([]))) --> [].
typeblock(typeinfo(S)) --> [l_type_t(_)], type_statements(S), [r_type_t(_)].
typeblock(typeinfo([])) --> [].

type_statements([S|T]) --> type_statement(S), type_statements(T).
type_statements([]) --> [].
type_statement(A) --> type_assignment(A).
type_statement(D) --> type_definition(D).

% type_assignments([H|T]) --> type_assignment(H), type_assignments(T).
% type_assignments([]) --> [].

type_assignment(type_assignment(T, N)) --> [l_square_t(_)], type_expression(T), [r_square_t(_)], names(N).

names([N|T]) --> [identifier_t(N, _)], [semicolon_t(_)], names(T).
names([]) --> [].

% type_definitions([H|T]) --> type_definition(H), type_definitions(T).
% type_definitions([]) --> [].

type_definition(typedef(Name, T)) --> [identifier_t(Name, _)], [walrus_t(_)], type_expression(T), [semicolon_t(_)].
type_definition(typedef(F, T)) --> type_functor(F), [walrus_t(_)], type_expression(T), [semicolon_t(_)].

% type operators (LOWEST TO HIGHEST PRECEDENCE) -> & | , *
% TODO: change reference precedence to be higher than product
% TODO: change array operator to []

type_expression(T) --> morphism(T).

morphism(morphism(X, Y)) --> typesum(X), [arrow_t(_)], morphism(Y).
morphism(T) --> typesum(T).

typesum(typesum([P|U])) --> product(P), [bar_t(_)], reference(T), {T = typesum(U) ; dif(typesum(_), T), [T] = U}.
typesum(P) --> product(P).

product(typeproduct([A|T])) --> reference(A), [comma_t(_)], reference(P), {P = typeproduct(T) ; dif(P, typeproduct(_)), [P] = T}.
product(A) --> reference(A).

reference(typeref(T)) --> [and_t(_)], array(T).
reference(T) --> array(T).

array(typeproduct(L)) --> type_primary(A), [star_t(_)], [literal_t(number(N), _)], {repeat(A, N, L)}.
array(T) --> type_primary(T).

type_primary(T) --> [identifier_t(T, _)].
type_primary(annotated(T, Ann)) --> [identifier_t(T, _)], [colon_t(_)], [identifier_t(Ann, _)].
type_primary(T) --> [l_paren_t(_)], type_expression(T), [r_paren_t(_)].
type_primary(T) --> type_functor(T).

% type_functor(functor(Name, Args)) --> [identifier_t(Name, _)], [l_paren_t(_)], type_expression(Args), [r_paren_t(_)].
type_functor(functor(Name, Arg)) --> [identifier_t(Name, _)], [l_curly_t(_)], [identifier_t(Arg, _)], [r_curly_t(_)].

% helpers
repeat(_Value, 0, []).
repeat(Value, Count, [Value|T]) :-
    NewCount is Count - 1,
    repeat(Value, NewCount, T).
