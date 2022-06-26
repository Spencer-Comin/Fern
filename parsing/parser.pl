:- module(parser, [parse/2, parse/3, parse_type_info/2]).
:- use_module(lexer).
:- use_module('../semantics/types').


parse(Tokens, AST) :-
    phrase(program(TypeInfo, AST), Tokens),
    print_term(TypeInfo, [indent_arguments(true)]).

parse(Tokens, TypeInfo, AST) :-
    phrase(program(TypeInfo, AST), Tokens).

parse_type_info(Tokens, TypeInfo) :-
    phrase(type_statements(TypeInfo), Tokens).

program(T, S) --> typeblock(T), statements(S), {!}.
statements([S|T]) --> statement(S), statements(T).
statements(S) --> directive, statements(S).
statements([]) --> [].

directive --> ['@'(_)], [identifier_t("trace", _)], {trace}.

statement(I) --> import_name(I), [';'(_)].
statement(D) --> definition(D).

definition(def(Name, E)) --> [identifier_t(Name, _)], [':='(_)], expression(E), [';'(_)].

expression(F) --> function(F).

function(S) --> struct(S).
function(function(Params, Body)) --> ['('(_)], params(Params), [')'(_)], ['->'(_)], expression(Body).
function(function(Params, Body)) --> params(Params), ['->'(_)], expression(Body).

struct(C) --> conditional(C).
struct(struct([C|T])) --> conditional(C), [','(_)], struct(P), {P = struct(T) ; P \= struct(_), [P] = T}.

conditional(T) --> comparison(T).
conditional(if(If, Then, Else)) --> comparison(If), conditional_centre(Then), conditional(Else).
conditional_centre(E) --> ['?'(_)], expression(E), [':'(_)].

comparison(F) --> term(F).
comparison(binary(Op, Left, Right)) --> term(Left), comparison_tail(Op, Right).
comparison_tail(NextOp, binary(Op, Left, Right)) --> comparison_op(NextOp), term(Left), comparison_tail(Op, Right).
comparison_tail(Op, F) --> comparison_op(Op), term(F).
comparison_op(<) --> ['<'(_)].
comparison_op(>) --> ['>'(_)].

term(F) --> factor(F).
term(binary(Op, Left, Right)) --> factor(Left), term_tail(Op, Right).
term_tail(NextOp, binary(Op, Left, Right)) --> add_op(NextOp), factor(Left), term_tail(Op, Right).
term_tail(Op, F) --> add_op(Op), factor(F).
add_op(+) --> ['+'(_)].
add_op(-) --> ['-'(_)].

factor(F) --> pointer_exp(F).
factor(binary(Op, Left, Right)) --> pointer_exp(Left), factor_tail(Op, Right).
factor_tail(NextOp, binary(Op, Left, Right)) --> mul_op(NextOp), pointer_exp(Left), factor_tail(Op, Right).
factor_tail(Op, P) --> mul_op(Op), pointer_exp(P).
mul_op(*) --> ['*'(_)].
mul_op(/) --> ['/'(_)].

pointer_exp(E) --> function_call(E).
pointer_exp(E) --> ref(E).
pointer_exp(E) --> deref(E).
ref(E) --> ['&'(_)], deref(dereference(E)).
ref(reference(E)) --> ['&'(_)], pointer_exp(E).
deref(E) --> ['^'(_)], ref(reference(E)).
deref(dereference(E)) --> ['^'(_)], pointer_exp(E).

function_call(P) --> primary(P).
function_call(Call) --> primary(FirstFunc), call_tail(FirstFunc, Call).
call_tail(PrevFunc, Call) --> call_arg(A), call_tail(call(PrevFunc, A), Call).
call_tail(Func, call(Func, A)) --> call_arg(A).
call_arg(literal(nil)) --> ['('(_)], [')'(_)].
call_arg(E) --> ['('(_)], expression(E), [')'(_)].

primary(literal(int(Value))) --> [literal_t(int(Value), _)].
primary(literal(fp(Value))) --> [literal_t(fp(Value), _)].
primary(var(Name)) --> var(Name).
primary(E) --> ['('(_)], expression(E), [')'(_)].

var(Name) --> [identifier_t(Name, _)].

% foo := params -> expr;

params([P|T]) --> param(P), [','(_)], params(T).
params([P]) --> param(P).
params([]) --> [].
param(P) --> [identifier_t(P, _)].

import_name(import_chain([Namespace|Imports])) --> [identifier_t(Namespace, _)], ['::'(_)], import_name(import_chain(Imports)).
import_name(import_chain([Name])) --> [identifier_t(Name, _)].

% type stuff
typeblock(typeinfo(S)) --> ['{:'(_)], type_statements(S), [':}'(_)].
typeblock(typeinfo([])) --> [].

type_statements([S|T]) --> type_statement(S), type_statements(T).
type_statements([]) --> [].
type_statement(A) --> type_assignment(A).
type_statement(D) --> type_definition(D).

type_assignment(type_assignment(T, N)) --> ['['(_)], type_expression(T), [']'(_)], names(N).

names([N|T]) --> [identifier_t(N, _)], [';'(_)], names(T).
names([]) --> [].

type_definition(typedef(Name, T)) --> [identifier_t(Name, _)], [':='(_)], type_expression(T), [';'(_)],
                                      {throw(unsupported_error("Typedefs not supported yet"))}.

% type operators (LOWEST TO HIGHEST PRECEDENCE) -> & | , *
% TODO: change array operator to []

type_expression(T) --> morphism(T).

morphism(X => Y) --> typesum(X), ['->'(_)], morphism(Y).
morphism(T) --> typesum(T).

typesum(typesum([P|U])) --> typeproduct(P), ['|'(_)], morphism(T), {T = typesum(U) ; typesum(_) \= T, [T] = U}.
typesum(P) --> typeproduct(P).

typeproduct(*[A|T]) --> reference(A), [','(_)], morphism(P), {P = *T ; P \= *_, [P] = T}.
typeproduct(A) --> reference(A).

reference(&T) --> ['&'(_)], array(T).
reference(T) --> array(T).

array(*L) --> type_primary(A), ['*'(_)], [literal_t(int(N), _)], {repeat(A, N, L)}.
array(T) --> type_primary(T).

type_primary(T) --> [identifier_t(T, _)].
type_primary(annotated(T, Ann)) --> [identifier_t(T, _)], [':'(_)], [identifier_t(Ann, _)].
type_primary(T) --> ['('(_)], type_expression(T), [')'(_)].

% helpers
repeat(_Value, 0, []).
repeat(Value, Count, [Value|T]) :-
    NewCount is Count - 1,
    repeat(Value, NewCount, T).
