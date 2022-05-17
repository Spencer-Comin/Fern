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

statement(I) --> import_name(I), [semicolon_t(_)].
statement(D) --> definition(D).

expression(C) --> struct(C).

struct(struct([C|T])) --> conditional(C), [comma_t(_)], struct(P), {P = struct(T) ; P \= struct(_), [P] = T}.
struct(C) --> conditional(C).

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

factor(binary(Op, Left, Right)) --> function_call(Left), factor_tail(Op, Right).
factor(F) --> function_call(F).
factor_tail(NextOp, binary(Op, Left, Right)) --> mul_op(NextOp), function_call(Left), factor_tail(Op, Right).
factor_tail(Op, P) --> mul_op(Op), function_call(P).
mul_op(*) --> [star_t(_)].
mul_op(/) --> [divide_t(_)].

function_call(Call) --> primary(FirstFunc), call_tail(FirstFunc, Call).
function_call(P) --> primary(P).
call_tail(PrevFunc, Call) --> call_arg(A), call_tail(call(PrevFunc, A), Call).
call_tail(Func, call(Func, A)) --> call_arg(A).
call_arg(literal(nil)) --> [l_paren_t(_)], [r_paren_t(_)].
call_arg(E) --> [l_paren_t(_)], expression(E), [r_paren_t(_)].

primary(literal(int(Value))) --> [literal_t(int(Value), _)].
primary(literal(fp(Value))) --> [literal_t(fp(Value), _)].
primary(var(Name)) --> var(Name).
primary(E) --> [l_paren_t(_)], expression(E), [r_paren_t(_)].

var(Name) --> [identifier_t(Name, _)].

% foo := params -> expr;
definition(def(Name, Obj)) --> [identifier_t(Name, _)], [walrus_t(_)], def_object(Obj), [semicolon_t(_)].
def_object(F) --> function(F).
def_object(E) --> expression(E).

function(function(Params, Body)) --> [l_paren_t(_)], params(Params), [r_paren_t(_)], [arrow_t(_)], expression(Body).
function(function(Params, Body)) --> params(Params), [arrow_t(_)], expression(Body).

params([P|T]) --> param(P), [comma_t(_)], params(T).
params([P]) --> param(P).
params([]) --> [].
param(P) --> [identifier_t(P, _)].

import_name(import_chain([Namespace|Imports])) --> [identifier_t(Namespace, _)], [namespace_t(_)], import_name(import_chain(Imports)).
import_name(import_chain([Name])) --> [identifier_t(Name, _)].

% type stuff
typeblock(typeinfo(S)) --> [l_type_t(_)], type_statements(S), [r_type_t(_)].
typeblock(typeinfo([])) --> [].

type_statements([S|T]) --> type_statement(S), type_statements(T).
type_statements([]) --> [].
type_statement(A) --> type_assignment(A).
type_statement(D) --> type_definition(D).

type_assignment(type_assignment(T, N)) --> [l_square_t(_)], type_expression(T), [r_square_t(_)], names(N).

names([N|T]) --> [identifier_t(N, _)], [semicolon_t(_)], names(T).
names([]) --> [].

type_definition(typedef(Name, T)) --> [identifier_t(Name, _)], [walrus_t(_)], type_expression(T), [semicolon_t(_)],
                                      {throw(unsupported_error("Typedefs not supported yet"))}.

% type operators (LOWEST TO HIGHEST PRECEDENCE) -> & | , *
% TODO: change array operator to []

type_expression(T) --> morphism(T).

morphism(morphism(X, Y)) --> typesum(X), [arrow_t(_)], morphism(Y).
morphism(T) --> typesum(T).

typesum(typesum([P|U])) --> typeproduct(P), [bar_t(_)], morphism(T), {T = typesum(U) ; typesum(_) \= T, [T] = U}.
typesum(P) --> typeproduct(P).

typeproduct(typeproduct([A|T])) --> reference(A), [comma_t(_)], morphism(P), {P = typeproduct(T) ; P \= typeproduct(_), [P] = T}.
typeproduct(A) --> reference(A).

reference(typeref(T)) --> [and_t(_)], array(T).
reference(T) --> array(T).

array(typeproduct(L)) --> type_primary(A), [star_t(_)], [literal_t(int(N), _)], {repeat(A, N, L)}.
array(T) --> type_primary(T).

type_primary(T) --> [identifier_t(T, _)].
type_primary(annotated(T, Ann)) --> [identifier_t(T, _)], [colon_t(_)], [identifier_t(Ann, _)].
type_primary(T) --> [l_paren_t(_)], type_expression(T), [r_paren_t(_)].

% helpers
repeat(_Value, 0, []).
repeat(Value, Count, [Value|T]) :-
    NewCount is Count - 1,
    repeat(Value, NewCount, T).
