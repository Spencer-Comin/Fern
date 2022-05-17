:- module(lexer, [tokenize/2]).

tokenize(In, Out) :- tokenize(In, Out, 1).

tokenize([], [], _LineNo).

% increment lineno on newline
tokenize([In|T], Out, LineNo) :-
    code_type(In, newline),
    NextLineNo is LineNo + 1, !,
    tokenize(T, Out, NextLineNo).

% ignore comments
tokenize([0'/, 0'/|T], Out, LineNo) :-
    consume_until(T, 0'\n, Remain, _),
    NextLineNo is LineNo + 1, !,
    tokenize(Remain, Out, NextLineNo).

% ignore whitespace
tokenize([In|T], Out, LineNo) :-
    code_type(In, space),
    tokenize(T, Out, LineNo).

% numbers
% TODO: add support for hex, binary, etc
% TODO: add support for type (float, int)
tokenize([In|T_i], [Out|T_o], LineNo) :-
    code_type(In, digit),
    consume_type([In|T_i], digit, [0'.|Next], WholeDigits),
    consume_type(Next, digit, Remain, FractionDigits),
    append(WholeDigits, [0'.|FractionDigits], DigitList),
    number_codes(Value, DigitList),
    Out = literal_t(fp(Value), LineNo),
    tokenize(Remain, T_o, LineNo).

tokenize([In|T_i], [Out|T_o], LineNo) :-
    code_type(In, digit),
    consume_type([In|T_i], digit, Remain, DigitList),
    number_codes(Value, DigitList),
    Out = literal_t(int(Value), LineNo),
    tokenize(Remain, T_o, LineNo).

% strings
tokenize([0'"|T_i], [Out|T_o], LineNo) :-
    consume_until(T_i, 0'", Remain, Codes),
    string_codes(Value, Codes),
    Out = literal_t(string(Value), LineNo),
    tokenize(Remain, T_o, LineNo).

tokenize([0''|T_i], [Out|T_o], LineNo) :-
    consume_until(T_i, 0'', Remain, Codes),
    string_codes(Value, Codes),
    Out = literal_t(Value, LineNo),
    tokenize(Remain, T_o, LineNo).

% identifiers
tokenize([In|T_i], [Out|T_o], LineNo) :-
    code_type(In, csymf),
    consume_type([In|T_i], csym, Remain, CharList),
    string_codes(Name, CharList),
    Out = identifier_t(Name, LineNo),
    tokenize(Remain, T_o, LineNo).

% symbols
tokenize([0'{, 0':|T_i], [l_type_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0':, 0'}|T_i], [r_type_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'{|T_i], [l_curly_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'}|T_i], [r_curly_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'(|T_i], [l_paren_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0')|T_i], [r_paren_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'[|T_i], [l_square_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0']|T_i], [r_square_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0';|T_i], [semicolon_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'?|T_i], [question_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0':, 0'=|T_i], [walrus_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0':, 0':|T_i], [namespace_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0':|T_i], [colon_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'=, 0'=|T_i], [equal_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'=|T_i], [assign_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'|, 0'||T_i], [logical_or_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'|, 0'=|T_i], [or_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'||T_i], [bar_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'&, 0'&|T_i], [logical_and_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'&, 0'=|T_i], [and_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'&|T_i], [and_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'+, 0'+|T_i], [increment_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'+, 0'=|T_i], [plus_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'+|T_i], [plus_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'-, 0'-|T_i], [decrement_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'-, 0'>|T_i], [arrow_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'-, 0'=|T_i], [minus_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'-|T_i], [minus_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'!, 0'=|T_i], [neq_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'!|T_i], [bang_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'~|T_i], [tilde_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'@|T_i], [at_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0', , 0'=|T_i], [comma_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0',|T_i], [comma_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'^, 0'=|T_i], [xor_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'^|T_i], [xor_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'*, 0'=|T_i], [multiply_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'*|T_i], [star_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'%, 0'=|T_i], [modulo_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'%|T_i], [modulo_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'<, 0'=|T_i], [leq_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'≤|T_i], [leq_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'<|T_i], [lt_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'>, 0'=|T_i], [geq_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'≥|T_i], [geq_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'>|T_i], [gt_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'/, 0'=|T_i], [divide_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'/|T_i], [divide_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'÷, 0'=|T_i], [divide_assign_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'÷|T_i], [divide_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'\\|T_i], [backslash_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

tokenize([0'., 0'., 0'.|T_i], [ellipsis_t(LineNo)|T_o], LineNo) :- !, tokenize(T_i, T_o, LineNo).
tokenize([0'…|T_i], [ellipsis_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).
tokenize([0'.|T_i], [dot_t(LineNo)|T_o], LineNo) :- tokenize(T_i, T_o, LineNo).

% utilities
consume_type([], _, [], []).
consume_type([Char|In], Type, Remain, [Char|Out]) :-
    code_type(Char, Type),
    consume_type(In, Type, Remain, Out).
consume_type([Char|In], Type, [Char|In], []) :-
    \+ code_type(Char, Type).

consume_until([], _, [], []).
consume_until([TargetChar|In], TargetChar, In, []).
consume_until([Char|In], TargetChar, Remain, [Char|Out]) :-
    consume_until(In, TargetChar, Remain, Out).
