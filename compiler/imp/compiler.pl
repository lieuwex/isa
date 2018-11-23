use_module(library(apply)).


% -------------- TOKENISER --------------

tokenise(S, T) :- string_codes(S, C), t_tokens(C, TC), maplist(t_string_codes_token, T, TC).

t_tokens(S, [T | TT]) :- t_whitespace(S, S1), t_token(S1, T, S2), t_tokens(S2, TT).
t_tokens(S, []) :- t_whitespace(S, []).

t_whitespace([C | S], R) :- char_type(C, space), t_whitespace(S, R).
t_whitespace(S, S).

t_token([C | S], word([C | TT]), Rest) :- t_initwordchar(C), t_wordtail(S, TT, Rest).
t_token([C | S], num([C | TT]), Rest) :- char_type(C, digit), t_numtail(S, TT, Rest).
t_token(S, sym(T), Rest) :- t_symtoken(S, T, Rest).
t_wordtail([C | S], [C | TT], Rest) :- t_wordchar(C), t_wordtail(S, TT, Rest).
t_wordtail([C | S], [], [C | S]) :- \+ t_wordchar(C).
t_wordtail([], [], []).
t_numtail([C | S], [C | TT], Rest) :- char_type(C, digit), t_numtail(S, TT, Rest).
t_numtail([C | S], [], [C | S]) :- \+ char_type(C, digit), \+ t_wordchar(C).
t_numtail([], [], []).
t_symtoken([0': | [0'= | S]], [0':, 0'=], S).
t_symtoken([0'> | [0'= | S]], [0'>, 0'=], S).
t_symtoken([0'< | [0'= | S]], [0'<, 0'=], S).
t_symtoken([0'( | S], [0'(], S).
t_symtoken([0') | S], [0')], S).
t_symtoken([0'{ | S], [0'{], S).
t_symtoken([0'} | S], [0'}], S).
t_symtoken([0': | S], [0':], S).
t_symtoken([0'+ | S], [0'+], S).
t_symtoken([0'- | S], [0'-], S).
t_symtoken([0'* | S], [0'*], S).
t_symtoken([0'/ | S], [0'/], S).
t_symtoken([0'< | S], [0'<], S).
t_symtoken([0'> | S], [0'>], S).
t_symtoken([0'; | S], [0';], S).
t_symtoken([0', | S], [0',], S).

t_initwordchar(C) :- char_type(C, alpha).
t_initwordchar(0'_).
t_wordchar(C) :- char_type(C, alnum).
t_wordchar(0'_).

t_string_codes_token(word(S), word(C)) :- string_codes(S, C).
t_string_codes_token(num(S), num(C)) :- string_codes(S, C).
t_string_codes_token(sym(S), sym(C)) :- string_codes(S, C).


% -------------- PARSER --------------

p_program(program([])) --> [].
p_program(program([F | Fs])) --> p_function(F), p_program(program(Fs)).

p_function(func(Name, Args, Ret, Body)) -->
	[word("def")], [word(Name)], [sym("(")], p_args(Args), [sym(")")], [sym(":")], p_type(Ret), p_block(Body).

p_args(args([])) --> [].
p_args(args([D | As])) --> p_decl(D), p_args2(args(As)).
p_args2(args([])) --> [].
p_args2(args([D | As])) --> [sym(",")], p_decl(D), p_args2(args(As)).

p_decl(decl(Name, Type)) --> [word(Name)], [sym(":")], p_type(Type).

p_type(T) --> p_type_atom(A), p_type_suffixes(A, T).
p_type_atom(type(int)) --> [word("int")].
p_type_suffixes(A, A) --> [].
p_type_suffixes(type(A), T) -->
	[sym("[")], [sym("]")], p_type_suffixes(type(array(A)), T).

p_block(Stmts) --> [sym("{")], p_stmts(Stmts), [sym("}")].

p_stmts([]) --> [].
p_stmts([S | Stmts]) --> p_stmt(S), p_stmts(Stmts).

p_stmt(skip) --> [sym(";")].
p_stmt(return(Expr)) --> [word("return")], p_expr(Expr), [sym(";")].
p_stmt(asg(Tgt, Expr)) --> p_expr(Tgt), [sym(":=")], p_expr(Expr), [sym(";")].
p_stmt(while(Expr, Stmts)) --> [word("while")], p_expr(Expr), p_block(Stmts).

oper_table(sym(">"),  1, gt).
oper_table(sym("<"),  1, lt).
oper_table(sym(">="), 1, ge).
oper_table(sym("<="), 1, le).
oper_table(sym("+"),  2, add).
oper_table(sym("-"),  2, sub).
oper_table(sym("*"),  3, mul).
oper_table(sym("/"),  3, div).
oper_table_maxlevel(4).  % one higher than largest level in oper_table
p_expr_atom(num(N)) --> [num(S)], { number_string(N, S) }.
p_expr_atom(ref(V)) --> [word(V)].

p_expr(E) --> p_expr_atom(A), p_expr_level(1, A, E).
p_expr_level(Lv, L, L) --> {oper_table_maxlevel(Lv)}, !.
p_expr_level(Lv, L, E) -->
	{oper_table(Sy, Lv, Tag), Lv1 is Lv + 1},
	[Sy], p_expr_atom(B), p_expr_level(Lv1, B, R), !, p_expr_level(Lv, oper(Tag, L, R), E).
p_expr_level(Lv, L, E) --> {Lv1 is Lv + 1}, p_expr_level(Lv1, L, E).


% -------------- TO IR --------------

to_ir(Prog, IR) :- i_program(Prog, IR, 1, _).

i_program(program([]), ir([]), Ctr, Ctr).
i_program(program([F | Fs]), ir([IF | IFs]), Ctr, Ctr2) :-
	i_func(F, IF, Ctr, Ctr1), i_program(program(Fs), ir(IFs), Ctr1, Ctr2).

i_func(func(Name, Args, Ret, Body), ifunc(Name, Args, Ret, IB), Ctr, Ctr1) :-
	i_stmts(Body, IB, Ctr, Ctr1).

i_stmts([], [], Ctr, Ctr).
i_stmts([S | Ss], [IS | ISs], Ctr, Ctr2) :-
	i_stmt(S, IS, Ctr, Ctr1), i_stmts(Ss, ISs, Ctr1, Ctr2).

i_stmt(asg(ref(Var), Expr), ?, Ctr, ?) :-
	...


% -------------- MAIN --------------

main :-
	open("test.imp", read, Stream),
	read_string(Stream, _, S),
	close(Stream),
	write(S), nl,
	tokenise(S, Tokens), !,
	phrase(p_program(Prog), Tokens), !,
	write(Prog), nl,
	to_ir(Prog, IR), !,
	write(IR), nl.
