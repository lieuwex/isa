use_module(library(apply)).


% -------------- STRING MAP --------------

sm_make(sm_map{}).

sm_put(Map, Key, Value, Map2) :-
	is_dict(Map, sm_map),
	string_codes(Key, Codes), sm_put2(Map, Codes, Value, Map2).

sm_put2(Map, [], Value, Map.put(-1, Value)) :- !.
sm_put2(Map, [C | Key], Value, Map2) :-
	Submap = Map.get(C)
	-> sm_put2(Submap, Key, Value, Submap2),
	   Map2 = Map.put(C, Submap2)
	;  sm_put2(_{}, Key, Value, Submap2),
	   Map2 = Map.put(C, Submap2).

sm_putx(Map, Key, Value, Map2) :-
	is_dict(Map, sm_map),
	string_codes(Key, Codes), sm_putx2(Map, Codes, Value, Map2).

sm_putx2(Map, [], Value, Map2) :-
	!, (_ = Map.get(-1) -> fail; Map2 = Map.put(-1, Value)).
sm_putx2(Map, [C | Key], Value, Map2) :-
	Submap = Map.get(C)
	-> sm_putx2(Submap, Key, Value, Submap2),
	   Map2 = Map.put(C, Submap2)
	;  sm_putx2(_{}, Key, Value, Submap2),
	   Map2 = Map.put(C, Submap2).

sm_get(Map, Key, Value) :-
	is_dict(Map, sm_map),
	string_codes(Key, Codes), sm_get2(Map, Codes, Value).

sm_get2(Map, [], Map.get(-1)) :- !.
sm_get2(Map, [C | Key], Value) :- sm_get2(Map.get(C), Key, Value).



% -------------- TOKENISER --------------

tokenise(S, T) :- string_codes(S, C), t_tokens(C, TC), maplist(t_string_codes_token, T, TC).

t_tokens(S, [T | TT]) :- t_whitespace(S, S1), t_token(S1, T, S2), t_tokens(S2, TT).
t_tokens(S, []) :- t_whitespace(S, []).

t_whitespace([C | S], R) :- char_type(C, space), t_whitespace(S, R).
t_whitespace(S, R) :- t_comment(S, S2), t_whitespace(S2, R).
t_whitespace(S, S).

t_comment([0'# | S], R) :- t_tillnewline(S, [10 | R]).
t_tillnewline([10 | S], [10 | S]) :- !.
t_tillnewline([_ | S], R) :- t_tillnewline(S, R).

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
p_stmt(if(Expr, Then, Else)) -->
	[word("if")], p_expr(Expr), p_block(Then), [word("else")], p_block(Else).
p_stmt(if(Expr, Then, [])) --> [word("if")], p_expr(Expr), p_block(Then).
p_stmt(decl(Name, Type)) --> [word(Name)], [sym(":")], p_type(Type), [sym(";")].

oper_table(sym(">"),  1, gt).
oper_table(sym("<"),  1, lt).
oper_table(sym(">="), 1, gte).
oper_table(sym("<="), 1, lte).
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

to_ir(Prog, IR) :- sm_make(Vars), i_program(Prog, IR, idict{ctr:1, vars:Vars}, _).

i_program(program([]), ir([]), Dict, Dict).
i_program(program([F | Fs]), ir([IF | IFs]), Dict, Dict2) :-
	i_func(F, IF, Dict, Dict1), i_program(program(Fs), ir(IFs), Dict1, Dict2).

i_func(func(Name, Args, Ret, Body), ifunc(Name, Args, Ret, IB), Dict, Dict1) :-
	i_stmts(Body, IB, Dict, Dict1).

i_stmts([], [], Dict, Dict).
i_stmts([S | Ss], ISs, Dict, Dict2) :-
	i_stmt(S, ISs1, Dict, Dict1), i_stmts(Ss, ISs2, Dict1, Dict2),
	append(ISs1, ISs2, ISs).

i_freshreg(Dict, Dict.put(ctr, Ctr1), reg(Dict.ctr)) :- Ctr1 is Dict.ctr + 1.
i_newlabel(Dict, Dict.put(ctr, Ctr1), label(Label)) :-
	N = Dict.ctr, Ctr1 is N + 1,
	number_string(N, S), string_concat("L", S, Label).

i_stmt(decl(Name, type(int)), [nop], Dict, Dict3) :-
	i_freshreg(Dict, Dict2, Loc),
	sm_putx(Dict2.vars, Name, Loc, Vars),
	Dict3 = Dict2.put(vars, Vars).
i_stmt(asg(ref(Var), Expr), Inss, Dict, Dict2) :-
	(sm_get(Dict.vars, Var, Loc), ! ;
		string_concat("Undeclared variable ", Var, Msg),
		write(Msg), nl, fail),
	i_expr(Expr, Inss1, Reg, Dict, Dict2),
	append(Inss1, [mov(Loc, Reg)], Inss).
i_stmt(return(Expr), Inss, Dict, Dict2) :-
	i_expr(Expr, Inss1, Reg, Dict, Dict2),
	append(Inss1, [ret(Reg)], Inss).
i_stmt(if(Cond, Then, Else), Inss, Dict, Dict6) :-
	i_expr(Cond, Inss1, Reg, Dict, Dict2),
	i_newlabel(Dict2, Dict3, ElseLab),
	i_newlabel(Dict3, Dict4, AfterLab),
	i_stmts(Then, Inss2, Dict4, Dict5),
	i_stmts(Else, Inss3, Dict5, Dict6),
	append([Inss1, [jcc(z, Reg, ElseLab)], Inss2,
				[jmp(AfterLab), ElseLab], Inss3, [AfterLab]],
			Inss).

i_expr(oper(Rator, E1, E2), Inss, Res, Dict, Dict4) :-
	i_expr(E1, Inss1, R1, Dict, Dict2),
	i_expr(E2, Inss2, R2, Dict2, Dict3),
	i_freshreg(Dict3, Dict4, Res),
	append([Inss1, Inss2, [arith(Rator, Res, R1, R2)]], Inss).
i_expr(num(N), [li(Res, num(N))], Res, Dict, Dict2) :-
	i_freshreg(Dict, Dict2, Res).
i_expr(ref(R), [mov(Res, Loc)], Res, Dict, Dict2) :-
	i_freshreg(Dict, Dict2, Res),
	sm_get(Dict2.vars, R, Loc).


% -------------- TO ASM --------------

to_asm(IR, Asm) :- a_program(IR, Asm).

a_program(ir(Fs), Asm) :- maplist(a_func, Fs, Asms), append(Asms, Asm).

a_func(ifunc(Name, _Args, _Ret, Inss), Asm) :-
	regalloc(Inss, Inss1),
	maplist(a_instr, Inss1, Inss2),
	append([label(Name)], Inss2, Asm).

a_instr(nop, mv(reg(1), reg(1))).
a_instr(li(Reg, num(Num)), li(Reg, num(Num))) :- !, between(-1 << 44, (1 << 44) - 1, Num).
a_instr(li(_, _), _) :- write("Integer in li out of range"), nl, fail.
a_instr(mov(reg(Rd), reg(R1)), mv(reg(Rd), reg(R1))).
a_instr(arith(add, reg(Rd), reg(R1), reg(R2)), add(reg(Rd), reg(R1), reg(R2))).
a_instr(arith(sub, reg(Rd), reg(R1), reg(R2)), sub(reg(Rd), reg(R1), reg(R2))).
a_instr(arith(mul, reg(Rd), reg(R1), reg(R2)), mul(reg(Rd), reg(R1), reg(R2))).
a_instr(arith(div, reg(Rd), reg(R1), reg(R2)), div(reg(Rd), reg(R1), reg(R2))).
a_instr(arith(lt, reg(Rd), reg(R1), reg(R2)), lt(reg(Rd), reg(R1), reg(R2))).
a_instr(arith(lte, reg(Rd), reg(R1), reg(R2)), lte(reg(Rd), reg(R1), reg(R2))).


% -------------- REGALLOC --------------

regalloc(Inss, Res) :-
	r_collect(Inss, Regs).

r_collect(Inss, Regs) :-
	maplist(r_collect_instr, Inss, Reglists),
	append(Reglists, Regs1),
	sort(Regs1, Regs2),
	uniq(Regs2, Regs).

r_collect_instr(nop, []).
r_collect_instr(li(reg(Rd), _), [Rd]).
r_collect_instr(mov(reg(Rd), reg(R1), reg(R2)), [Rd, R1, R2]).
r_collect_instr(arith(_, reg(Rd), reg(R1), reg(R2)), [Rd, R1, R2]).
r_collect_instr(jcc(_, reg(R1), _), [R1]).
r_collect_instr(jmp(_), []).
r_collect_instr(ret(reg(R1)), [R1]).


% -------------- PRETTY --------------

pretty(ir(IRFs)) :- maplist(pretty, IRFs).
pretty(ifunc(Name, Args, Ret, Inss)) :-
	write("ifunc "), write(Name),
	write("("), pretty(Args), write(") : "),
	pretty(Ret), write(" {"), nl,
	maplist(pretty_irins, Inss),
	write("}"), nl.
pretty(type(int)) :- write("int").
pretty(num(N)) :- write(N).
pretty(reg(R)) :- write("r"), write(R).
pretty(args([])).
pretty(args([A])) :- pretty(A), !.
pretty(args([A | As])) :- pretty(A), write(", "), pretty(args(As)).
pretty(decl(Name, Type)) :- write(Name), write(" : "), pretty(Type).

pretty_irins(nop) :- write("\tnop"), nl.
pretty_irins(mov(Rd, R1)) :- write("\tmov "), pretty(Rd), write(", "), pretty(R1), nl.
pretty_irins(li(Rd, R1)) :- write("\tli "), pretty(Rd), write(", "), pretty(R1), nl.
pretty_irins(arith(Rator, Rd, R1, R2)) :-
	write("\t"), pretty_ir_rator(Rator), write(" "),
	pretty(Rd), write(", "), pretty(R1), write(", "), pretty(R2), nl.
pretty_irins(ret(R1)) :- write("\tret "), pretty(R1), nl.
pretty_irins(jcc(Cc, R1, label(Dest))) :-
	write("\tj"), write(Cc), write(" "), pretty(R1), write(", "), write(Dest), nl.
pretty_irins(jmp(label(Dest))) :- write("\tjmp "), write(Dest), nl.
pretty_irins(label(Label)) :- write(Label), write(":"), nl.

pretty_ir_rator(add) :- write("add").
pretty_ir_rator(sub) :- write("sub").
pretty_ir_rator(mul) :- write("mul").
pretty_ir_rator(div) :- write("div").
pretty_ir_rator(lt) :- write("lt").
pretty_ir_rator(gt) :- write("gt").
pretty_ir_rator(lte) :- write("lte").
pretty_ir_rator(gte) :- write("gte").


% -------------- UTILITIES --------------

uniq([], []).
uniq([X], [X]).
uniq([X | [Y | L]], [X | R]) :- X = Y, !, uniq(L, R).
uniq([X | L], [X | R]) :- uniq(L, R).


% -------------- MAIN --------------

main :-
	open("test.imp", read, Stream),
	read_string(Stream, _, S),
	close(Stream),
	write(S), nl,
	tokenise(S, Tokens), !,
	write(Tokens), nl,
	phrase(p_program(Prog), Tokens), !,
	write(Prog), nl,
	to_ir(Prog, IR), !,
	write(IR), nl,
	pretty(IR),
	to_asm(IR, Asm), !,
	write(Asm), nl.
