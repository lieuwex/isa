% Environment: [(Varname, Type)]
env_var_type([(V, T) | _], V, T).
env_var_type([_ | L], V, T) :- env_var_type(L, V, T).

typecheck(stmt_decl(decl(Name, Type), E), Env, stmt_decl(R), [(Name, Type) | Env]) :-
	\+ env_var_type(Env, Name, _),
	typeof(E, Env, T1, R1),
	(T1 = Type -> R = R1 ; can_coerce(T1, Type), R = coerce(Type, R1)).
typecheck(stmt_assign(Tgt, E), Env, stmt_assign(R), Env) :-
	env_var_type(Env, Tgt, T), typeof(E, Env, T1, R1),
	(T1 = T -> R = R1; can_coerce(T1, T), R = coerce(T, R1)).
typecheck(stmt_do([]), Env, stmt_do([]), Env).
typecheck(stmt_do([Stmt | Stmts]), Env, stmt_do([R | Rs]), Env2) :-
	typecheck(Stmt, Env, R, Env1),
	typecheck(stmt_do(Stmts), Env1, stmt_do(Rs), Env2).

typeof(expr_num(_), _, type_int, type_int).
typeof(expr_num(N), _, type_int_sized(B), type_int_sized(B)) :- necessary_int_size(N, B).
typeof(expr_var(V), Env, T, T) :- env_var_type(Env, V, T).

typeof(expr_plus(E1, E2), Env, T, R) :- typeof(expr_arith_binop(plus, E1, E2), Env, T, R).
typeof(expr_minus(E1, E2), Env, T, R) :- typeof(expr_arith_binop(minus, E1, E2), Env, T, R).
typeof(expr_times(E1, E2), Env, T, R) :- typeof(expr_arith_binop(times, E1, E2), Env, T, R).
typeof(expr_divide(E1, E2), Env, T, R) :- typeof(expr_arith_binop(divide, E1, E2), Env, T, R).
typeof(expr_less(E1, E2), Env, T, R) :- typeof(expr_compar_binop(less, E1, E2), Env, T, R).
typeof(expr_lessequal(E1, E2), Env, T, R) :- typeof(expr_compar_binop(lessequal, E1, E2), Env, T, R).

typeof(expr_arith_binop(Op, E1, E2), Env, T, R) :-
	typeof(E1, Env, T1, R1), typeof(E2, Env, T2, R2),
	binop(Op, T1, T2, T, R1, R2, R).
typeof(expr_compar_binop(Op, E1, E2), Env, type_int_sized(b1), R) :-
	typeof(E1, Env, T1, R1), typeof(E2, Env, T2, R2),
	binop(Op, T1, T2, type_int_sized(b1), R1, R2, R).

% typeof(Expr, Env, T, coerce(T1, R1)) :-
%     typeof(Expr, Env, T1, R1), can_coerce(T1, T).

binop(Op, T, T, T, R1, R2, expr_binop(Op, R1, R2)) :- integral_binop(Op), type_isintegral(T).
binop(Op, T1, T2, T3, R1, R2, R) :-
	integral_binop(Op),
	type_isintegral(T1), type_isintegral(T2), type_isintegral(T3),
	(T1 = Tin -> R1c = R1 ; can_coerce(T1, Tin), R1c = coerce(Tin, R1)),
	(T2 = Tin -> R2c = R2 ; can_coerce(T2, Tin), R2c = coerce(Tin, R2)),
	(T3 = Tin -> R = expr_binop(Op, R1c, R2c) ; can_coerce(Tin, T3), R = coerce(T3, expr_binop(Op, R1c, R2c))).

integral_binop(plus).
integral_binop(minus).
integral_binop(times).
integral_binop(divide).

type_isintegral(type_int).
type_isintegral(type_int_sized(_)).

can_coerce(type_int_sized(B), type_int) :- int_size_leq(B, b64).
can_coerce(type_int, type_int_sized(b64)).
can_coerce(type_int_sized(B), type_int_sized(B2)) :- int_size_leq(B, B2).

int_size_equiv(b1, 1).
int_size_equiv(b8, 8).
int_size_equiv(b16, 16).
int_size_equiv(b32, 32).
int_size_equiv(b64, 64).
int_size_leq(B1, B2) :- int_size_equiv(B1, N1), int_size_equiv(B2, N2), N1 =< N2.
int_size_lt(B1, B2) :- int_size_equiv(B1, N1), int_size_equiv(B2, N2), N1 < N2.

necessary_bits(0, 1) :- !.
necessary_bits(N, B) :- N < 0, !, M is -N - 1, necessary_bits(M, B).
necessary_bits(N, B) :-
	M is msb(N), B is M + 2.

% Note that we don't include booleans (aka b1) here
necessary_int_size(N, b8) :- necessary_bits(N, B), B =< 8, !.
necessary_int_size(N, b16) :- necessary_bits(N, B), B =< 16, !.
necessary_int_size(N, b32) :- necessary_bits(N, B), B =< 32, !.
necessary_int_size(N, b64) :- necessary_bits(N, B), B =< 64, !.
