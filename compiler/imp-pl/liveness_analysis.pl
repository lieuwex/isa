use_module(library(assoc)).

% Instrs: [instr(Read, Written, Nexts)]
% Sets: assoc(reg-[Idx])
liveness_analysis(Instrs, Sets) :-
	conv_instrs_pairs(Instrs, Pairs),
	list_to_assoc(Pairs, IAN),  % IAN: assoc(Idx-instr(Read, Written, Nexts))
	find_prevs(IAN, IA),  % IA: assoc(Idx-instr(Read, Written, Nexts, Prevs))
	empty_assoc(IsLive),  % IsLive: assoc(Idx-[Reg])
	fill_loop(IA, IsLive, IsLive2),
	build_sets(IsLive2, Sets).

fill_loop(IA, IsLiveIn, IsLiveOut) :-
	live_prop(IA, Idx, IsLiveIn, IsLive2, Changed),
	(Changed = 1
		-> fill_loop(IA, Pairs, IsLive2, IsLiveOut)
		;  IsLive2 = IsLiveOut).

live_prop(IA, Idx, IsLiveIn, IsLiveOut, Changed) :-
	get_assoc(Idx, IA, instr(Read, Written, Nexts, Prevs)),
	get_assoc(Idx, IsLiveIn, Live),
	.


conv_instrs_pairs(Instrs, Pairs) :- conv_instrs_pairs_i(Instrs, 0, Pairs).
conv_instrs_pairs_i([], _, []).
conv_instrs_pairs_i([Ins | Instrs], Idx, [Pair | Pairs]) :-
	conv_instrs_pairs_item(Ins, Idx, Pair),
	Idx1 is Idx + 1,
	conv_instrs_pairs_i(Instrs, Idx1, Pairs).
conv_instrs_pairs_item(instr(Read, Written, Nexts), Idx, Idx-instr(Read, Written, Nexts)).

replicate(_, 0, []) :- !.
replicate(Value, Times, [Value | L]) :- N is Times - 1, replicate(Value, N, L).

build_sets(IsLive, Sets) :-
	assoc_to_list(IsLive, Pairs),
	build_sets_pairs(Pairs, Sets).
build_sets_pairs([], Sets) :- empty_assoc(Sets).
build_sets_pairs([Idx-Regs | IsLive], Sets) :-
	build_sets_pairs(IsLive, Sets1),
	add_to_sets(Regs, Idx, Sets1, Sets).
add_to_sets([], _, Sets, Sets).
add_to_sets([Reg | Regs], Idx, Sets1, Sets) :-
	(get_assoc(Reg, Sets1, List)
		-> put_assoc(Reg, Sets1, [Idx | List], Sets2)
		;  put_assoc(Reg, Sets1, [Idx], Sets2)),
	add_to_sets(Regs, Idx, Sets2, Sets).

find_prevs(IAN, IA) :-
	map_assoc(make_empty_prevs, IAN, IA1),
	assocs_to_list(IAN, Pairs),
	find_prevs(Pairs, IA1, IA).
find_prevs([], IA1, IA).
find_prevs([Idx-instr(Read, Written, Nexts) | Pairs], IA1, IA) :-
	add_prevs(Nexts, Idx, IA1, IA2),
	find_prevs(Pairs, IA2, IA).
add_prevs([], _, IA, IA).
add_prevs([Idx | Idcs], Prev, IA1, IA3) :-
	get_assoc(Idx, IA1, instr(Read, Written, Nexts, Prevs)),
	put_assoc(Idx, IA1, [Prev | Prevs], IA2),
	add_prevs(Idcs, Prev, IA2, IA3).

make_empty_prevs(instr(Read, Written, Nexts), instr(Read, Written, Nexts, [])).


% vim: set ft=prolog:
