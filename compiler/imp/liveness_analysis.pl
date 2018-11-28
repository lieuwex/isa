use_module(library(assoc)).

liveness_analysis(Instrs, Sets) :-
	conv_instrs_pairs(Instrs, Pairs),
	list_to_assoc(Pairs, IA).


conv_instrs_pairs(Instrs, Pairs) :- conv_instrs_pairs_i(Instrs, 0, Pairs).
conv_instrs_pairs_i([], _, []).
conv_instrs_pairs_i([Ins | Instrs], Idx, [Pair | Pairs]) :-
	conv_instrs_pairs_item(Ins, Idx, Pair),
	Idx1 is Idx + 1,
	conv_instrs_pairs_i(Instrs, Idx1, Pairs).
conv_instrs_pairs_item(_(Read, Written, Nexts), Idx, Idx-instr(Read, Written, Nexts)).


% vim: set ft=prolog:
