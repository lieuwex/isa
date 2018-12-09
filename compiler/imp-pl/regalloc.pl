% regalloc(++Instrs, +Regs, -Mapping).
%
% Instrs: list of terms, where the terms are of the form instr(Read, Written,
%   Nexts). Each such term represents an instruction in a program. Read is a
%   list of integers indicating the registers read by the instruction, and
%   Written similarly indicates the registers written. Nexts is a list of
%   indices into the input list, zero-based, that indicate the instructions that
%   can follow this one.
%
% HRegs: list of values representing the hardware registers available. The type
%   of these values is not relevant.
%
% Mapping: a dict mapping registers (i.e. integers) to a term, either 'reg(R)'
%   if the register was allocated to a hardware register, or 'spill' if the
%   register was spilled to memory.

regalloc(Instrs, HRegs, Mapping) :-
	liveness_analysis(Instrs, Sets),
	maplist(set_min_max, Sets, Intervals).

set_min_max([X | L], Iv) :- set_min_max_i(L, iv(X, X), Iv).
set_min_max_i([], iv(Min, Max), iv(Min, Max)).
set_min_max_i([X | L], iv(Min1, Max1), iv(Min, Max)) :-
	!, Min2 is min(X, Min1), Max2 is max(X, Max2),
	set_min_max_i(L, iv(Min2, Max2), iv(Min, Max)).


% vim: set ft=prolog:
