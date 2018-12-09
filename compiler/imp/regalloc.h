#pragma once

#include <iostream>
#include <unordered_map>
#include <vector>
#include "live_analysis.h"

using namespace std;


struct Alloc {
	enum {
		HREG,  // hreg
		SPILL,
	};

	int tag = -1;
	int hreg = -1;

	static Alloc inReg(int hreg);
	static Alloc spill();
};

ostream& operator<<(ostream &os, const Alloc &alloc);


unordered_map<Loc, Alloc> regalloc(
		const unordered_map<Loc, Interval> &ivMap, const vector<int> &hregs);

// Returns the amount the stack pointer has been decreased by at function
// entry. This should be used to correctly replace argument references
// afterwards.
i64 applyRegalloc(IFunc &ifunc, const unordered_map<Loc, Alloc> &allocation);
