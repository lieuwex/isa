#pragma once

#include <vector>
#include <set>
#include <unordered_map>
#include "ir.h"

using namespace std;


struct Interval {
	// Zero-based instruction indices when considering the BB's laid
	// out in order specified by bbOrder, with the first instruction
	// of the next BB directly following the terminator of the last
	// one.
	// Both limits are inclusive.
	int from = 0, to = -1;

	void include(int index);
};

unordered_map<Loc, Interval> live_analysis(const IFunc &ifunc);
