#include <cassert>
#include "live_analysis.h"

using namespace std;


void Interval::include(int index) {
	if (from > to) {
		from = to = index;
	} else {
		from = min(from, index);
		to = max(to, index);
	}
}


struct GK {
	set<Loc> gen, kill, liveIn, liveOut;
	set<size_t> nexts, prevs;  
};

static void fill_prevs(vector<GK> &blocks) {
	for (size_t i = 0; i < blocks.size(); i++) {
		for (size_t n : blocks[i].nexts) {
			blocks[n].prevs.insert(i);
		}
	}
}

// Backwards topological sort, breaking cycles to get a full ordering
static vector<size_t> lenient_topsort(const vector<GK> &blocks) {
	vector<int> incoming(blocks.size());
	for (size_t i = 0; i < blocks.size(); i++) {
		incoming[i] = blocks[i].prevs.size();
	}

	vector<size_t> L;
	vector<size_t> S;
	for (size_t i = 0; i < blocks.size(); i++) {
		if (incoming[i] == 0) S.push_back(i);
	}

	while (true) {
		while (S.size() > 0) {
			size_t bl = S.back(); S.pop_back();
			L.push_back(bl);
			for (size_t n : blocks[bl].prevs) {
				// This if is only necessary because of the cycle-breaking.
				if (incoming[n] > 0) {
					incoming[n]--;
					if (incoming[n] == 0) S.push_back(n);
				}
			}
		}

		if (L.size() == blocks.size()) break;

		// Break a cycle by choosing a min-in-degree node and lifting
		// it to a source
		int min_degree = blocks.size() + 1;
		int min_at = -1;
		for (int node = 0; node < (int)blocks.size(); node++) {
			if (incoming[node] > 0 && incoming[node] < min_degree) {
				min_degree = incoming[node];
				min_at = node;
			}
		}

		assert(min_at != -1);
		incoming[min_at] = 0;  // fake
		S.push_back(min_at);
	}

	return L;
}

static void fill_live(vector<GK> &blocks) {
	vector<size_t> order = lenient_topsort(blocks);

	bool changed;
	do {
		changed = false;

		for (size_t idx : order) {
			set<Loc> newSet;
			for (size_t next : blocks[idx].nexts) {
				newSet.insert(blocks[next].liveIn.begin(), blocks[next].liveIn.end());
			}

			if (newSet != blocks[idx].liveOut) {
				changed = true;
				blocks[idx].liveOut = move(newSet);
			}
			newSet.clear();

			newSet = blocks[idx].gen;
			for (const Loc &loc : blocks[idx].liveOut) {
				if (blocks[idx].kill.count(loc) == 0) {
					newSet.insert(loc);
				}
			}

			if (newSet != blocks[idx].liveIn) {
				changed = true;
				blocks[idx].liveIn = move(newSet);
			}
		}
	} while (changed);
}

static void fill_nexts(
		vector<GK> &blocks, const IFunc &ifunc, const vector<Id> &bbOrder) {

	unordered_map<Id, size_t> bbIndex;
	for (size_t i = 0; i < bbOrder.size(); i++) {
		bbIndex[bbOrder[i]] = i;
	}

	for (size_t i = 0; i < bbOrder.size(); i++) {
		const BB &bb = ifunc.BBs.find(bbOrder[i])->second;
		for (Id id : bb.term.nexts()) {
			blocks[i].nexts.insert(bbIndex.find(id)->second);
		}
	}
}

static void fill_genkill(
		vector<GK> &blocks, const IFunc &ifunc, const vector<Id> &bbOrder) {

	for (size_t i = 0; i < bbOrder.size(); i++) {
		const BB &bb = ifunc.BBs.find(bbOrder[i])->second;

		for (const IRIns &ins : bb.inss) {
			for (const Loc &loc : onlyIRRegs(ins.read())) {
				if (blocks[i].kill.count(loc) == 0) {
					blocks[i].gen.insert(loc);
				}
			}

			for (const Loc &loc : onlyIRRegs(ins.written())) {
				blocks[i].kill.insert(loc);
			}
		}

		for (const Loc &loc : onlyIRRegs(bb.term.read())) {
			if (blocks[i].kill.count(loc) == 0) {
				blocks[i].gen.insert(loc);
			}
		}
	}
}

template <typename T>
__attribute__((unused))
ostream& operator<<(ostream &os, const set<T> &s) {
	os << '{';

	bool first = true;
	for (const T &t : s) {
		if (first) first = false;
		else os << ',';
		os << t;
	}

	return os << '}';
}

unordered_map<Loc, Interval> live_analysis(
		const IFunc &ifunc, const vector<Id> &bbOrder) {

	assert(ifunc.BBs.size() == bbOrder.size());

	vector<GK> blocks(bbOrder.size());
	fill_genkill(blocks, ifunc, bbOrder);
	fill_nexts(blocks, ifunc, bbOrder);
	fill_prevs(blocks);
	fill_live(blocks);

	unordered_map<Loc, Interval> ivs;

	int index = 0;
	for (size_t i = 0; i < bbOrder.size(); i++) {
		const BB &bb = ifunc.BBs.find(bbOrder[i])->second;

		set<Loc> live = blocks[i].liveOut;
		for (const Loc &loc : live) {
			ivs[loc].include(index + bb.inss.size());
		}

		// cerr << "LA: BB " << bb.id << endl;
		// cerr << "LA:   liveOut=" << live << endl;

		for (const Loc &loc : onlyIRRegs(bb.term.read())) {
			ivs[loc].include(index + bb.inss.size());
			live.insert(loc);
			// cerr << "LA:   read insert " << loc << endl;
		}

		for (int j = bb.inss.size() - 1; j >= 0; j--) {
			for (const Loc &loc : onlyIRRegs(bb.inss[j].written())) {
				if (live.count(loc) > 0) ivs[loc].include(index + j);
				live.erase(loc);
				// cerr << "LA:   write erase " << loc << endl;
			}

			for (const Loc &loc : onlyIRRegs(bb.inss[j].read())) {
				ivs[loc].include(index + j);
				live.insert(loc);
				// cerr << "LA:   read insert " << loc << endl;
			}
		}

		// cerr << "LA:   live   =" << live << endl;
		// cerr << "LA:   liveIn =" << blocks[i].liveIn << endl;

		assert(live == blocks[i].liveIn);
		for (const Loc &loc : live) ivs[loc].include(index);

		index += bb.inss.size() + 1;
	}

	return ivs;
}
