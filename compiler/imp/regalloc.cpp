#include <stack>
#include <utility>
#include <algorithm>
#include <cassert>
#include "regalloc.h"

using namespace std;


Alloc Alloc::inReg(int hreg) {
	Alloc a;
	a.tag = HREG;
	a.hreg = hreg;
	return a;
}

Alloc Alloc::spill() {
	Alloc a;
	a.tag = SPILL;
	return a;
}

ostream& operator<<(ostream &os, const Alloc &alloc) {
	switch (alloc.tag) {
		case Alloc::HREG:
			return os << "hreg(" << alloc.hreg << ")";

		case Alloc::SPILL:
			return os << "spill";

		default:
			assert(false);
	}
}


unordered_map<Loc, Alloc> regalloc(
		const unordered_map<Loc, Interval> &ivMap, const vector<int> &hregs) {

	stack<int> available;
	for (int r : hregs) available.push(r);

	struct Ival {
		Loc loc;
		Interval interval;
	};

	vector<Ival> ivs;
	for (const auto &p : ivMap) ivs.push_back({p.first, p.second});
	sort(ivs.begin(), ivs.end(), [](const Ival &a, const Ival &b) {
		return a.interval.from < b.interval.from;
	});

	vector<Ival> active;

	unordered_map<Loc, Alloc> allocation;

	for (const Ival &ival : ivs) {
		// Expire old intervals
		size_t k;
		for (k = 0; k < active.size(); k++) {
			const Ival &j = active[k];
			if (j.interval.to >= ival.interval.from) {
				break;
			}
			assert(allocation[j.loc].tag == Alloc::HREG);
			available.push(allocation[j.loc].hreg);
		}
		active.erase(active.begin(), active.begin() + k);

		if (available.size() == 0) {
			// Spill at interval
			const Ival &spill = active.back();
			if (spill.interval.to > ival.interval.to) {
				allocation[ival.loc] = allocation[spill.loc];

				allocation[spill.loc] = Alloc::spill();
				active.pop_back();

				active.push_back(ival);
				sort(active.begin(), active.end(), [](const Ival &a, const Ival &b) {
					return a.interval.to < b.interval.to;
				});
			} else {
				allocation[ival.loc] = Alloc::spill();
			}
		} else {
			allocation[ival.loc] = Alloc::inReg(available.top());
			available.pop();

			active.push_back(ival);
			sort(active.begin(), active.end(), [](const Ival &a, const Ival &b) {
				return a.interval.to < b.interval.to;
			});
		}
	}

	return allocation;
}


static void collectLocs(const IRIns &ins, set<Loc> &locs) {
	for (Loc loc : onlyIRRegs(ins.read())) locs.insert(loc);
	for (Loc loc : onlyIRRegs(ins.written())) locs.insert(loc);
}

static void collectLocs(const IRTerm &term, set<Loc> &locs) {
	for (Loc loc : onlyIRRegs(term.read())) locs.insert(loc);
}

static void collectLocs(
		const IFunc &ifunc, Id bid, set<Loc> &locs, set<Id> &seen) {

	seen.insert(bid);
	const BB &bb = ifunc.BBs.find(bid)->second;

	for (const IRIns &ins : bb.inss) {
		collectLocs(ins, locs);
	}
	collectLocs(bb.term, locs);

	for (Id next : bb.term.nexts()) {
		if (seen.count(next) == 0) {
			collectLocs(ifunc, next, locs, seen);
		}
	}
}

static void applyRegalloc(
		BB &bb,
		const unordered_map<Loc, Alloc> &allocation,
		const unordered_map<Loc, i64> &spillOffset) {

	for (size_t i = 0; i < bb.inss.size(); i++) {
		IRIns &ins = bb.inss[i];

		vector<IRIns> prefix, suffix;

		vector<Loc> available = {Loc::reg(RRET), Loc::reg(RLINK)};
		ins.forEachRead([&available, &prefix, &allocation, &spillOffset](Loc &loc) {
			if (!isIRReg(loc)) return;
			const Alloc &alloc = allocation.find(loc)->second;
			if (alloc.tag == Alloc::SPILL) {
				assert(available.size() >= 1);
				Loc reg = available.back(); available.pop_back();

				i64 offset = spillOffset.find(loc)->second;
				prefix.push_back(IRIns::li(reg, offset));
				prefix.push_back(IRIns::arith(Arith::ADD, reg, Loc::reg(RSP), reg));
				prefix.push_back(IRIns::load(reg, reg, 8));

				loc.n = reg.n;
			} else {
				loc.n = alloc.hreg;
			}
		});

		available = {Loc::reg(RRET), Loc::reg(RLINK)};
		ins.forEachWrite([&available, &suffix, &allocation, &spillOffset](Loc &loc) {
			if (!isIRReg(loc)) return;
			const Alloc &alloc = allocation.find(loc)->second;
			if (alloc.tag == Alloc::SPILL) {
				assert(available.size() >= 2);
				Loc ptrreg = available.back(); available.pop_back();
				Loc reg = available.back(); available.pop_back();

				i64 offset = spillOffset.find(loc)->second;
				suffix.push_back(IRIns::li(ptrreg, offset));
				suffix.push_back(IRIns::arith(Arith::ADD, ptrreg, Loc::reg(RSP), ptrreg));
				suffix.push_back(IRIns::store(ptrreg, reg, 8));

				loc.n = reg.n;
			} else {
				loc.n = alloc.hreg;
			}
		});

		bb.inss.insert(bb.inss.begin() + i, prefix.begin(), prefix.end());
		bb.inss.insert(bb.inss.begin() + i + prefix.size() + 1, suffix.begin(), suffix.end());
		i += prefix.size() + suffix.size();
	}

	// TODO: more DRY

	vector<IRIns> prefix;

	vector<Loc> available = {Loc::reg(RRET), Loc::reg(RLINK)};
	bb.term.forEachRead([&](Loc &loc) {
		if (loc.tag != Loc::REG) return;
		const Alloc &alloc = allocation.find(loc)->second;
		if (alloc.tag == Alloc::SPILL) {
			assert(available.size() >= 1);
			Loc reg = available.back(); available.pop_back();

			i64 offset = spillOffset.find(loc)->second;
			prefix.push_back(IRIns::li(reg, offset));
			prefix.push_back(IRIns::arith(Arith::ADD, reg, Loc::reg(RSP), reg));
			prefix.push_back(IRIns::load(reg, reg, 8));

			loc.n = reg.n;
		} else {
			loc.n = alloc.hreg;
		}
	});

	bb.inss.insert(bb.inss.end(), prefix.begin(), prefix.end());
}

i64 applyRegalloc(IFunc &ifunc, const unordered_map<Loc, Alloc> &allocation) {
	unordered_map<Loc, i64> spillOffset;

	const Id startid = 0;

	set<Loc> locs;
	set<Id> seen;
	collectLocs(ifunc, startid, locs, seen);

	// The first spilled value has offset 0 and will be put at [RSP] (after
	// subtracting 8 * nspilled from RSP).

	int nspilled = 0;

	for (const Loc &loc : locs) {
		const Alloc &alloc = allocation.find(loc)->second;
		if (alloc.tag == Alloc::SPILL) {
			assert(spillOffset.find(loc) == spillOffset.end());
			spillOffset[loc] = 8 * nspilled;
			nspilled++;
		}
	}

	if (spillOffset.size() > 0) {
		cerr << "WARNING: applyRegalloc is untested for spilled values!" << endl;

		vector<IRIns> prefix;
		prefix.push_back(IRIns::li(Loc::reg(RLINK), 8 * nspilled));
		prefix.push_back(IRIns::arith(Arith::SUB, Loc::reg(RSP), Loc::reg(RSP), Loc::reg(RLINK)));

		ifunc.BBs[startid].inss.insert(ifunc.BBs[startid].inss.begin(), prefix.begin(), prefix.end());

		vector<IRIns> suffix;
		prefix.push_back(IRIns::li(Loc::reg(RLINK), 8 * nspilled));
		prefix.push_back(IRIns::arith(Arith::ADD, Loc::reg(RSP), Loc::reg(RSP), Loc::reg(RLINK)));

		insertAtRet(ifunc, suffix);
	}

	for (auto &p : ifunc.BBs) {
		applyRegalloc(p.second, allocation, spillOffset);
	}

	return 8 * nspilled;
}
