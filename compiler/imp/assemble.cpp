#include <cassert>
#include "assemble.h"

using namespace std;


static void replaceArgRefs(IFunc &ifunc, i64 rspShift) {
	const auto argOffset = [rspShift](int argNum) {
		// rspShift to cover the offset due to spilling of IR registers
		// 8 to cover the link register storage
		// 8 * argNum to find the right argument in the list of the stack
		return rspShift + 8 + 8 * argNum;
	};

	for (auto &p : ifunc.BBs) {
		BB &bb = p.second;

		for (size_t i = 0; i < bb.inss.size(); i++) {
			vector<IRIns> prefix;

			int numArgsInInstr = 0;
			bb.inss[i].forEachRead([&](Loc &loc) {
				if (loc.tag != Loc::ARG) return;
				numArgsInInstr++;
				assert(numArgsInInstr <= 1);

				prefix.push_back(IRIns::li(Loc::reg(RRET), argOffset(loc.n)));
				prefix.push_back(IRIns::arith(Arith::ADD, Loc::reg(RRET), Loc::reg(RRET), Loc::reg(RSP)));
				loc.tag = Loc::REG;
				loc.n = RRET;
			});

			bb.inss.insert(bb.inss.begin() + i, prefix.begin(), prefix.end());
			i += prefix.size();
		}

		vector<IRIns> prefix;

		int numArgsInInstr = 0;
		bb.term.forEachRead([&](Loc &loc) {
			if (loc.tag != Loc::ARG) return;
			numArgsInInstr++;
			assert(numArgsInInstr <= 1);

			prefix.push_back(IRIns::li(Loc::reg(RRET), argOffset(loc.n)));
			prefix.push_back(IRIns::arith(Arith::ADD, Loc::reg(RRET), Loc::reg(RRET), Loc::reg(RSP)));
			loc.tag = Loc::REG;
			loc.n = RRET;
		});

		bb.inss.insert(bb.inss.end(), prefix.begin(), prefix.end());
	}
}

static void replaceSpecialLocs(IFunc &ifunc) {
	const auto handle = [](Loc loc) {
		if (loc.tag == Loc::REG) {
			switch (loc.n) {
				case RPC: loc.n = 0; break;
				case RRET: loc.n = 13; break;
				case RLINK: loc.n = 14; break;
				case RSP: loc.n = 15; break;
			}
		}
		return loc;
	};

	for (auto &p : ifunc.BBs) {
		BB &bb = p.second;

		for (size_t i = 0; i < bb.inss.size(); i++) {
			bb.inss[i].forEachRead([&](Loc &loc) {
				loc = handle(loc);
			});
			bb.inss[i].forEachWrite([&](Loc &loc) {
				loc = handle(loc);
			});
		}

		bb.term.forEachRead([&](Loc &loc) {
			loc = handle(loc);
		});
	}
}

static void entryStoreLink(IFunc &ifunc) {
	vector<IRIns> prefix;
	prefix.push_back(IRIns::li(Loc::reg(RRET), 8));
	prefix.push_back(IRIns::arith(Arith::SUB, Loc::reg(RSP), Loc::reg(RSP), Loc::reg(RRET)));
	prefix.push_back(IRIns::store(Loc::reg(RSP), Loc::reg(RLINK), 8));

	BB &startBB = ifunc.BBs[0];
	startBB.inss.insert(startBB.inss.begin(), prefix.begin(), prefix.end());
}

static void retRestoreLink(IFunc &ifunc) {
	// Note that we use register 12 here, where otherwise we would refrain from
	// doing administrative actions using anything else than RLINK and RRET.
	// However, both are taken here, so we have to use another register. HACK.
	vector<IRIns> suffix;
	suffix.push_back(IRIns::load(Loc::reg(RLINK), Loc::reg(RSP), 8));
	suffix.push_back(IRIns::li(Loc::reg(12), 8));
	suffix.push_back(IRIns::arith(Arith::ADD, Loc::reg(RSP), Loc::reg(RSP), Loc::reg(12)));

	insertAtRet(ifunc, suffix);
}

// Only collects "normal" registers; i.e. skips RPC, RRET, RLINK and RSP.
static void collectRegs(const BB &bb, set<Loc> &usedRegs) {
	const auto handle = [&usedRegs](const Loc &loc) {
		// 13, 14 and 15 are special registers that we don't need here.
		// 12 is used as a special register as well; HACK.
		if (loc.tag == Loc::REG && loc.n >= 1 && loc.n <= 11) {
			usedRegs.insert(loc);
		}
	};

	for (const IRIns &ins : bb.inss) {
		for (const Loc &loc : ins.read()) handle(loc);
		for (const Loc &loc : ins.written()) handle(loc);
	}

	for (const Loc &loc : bb.term.read()) handle(loc);
}

static void calleeSaveRegisters(IFunc &ifunc, const set<Loc> &usedRegs) {
	vector<Loc> usedRegsOrder(usedRegs.begin(), usedRegs.end());

	vector<IRIns> prefix;
	prefix.push_back(IRIns::li(Loc::reg(RLINK), 8));
	for (const Loc &loc : usedRegsOrder) {
		prefix.push_back(IRIns::arith(Arith::SUB, Loc::reg(RSP), Loc::reg(RSP), Loc::reg(RLINK)));
		prefix.push_back(IRIns::store(Loc::reg(RSP), loc, 8));
	}

	BB &startBB = ifunc.BBs[0];
	startBB.inss.insert(startBB.inss.begin(), prefix.begin(), prefix.end());

	reverse(usedRegsOrder.begin(), usedRegsOrder.end());

	vector<IRIns> suffix;
	suffix.push_back(IRIns::li(Loc::reg(RLINK), 8));
	for (const Loc &loc : usedRegsOrder) {
		suffix.push_back(IRIns::load(loc, Loc::reg(RSP), 8));
		suffix.push_back(IRIns::arith(Arith::ADD, Loc::reg(RSP), Loc::reg(RSP), Loc::reg(RLINK)));
	}

	insertAtRet(ifunc, suffix);
}

/*
Function layout:
- First store the link register. This frees up RLINK. This is done in
  entryStoreLink();
- Then save all the callee-saved registers. This is done in
  calleeSaveRegisters(). We can use RLINK here.
- Then reserve space for the spilled IR registers. This has already
  been done when assemble() is called.
[ ... normal function body ... ]
- Reclaim the spill space. This has already been done.
- Restore all the callee-saved registers. This is done in
  calleeSaveRegisters, which can use RLINK still.
- Finally, restore the link register. This is done in retRestoreLink().
  Here we need to resort to r12. HACK.
*/

void assemble(IFunc &ifunc, i64 rspShift) {
	set<Loc> usedRegs;
	for (const auto &p : ifunc.BBs) collectRegs(p.second, usedRegs);

	// The 8 * usedRegs.size() compensates for the register backup that
	// we'll add afterwards.
	replaceArgRefs(ifunc, rspShift + 8 * usedRegs.size());

	calleeSaveRegisters(ifunc, usedRegs);

	entryStoreLink(ifunc);
	retRestoreLink(ifunc);

	replaceSpecialLocs(ifunc);
}
