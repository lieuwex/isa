#include "unmacro.h"

using namespace std;


static void unmacro(BB &bb) {
	for (size_t i = 0; i < bb.inss.size(); i++) {
		IRIns &ins = bb.inss[i];

		switch (ins.tag) {
			case IRIns::SEXT:
				if (ins.sizeto <= ins.sizefrom) {
					ins.tag = IRIns::MOV;
					break;
				}

				vector<IRIns> repl;
				repl.push_back(IRIns::li(Loc::reg(RLINK), 64 - ins.sizefrom));
				repl.push_back(IRIns::arith(Arith::SLL, Loc::reg(RRET), ins.r1, Loc::reg(RLINK)));
				repl.push_back(IRIns::arith(Arith::SAR, ins.rd, Loc::reg(RRET), Loc::reg(RLINK)));

				bb.inss[i] = repl[0];
				bb.inss.insert(bb.inss.begin() + i + 1, repl.begin() + 1, repl.end());
				break;
		}
	}
}

static void unmacro(IFunc &ifunc) {
	for (auto &p : ifunc.BBs) {
		unmacro(p.second);
	}
}

void unmacro(IR &ir) {
	for (auto &p : ir.funcs) {
		unmacro(p.second);
	}
}
