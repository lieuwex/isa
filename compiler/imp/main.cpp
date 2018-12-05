#include <iostream>
#include <fstream>
#include "ast.h"
#include "parser.h"
#include "typecheck.h"
#include "ir.h"
#include "to_ir.h"
#include "optimiser.h"
#include "ir_order.h"
#include "live_analysis.h"
#include "regalloc.h"
#include "assemble.h"
#include "print_asm.h"

using namespace std;


static void readfull(istream &in, string &dst) {
	char buf[1024];
	do {
		in.read(buf, sizeof buf);
		dst.insert(dst.end(), buf, buf + in.gcount());
	} while (in);
}


int main(int argc, char **argv) {
	if (argc != 2) {
		cerr << "Usage: " << argv[0] << " <file.simp>" << endl;
		return 1;
	}

	string source;
	{
		ifstream file(argv[1]);
		readfull(file, source);
	}

	Program program = parseProgram(SExpr::parse(source));

	// for (const Function &function : program.functions) {
	//     function.body.writeProlog(cerr);
	//     cerr << endl;
	// }

	typecheck(program);

	IR ir = toIR(program);
	cerr << ir << endl;

	optimise(ir);
	cerr << ir << endl;

	for (auto &p : ir.funcs) {
		IFunc &ifunc = p.second;

		ifunc.bbOrder = orderBBs(ifunc);
		cerr << "ORDER" << endl;
		for (Id id : ifunc.bbOrder) cerr << id << ' ';
		cerr << endl;

		unordered_map<Loc, Interval> liveness = live_analysis(ifunc);
		cerr << "LIVENESS" << endl;
		for (const auto &p : liveness) {
			cerr << p.first << ": " << p.second.from << " - " << p.second.to << endl;
		}

		// 0=PC, 13=RET, 14=LINK, 15=SP
		// We use RET, LINK and 12 for hacks when necessary.
		unordered_map<Loc, Alloc> allocation = regalloc(liveness, {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11});
		cerr << "REGALLOC" << endl;
		for (const auto &p : allocation) {
			cerr << p.first << ": " << p.second << endl;
		}

		i64 rspShift = applyRegalloc(ifunc, allocation);

		assemble(ifunc, rspShift);
		cerr << ifunc << endl;
	}

	printASM(cout, ir);
}
