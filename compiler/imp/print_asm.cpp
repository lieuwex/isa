#include "print_asm.h"

using namespace std;


static void printASM(ostream &os, const Loc &loc) {
	switch (loc.tag) {
		case Loc::REG:
			os << "r" << loc.n;
			break;

		default:
			assert(false);
	}
}

static void printASM(ostream &os, const IRIns &ins) {
	switch (ins.tag) {
		case IRIns::NOP:
			os << "mv r1, r1" << endl;
			break;

		case IRIns::LI:
			os << "li ";
			printASM(os, ins.rd);
			os << ", " << ins.number << endl;
			break;

		case IRIns::MOV:
			os << "mv ";
			printASM(os, ins.rd);
			os << ", ";
			printASM(os, ins.r1);
			os << endl;
			break;

		case IRIns::STORE:
			os << "s" << 8 * ins.size << " ";
			printASM(os, ins.rd);
			os << ", ";
			printASM(os, ins.r1);
			os << endl;
			break;

		case IRIns::LOAD:
			os << "l" << 8 * ins.size << " ";
			printASM(os, ins.rd);
			os << ", ";
			printASM(os, ins.r1);
			os << endl;
			break;

		case IRIns::ARITH:
			switch (ins.op) {
				case Arith::ADD: os << "add"; break;
				case Arith::SUB: os << "sub"; break;
				case Arith::MUL: os << "mul"; break;
				case Arith::DIV: os << "div"; break;
				case Arith::LT: os << "lt"; break;
				case Arith::LTE: os << "lte"; break;
				default: assert(false);
			}
			os << " ";
			printASM(os, ins.rd);
			os << ", ";
			printASM(os, ins.r1);
			os << ", ";
			printASM(os, ins.r2);
			os << endl;
			break;

		case IRIns::CALL:
			os << "call r14, " << ins.name << endl;
			break;

		default:
			assert(false);
	}
}

static void printASM(ostream &os, const IRTerm &term, const string &lblPrefix) {
	switch (term.tag) {
		case IRTerm::JMP:
			os << "jnz r0, BB" << lblPrefix << term.id << endl;
			break;

		case IRTerm::JZ:
			os << "jz ";
			printASM(os, Loc(term.r1));
			os << ", BB" << lblPrefix << term.idT;
			os << ", BB" << lblPrefix << term.idF << endl;
			break;

		case IRTerm::RET:
			os << "mv r0, r14" << endl;
			break;

		case IRTerm::UNREACH:
			os << "unreachable" << endl;
			break;

		default:
			assert(false);
	}
}

static void printASM(ostream &os, const BB &bb, const string &lblPrefix) {
	os << "BB" << lblPrefix << bb.id << ":" << endl;
	for (const IRIns &ins : bb.inss) {
		os << "\t";
		printASM(os, ins);
	}
	os << "\t";
	printASM(os, bb.term, lblPrefix);
}

static void printASM(ostream &os, const IFunc &ifunc) {
	os << ifunc.name << ":" << endl;
	for (const auto &p : ifunc.BBs) {
		printASM(os, p.second, ifunc.name);
	}
}

static void printPrologue(ostream &os) {
	os << "\tli r15, 4000" << endl;
	os << "\tcall r14, main" << endl;
	os << "\tjnz r0, exit" << endl;
	os << endl;
}

static void printEpilogue(ostream &os) {
	os << endl;
	os << "exit:" << endl;
}

void printASM(ostream &os, const IR &ir) {
	printPrologue(os);
	for (const auto &p : ir.funcs) {
		printASM(os, p.second);
	}
	printEpilogue(os);
}
