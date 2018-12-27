#include <cassert>
#include "print_asm.h"
#include "prologue.asm.h"
#include "epilogue.asm.h"

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
			os << "\tmv r1, r1" << endl;
			break;

		case IRIns::LI:
			os << "\tli ";
			printASM(os, ins.rd);
			os << ", " << ins.number << endl;
			break;

		case IRIns::MOV:
			os << "\tmv ";
			printASM(os, ins.rd);
			os << ", ";
			printASM(os, ins.r1);
			os << endl;
			break;

		case IRIns::STORE:
			os << "\ts" << 8 * ins.size << " ";
			printASM(os, ins.rd);
			os << ", ";
			printASM(os, ins.r1);
			os << endl;
			break;

		case IRIns::LOAD:
			os << "\tl" << 8 * ins.size << " ";
			printASM(os, ins.rd);
			os << ", ";
			printASM(os, ins.r1);
			os << endl;
			break;

		case IRIns::ARITH:
			switch (ins.op) {
				case Arith::ADD: os << "\tadd"; break;
				case Arith::SUB: os << "\tsub"; break;
				case Arith::MUL: os << "\tmul"; break;
				case Arith::DIV: os << "\tdiv"; break;
				case Arith::LT: os << "\tlt"; break;
				case Arith::LTE: os << "\tlte"; break;
				case Arith::SLL: os << "\tsll"; break;
				case Arith::SLR: os << "\tslr"; break;
				case Arith::SAR: os << "\tsar"; break;
				case Arith::AND: os << "\tand"; break;
				case Arith::OR: os << "\tor"; break;
				case Arith::XOR: os << "\txor"; break;
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
			os << "\tcall r14, " << ins.name << endl;
			break;

		case IRIns::DEBUGGER:
			os << "\tdebugger" << endl;
			break;

		default:
			assert(false);
	}
}

static void printASM(ostream &os, const IRTerm &term, const string &lblPrefix) {
	switch (term.tag) {
		case IRTerm::JMP:
			os << "\tjnz r0, BB" << lblPrefix << term.id << endl;
			break;

		case IRTerm::JZ:
			os << "\tjz ";
			printASM(os, Loc(term.r1));
			os << ", BB" << lblPrefix << term.idT << endl;
			os << "\tjnz r0, BB" << lblPrefix << term.idF << endl;
			break;

		case IRTerm::RET:
			os << "\tmv r0, r14" << endl;
			break;

		case IRTerm::UNREACH:
			os << "\tunreachable" << endl;
			break;

		default:
			assert(false);
	}
}

static void printASM(ostream &os, const BB &bb, const string &lblPrefix) {
	os << "BB" << lblPrefix << bb.id << ":" << endl;
	for (const IRIns &ins : bb.inss) {
		printASM(os, ins);
	}
	printASM(os, bb.term, lblPrefix);
}

static void printASM(ostream &os, const IFunc &ifunc) {
	os << ifunc.name << ":" << endl;
	for (Id id : ifunc.bbOrder) {
		printASM(os, ifunc.BBs.find(id)->second, ifunc.name);
	}
}

static void printPrologue(ostream &os) {
	os.write((const char*)prologue_asm, prologue_asm_len);
}

static void printEpilogue(ostream &os) {
	os.write((const char*)epilogue_asm, epilogue_asm_len);
}

void printASM(ostream &os, const IR &ir) {
	printPrologue(os);
	for (const auto &p : ir.funcs) {
		printASM(os, p.second);
	}
	printEpilogue(os);
}
