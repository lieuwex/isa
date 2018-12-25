#include <vector>
#include <unordered_map>
#include <cassert>
#include "ir.h"
#include "ir_builder.h"

using namespace std;


IRIns IRIns::nop() {
	IRIns ins;
	ins.tag = NOP;
	return ins;
}

IRIns IRIns::li(Loc rd, i64 number) {
	IRIns ins;
	ins.tag = LI;
	ins.rd = rd;
	ins.number = number;
	return ins;
}

IRIns IRIns::mov(Loc rd, Loc r1) {
	IRIns ins;
	ins.tag = MOV;
	ins.rd = rd;
	ins.r1 = r1;
	return ins;
}

IRIns IRIns::store(Loc rd, Loc r1, int size) {
	IRIns ins;
	ins.tag = STORE;
	ins.rd = rd;
	ins.r1 = r1;
	ins.size = size;
	return ins;
}

IRIns IRIns::load(Loc rd, Loc r1, int size) {
	IRIns ins;
	ins.tag = LOAD;
	ins.rd = rd;
	ins.r1 = r1;
	ins.size = size;
	return ins;
}

IRIns IRIns::arith(Arith op, Loc rd, Loc r1, Loc r2) {
	IRIns ins;
	ins.tag = ARITH;
	ins.op = op;
	ins.rd = rd;
	ins.r1 = r1;
	ins.r2 = r2;
	return ins;
}

IRIns IRIns::call(const string &name) {
	IRIns ins;
	ins.tag = CALL;
	ins.name = name;
	return ins;
}

IRIns IRIns::signExtend(Loc rd, Loc r1, int sizeto, int sizefrom) {
	IRIns ins;
	ins.tag = SEXT;
	ins.rd = rd;
	ins.r1 = r1;
	ins.sizeto = sizeto;
	ins.sizefrom = sizefrom;
	return ins;
}

set<Loc> IRIns::written() const {
	switch (tag) {
		case NOP: return {};
		case LI: return {rd};
		case MOV: return {rd};
		case STORE: return {};
		case LOAD: return {rd};
		case ARITH: return {rd};
		case CALL: return {};
		case SEXT: return {rd};
		default: assert(false);
	}
}

set<Loc> IRIns::read() const {
	switch (tag) {
		case NOP: return {};
		case LI: return {};
		case MOV: return {r1};
		case STORE: return {rd, r1};
		case LOAD: return {r1};
		case ARITH: return {r1, r2};
		case CALL: return {};
		case SEXT: return {r1};
		default: assert(false);
	}
}

void IRIns::forEachRead(function<void(Loc&)> f) {
	switch (tag) {
		case NOP: break;
		case LI: break;
		case MOV: f(r1); break;
		case STORE: f(rd); f(r1); break;
		case LOAD: f(r1); break;
		case ARITH: f(r1); f(r2); break;
		case CALL: break;
		case SEXT: f(r1); break;
		default: assert(false);
	}
}

void IRIns::forEachWrite(function<void(Loc&)> f) {
	switch (tag) {
		case NOP: break;
		case LI: f(rd); break;
		case MOV: f(rd); break;
		case STORE: break;
		case LOAD: f(rd); break;
		case ARITH: f(rd); break;
		case CALL: break;
		case SEXT: f(rd); break;
		default: assert(false);
	}
}


IRTerm IRTerm::jmp(Id id) {
	IRTerm term;
	term.tag = JMP;
	term.id = id;
	return term;
}

IRTerm IRTerm::jz(Loc r1, Id idT, Id idF) {
	IRTerm term;
	term.tag = JZ;
	term.r1 = r1;
	term.idT = idT;
	term.idF = idF;
	return term;
}

IRTerm IRTerm::ret() {
	IRTerm term;
	term.tag = RET;
	return term;
}

IRTerm IRTerm::unreach() {
	IRTerm term;
	term.tag = UNREACH;
	return term;
}

set<Loc> IRTerm::read() const {
	switch (tag) {
		case JMP: return {};
		case JZ: return {r1};
		case RET: return {};
		case UNREACH: return {};
		default: assert(false);
	}
}

set<Id> IRTerm::nexts() const {
	switch (tag) {
		case JMP: return {id};
		case JZ: return {idT, idF};
		case RET: return {};
		case UNREACH: return {};
		default: assert(false);
	}
}

void IRTerm::forEachRead(function<void(Loc&)> fr) {
	switch (tag) {
		case JMP: break;
		case JZ: fr(r1); break;
		case RET: break;
		case UNREACH: break;
		default: assert(false);
	}
}


Loc Loc::reg(int n) {
	Loc loc;
	loc.tag = REG;
	loc.n = n;
	return loc;
}

Loc Loc::arg(int n) {
	Loc loc;
	loc.tag = ARG;
	loc.n = n;
	return loc;
}

bool Loc::operator==(const Loc &other) const {
	return tag == other.tag && n == other.n;
}

bool Loc::operator!=(const Loc &other) const {
	return !(*this == other);
}

bool Loc::operator<(const Loc &other) const {
	if (tag < other.tag) return true;
	if (tag == other.tag && n < other.n) return true;
	return false;
}

namespace std {
	size_t hash<Loc>::operator()(const Loc &loc) const {
		return ((size_t)loc.tag << 60) | loc.n;
	}
}

bool isIRReg(const Loc &loc) {
	return loc.tag == Loc::REG && loc.n >= 0;
}

set<Loc> onlyIRRegs(const set<Loc> &locs) {
	set<Loc> res;
	for (const Loc &loc : locs) {
		if (isIRReg(loc)) res.insert(loc);
	}
	return res;
}


ostream& operator<<(ostream &os, const Loc &loc) {
	switch (loc.tag) {
		case Loc::REG:
			os << "r";
			if (loc.n == RRET) return os << "RET";
			if (loc.n == RLINK) return os << "LINK";
			if (loc.n == RSP) return os << "SP";
			if (loc.n == RPC) return os << "PC";
			return os << loc.n;

		case Loc::ARG:
			return os << "arg" << loc.n;

		default:
			assert(false);
	}
}

ostream& operator<<(ostream &os, const IRIns &ins) {
	switch (ins.tag) {
		case IRIns::NOP:
			return os << "nop";

		case IRIns::LI:
			return os << "li " << ins.rd << ", " << ins.number;

		case IRIns::MOV:
			return os << "mov " << ins.rd << ", " << ins.r1;

		case IRIns::STORE:
			return os << "s" << 8 * ins.size << " [" << ins.rd << "], " << ins.r1;

		case IRIns::LOAD:
			return os << "l" << 8 * ins.size << " " << ins.rd << ", [" << ins.r1 << "]";

		case IRIns::ARITH:
			switch (ins.op) {
				case Arith::ADD: os << "add"; break;
				case Arith::SUB: os << "sub"; break;
				case Arith::MUL: os << "mul"; break;
				case Arith::DIV: os << "div"; break;
				case Arith::LT: os << "lt"; break;
				case Arith::LTE: os << "lte"; break;
				case Arith::SLL: os << "sll"; break;
				case Arith::SLR: os << "slr"; break;
				case Arith::SAR: os << "sar"; break;
				case Arith::AND: os << "and"; break;
				case Arith::OR: os << "or"; break;
				case Arith::XOR: os << "xor"; break;
				default: assert(false);
			}
			return os << " " << ins.rd << ", " << ins.r1 << ", " << ins.r2;

		case IRIns::CALL:
			return os << "call " << ins.name;

		case IRIns::SEXT:
			return os << "sext" << ins.sizeto << "." << ins.sizefrom << " " << ins.rd << ", " << ins.r1;

		default:
			assert(false);
	}
}

ostream& operator<<(ostream &os, const IRTerm &term) {
	switch (term.tag) {
		case IRTerm::JMP:
			return os << "jmp " << term.id;

		case IRTerm::JZ:
			return os << "jz " << Loc(term.r1) << ", " << term.idT << ", " << term.idF;

		case IRTerm::RET:
			return os << "ret";

		case IRTerm::UNREACH:
			return os << "unreachable";

		default:
			assert(false);
	}
}

ostream& operator<<(ostream &os, const BB &bb) {
	os << "BB " << bb.id << ":" << endl;
	for (const IRIns &ins : bb.inss) {
		os << "  " << ins << endl;
	}
	return os << "  " << bb.term;
}

ostream& operator<<(ostream &os, const IFunc &ifunc) {
	os << "ifunc " << ifunc.name << ":";
	for (const auto &p : ifunc.BBs) {
		os << endl << p.second;
	}
	return os;
}

ostream& operator<<(ostream &os, const IR &ir) {
	os << "IR:";
	for (const auto &p : ir.funcs) {
		os << endl << p.second;
	}
	return os;
}

static void insertAtRet(
		IFunc &ifunc, Id bid, const vector<IRIns> &inss, set<Id> &seen) {

	seen.insert(bid);
	BB &bb = ifunc.BBs.find(bid)->second;

	if (bb.term.tag == IRTerm::RET) {
		bb.inss.insert(bb.inss.end(), inss.begin(), inss.end());
		return;
	}

	for (Id next : bb.term.nexts()) {
		if (seen.count(next) == 0) {
			insertAtRet(ifunc, next, inss, seen);
		}
	}
}

void insertAtRet(IFunc &ifunc, const vector<IRIns> &inss) {
	set<Id> seen;
	insertAtRet(ifunc, 0, inss, seen);
}
