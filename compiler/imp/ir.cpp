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

IRIns IRIns::li(Loc rd, INT number) {
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

set<Loc> IRIns::written() const {
	switch (tag) {
		case NOP: return {};
		case LI: return {rd};
		case MOV: return {rd};
		case STORE: return {};
		case LOAD: return {rd};
		case ARITH: return {rd};
		case CALL: return {};
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
				default: assert(false);
			}
			return os << " " << ins.rd << ", " << ins.r1 << ", " << ins.r2;

		case IRIns::CALL:
			return os << "call " << ins.name;

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


class Build {
public:
	Build(IRBuilder &B);

	void build(const Function &function);

private:
	void pushScope();
	void popScope();
	Loc lookup(const string &name);
	void setLoc(const string &name, Loc loc);

	void build(const Stmt &stmt, Id endbb);
	Loc build(const Expr &expr, Id endbb);  // returns result reg
	void buildDecl(const Stmt &stmt, Id endbb);
	void buildAssign(const Stmt &stmt, Id endbb);
	void buildIf(const Stmt &stmt, Id endbb);
	void buildWhile(const Stmt &stmt, Id endbb);
	void buildDo(const Stmt &stmt, Id endbb);
	void buildCall(const Stmt &stmt, Id endbb, bool hasRet);
	void buildReturn(const Stmt &stmt, Id endbb);

	vector<unordered_map<string, Loc>> stk;

	IRBuilder &B;
};

Build::Build(IRBuilder &B)
		: B(B) {}

void Build::pushScope() {
	stk.emplace_back();
}

void Build::popScope() {
	stk.pop_back();
}

Loc Build::lookup(const string &name) {
	for (int i = stk.size() - 1; i >= 0; i--) {
		auto it = stk[i].find(name);
		if (it != stk[i].end()) return it->second;
	}
	return Loc();
}

void Build::setLoc(const string &name, Loc loc) {
	stk.back().emplace(name, loc);
}

void Build::build(const Function &function) {
	pushScope();

	for (size_t i = 0; i < function.args.size(); i++) {
		Loc reg = B.genReg();
		B.add(IRIns::load(reg, Loc::arg(i), 8));  // TODO: sizes?
		setLoc(function.args[i].name, reg);
	}

	int bb1 = B.newBB();
	build(function.body, bb1);

	B.switchBB(bb1);
	B.setTerm(IRTerm::ret());

	popScope();
}

void Build::build(const Stmt &stmt, Id endbb) {
	switch (stmt.tag) {
		case Stmt::DECL: buildDecl(stmt, endbb); break;
		case Stmt::ASSIGN: buildAssign(stmt, endbb); break;
		case Stmt::IF: buildIf(stmt, endbb); break;
		case Stmt::WHILE: buildWhile(stmt, endbb); break;
		case Stmt::DO: buildDo(stmt, endbb); break;
		case Stmt::CALL: buildCall(stmt, endbb, false); break;
		case Stmt::CALLR: buildCall(stmt, endbb, true); break;
		case Stmt::RETURN: buildReturn(stmt, endbb); break;
		default: assert(false);
	}
}

void Build::buildDecl(const Stmt &stmt, Id endbb) {
	Loc loc = build(stmt.expr, endbb);
	setLoc(stmt.decl.name, loc);
}

void Build::buildAssign(const Stmt &stmt, Id endbb) {
	Loc loc = lookup(stmt.decl.name);
	if (loc.tag == -1) throw runtime_error("Assignment to undeclared variable");

	Loc eloc = build(stmt.expr, endbb);
	B.switchBB(endbb);
	B.add(IRIns::mov(loc, eloc));
}

void Build::buildIf(const Stmt &stmt, Id endbb) {
	Id bb1 = B.newBB();
	Loc eloc = build(stmt.expr, bb1);

	B.switchBB(bb1);
	Id bbT = B.newBB();
	Id bbF = B.newBB();
	B.setTerm(IRTerm::jz(eloc, bbT, bbF));

	B.switchBB(bbT);
	build(stmt.ch[0], endbb);

	B.switchBB(bbF);
	build(stmt.ch[1], endbb);
}

void Build::buildWhile(const Stmt &stmt, Id endbb) {
	Id bbCond = B.newBB();
	Id bbTest = B.newBB();
	Id bbBody = B.newBB();

	B.setTerm(IRTerm::jmp(bbCond));

	B.switchBB(bbCond);
	Loc eloc = build(stmt.expr, bbTest);

	B.switchBB(bbTest);
	B.setTerm(IRTerm::jz(eloc, endbb, bbBody));

	B.switchBB(bbBody);
	build(stmt.ch[0], bbCond);
}

void Build::buildDo(const Stmt &stmt, Id endbb) {
	pushScope();

	for (size_t i = 0; i < stmt.ch.size() - 1; i++) {
		Id bb = B.newBB();
		build(stmt.ch[i], bb);
		B.switchBB(bb);
	}
	if (stmt.ch.size() > 0) build(stmt.ch.back(), endbb);

	popScope();
}

void Build::buildCall(const Stmt &stmt, Id endbb, bool hasRet) {
	INT sizesum = 0;
	for (int i = stmt.args.size() - 1; i >= 0; i--) {
		const Expr &expr = stmt.args[i].first;
		const Type &type = stmt.args[i].second;

		Id bb1 = B.newBB();
		Loc loc = build(expr, bb1);

		B.switchBB(bb1);
		Loc roff = B.genReg();
		B.add(IRIns::li(roff, type.size()));
		B.add(IRIns::arith(Arith::SUB, Loc::reg(RSP), Loc::reg(RSP), roff));
		B.add(IRIns::store(Loc::reg(RSP), loc, type.size()));

		sizesum += type.size();
	}

	B.add(IRIns::call(stmt.name));

	Loc sizereg = B.genReg();
	B.add(IRIns::li(sizereg, sizesum));
	B.add(IRIns::arith(Arith::ADD, Loc::reg(RSP), Loc::reg(RSP), sizereg));

	if (hasRet) {
		Loc loc = lookup(stmt.decl.name);
		if (loc.tag == -1) throw runtime_error("Call asg to undefined variable");

		B.add(IRIns::mov(loc, Loc::reg(RRET)));
	}

	B.setTerm(IRTerm::jmp(endbb));
}

void Build::buildReturn(const Stmt &stmt, Id) {
	Id bb1 = B.newBB();
	Loc loc = build(stmt.expr, bb1);

	B.switchBB(bb1);
	B.add(IRIns::mov(Loc::reg(RRET), loc));
	B.setTerm(IRTerm::ret());
}

Loc Build::build(const Expr &expr, Id endbb) {
	switch (expr.tag) {
		case Expr::NUMBER: {
			Loc reg = B.genReg();
			B.add(IRIns::li(reg, expr.number));
			B.setTerm(IRTerm::jmp(endbb));
			return reg;
		}

		case Expr::VARIABLE: {
			Loc loc = lookup(expr.variable);
			if (loc.tag == -1) throw runtime_error("Use of undeclared variable");
			B.setTerm(IRTerm::jmp(endbb));
			return loc;
		}

		case Expr::PLUS:
		case Expr::MINUS:
		case Expr::TIMES:
		case Expr::DIVIDE:
		case Expr::LESS:
		case Expr::LESSEQUAL: {
			Id bb1 = B.newBB();
			Loc l1 = build(*expr.e1, bb1);

			B.switchBB(bb1);
			Id bb2 = B.newBB();
			Loc l2 = build(*expr.e2, bb2);

			B.switchBB(bb2);
			Arith op;
			switch (expr.tag) {
				case Expr::PLUS: op = Arith::ADD; break;
				case Expr::MINUS: op = Arith::SUB; break;
				case Expr::TIMES: op = Arith::MUL; break;
				case Expr::DIVIDE: op = Arith::DIV; break;
				case Expr::LESS: op = Arith::LT; break;
				case Expr::LESSEQUAL: op = Arith::LTE; break;
				default: assert(false);
			}

			Loc reg = B.genReg();
			B.add(IRIns::arith(op, reg, l1, l2));
			B.setTerm(IRTerm::jmp(endbb));
			return reg;
		}

		default:
			assert(false);
	}
}

IR buildIR(const Program &program) {
	IR ir;

	for (const Function &func : program.functions) {
		IRBuilder B;
		assert(B.newBB() == 0);
		B.switchBB(0);

		Build(B).build(func);

		IFunc ifunc = B.finalise();
		ifunc.name = func.name;
		ir.funcs.emplace(func.name, move(ifunc));
	}

	return ir;
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
