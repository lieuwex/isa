#include <cassert>
#include "to_ir.h"
#include "ir_builder.h"

using namespace std;


class ToIR {
public:
	ToIR(IRBuilder &B);

	void build(const Function &function);

private:
	void pushScope();
	void popScope();
	Loc lookup(const string &name);
	void setLoc(const string &name, Loc loc);

	void build(const Stmt &stmt, Id endbb);
	Loc build(const Expr &expr, Id endbb);  // returns result reg

	Loc buildCallGeneric(const string &name, const vector<Expr> &args, Id endbb);  // returns result reg

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

ToIR::ToIR(IRBuilder &B)
		: B(B) {}

void ToIR::pushScope() {
	stk.emplace_back();
}

void ToIR::popScope() {
	stk.pop_back();
}

Loc ToIR::lookup(const string &name) {
	for (int i = stk.size() - 1; i >= 0; i--) {
		auto it = stk[i].find(name);
		if (it != stk[i].end()) return it->second;
	}
	return Loc();
}

void ToIR::setLoc(const string &name, Loc loc) {
	stk.back().emplace(name, loc);
}

void ToIR::build(const Function &function) {
	pushScope();

	for (size_t i = 0; i < function.args.size(); i++) {
		Loc reg = B.genReg();
		B.add(IRIns::load(reg, Loc::arg(i), function.args[i].type.size()));
		setLoc(function.args[i].name, reg);
	}

	int bb1 = B.newBB();
	build(function.body, bb1);

	B.switchBB(bb1);
	B.setTerm(IRTerm::ret());

	popScope();
}

Loc ToIR::buildCallGeneric(const string &name, const vector<Expr> &args, Id endbb) {
	Loc r8 = B.genReg();
	B.add(IRIns::li(r8, 8));

	for (int i = args.size() - 1; i >= 0; i--) {
		Id bb1 = B.newBB();
		Loc loc = build(args[i], bb1);

		B.switchBB(bb1);
		B.add(IRIns::arith(Arith::SUB, Loc::reg(RSP), Loc::reg(RSP), r8));
		B.add(IRIns::store(Loc::reg(RSP), loc, args[i].restype.size()));
	}

	B.add(IRIns::call(name));

	Loc sizereg = B.genReg();
	B.add(IRIns::li(sizereg, 8 * args.size()));
	B.add(IRIns::arith(Arith::ADD, Loc::reg(RSP), Loc::reg(RSP), sizereg));

	B.setTerm(IRTerm::jmp(endbb));

	return Loc::reg(RRET);
}

void ToIR::build(const Stmt &stmt, Id endbb) {
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

void ToIR::buildDecl(const Stmt &stmt, Id endbb) {
	Loc loc = build(stmt.expr, endbb);
	setLoc(stmt.decl.name, loc);
}

void ToIR::buildAssign(const Stmt &stmt, Id endbb) {
	Loc loc = lookup(stmt.target);
	if (loc.tag == -1) throw runtime_error("Assignment to undeclared variable");

	Id bb1 = B.newBB();
	Loc eloc = build(stmt.expr, bb1);

	B.switchBB(bb1);
	B.add(IRIns::mov(loc, eloc));
	B.setTerm(IRTerm::jmp(endbb));
}

void ToIR::buildIf(const Stmt &stmt, Id endbb) {
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

void ToIR::buildWhile(const Stmt &stmt, Id endbb) {
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

void ToIR::buildDo(const Stmt &stmt, Id endbb) {
	pushScope();

	for (size_t i = 0; i < stmt.ch.size() - 1; i++) {
		Id bb = B.newBB();
		build(stmt.ch[i], bb);
		B.switchBB(bb);
	}
	if (stmt.ch.size() > 0) build(stmt.ch.back(), endbb);

	popScope();
}

void ToIR::buildCall(const Stmt &stmt, Id endbb, bool hasRet) {
	Id bb1 = B.newBB();
	Loc retreg = buildCallGeneric(stmt.name, stmt.args, bb1);

	B.switchBB(bb1);

	if (hasRet) {
		Loc loc = lookup(stmt.target);
		if (loc.tag == -1) throw runtime_error("Call asg to undefined variable");

		B.add(IRIns::mov(loc, retreg));
	}

	B.setTerm(IRTerm::jmp(endbb));
}

void ToIR::buildReturn(const Stmt &stmt, Id) {
	Id bb1 = B.newBB();
	Loc loc = build(stmt.expr, bb1);

	B.switchBB(bb1);
	B.add(IRIns::mov(Loc::reg(RRET), loc));
	B.setTerm(IRTerm::ret());
}

Loc ToIR::build(const Expr &expr, Id endbb) {
	switch (expr.tag) {
		case Expr::NUMBER: {
			Loc reg = B.genReg();
			B.add(IRIns::li(reg, expr.number));
			B.setTerm(IRTerm::jmp(endbb));
			return reg;
		}

		case Expr::VARIABLE: {
			Loc loc = lookup(expr.name);
			if (loc.tag == -1) throw runtime_error("IR: Use of undeclared variable");
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

			assert(expr.e1->restype.tag == expr.e2->restype.tag);
			if (expr.e1->restype.tag == Type::UINT) {
				if (expr.tag != Expr::PLUS && expr.tag != Expr::MINUS) {
					throw runtime_error("Unsupported arithmetic operation on unsigned operands");
				}
			}

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

		case Expr::CONVERT: {
			Id bb1 = B.newBB();
			Loc loc = build(*expr.e1, bb1);
			B.switchBB(bb1);
			Loc resloc;

			if (expr.type == expr.e1->restype) {
				resloc = loc;
			} else if (expr.type.isIntegral() && expr.e1->restype.isIntegral()) {
				if (expr.type.tag == Type::INT && expr.e1->restype.tag == Type::INT &&
						expr.type.bits >= expr.e1->restype.bits) {
					resloc = B.genReg();
					B.add(IRIns::signExtend(resloc, loc, expr.type.bits, expr.e1->restype.bits));
				} else {
					resloc = loc;
				}
			} else {
				throw runtime_error("Cannot convert between incompatible types");
			}

			B.setTerm(IRTerm::jmp(endbb));
			return resloc;
		}

		case Expr::CALL: {
			Id bb1 = B.newBB();
			Loc retreg = buildCallGeneric(expr.name, expr.args, bb1);

			Loc resloc = B.genReg();
			B.switchBB(bb1);
			B.add(IRIns::mov(resloc, retreg));
			B.setTerm(IRTerm::jmp(endbb));
			return resloc;
		}

		default:
			assert(false);
	}
}

IR toIR(const Program &program) {
	IR ir;

	for (const Function &func : program.functions) {
		IRBuilder B;
		assert(B.newBB() == 0);
		B.switchBB(0);

		ToIR(B).build(func);

		IFunc ifunc = B.finalise();
		ifunc.name = func.name;
		ir.funcs.emplace(func.name, move(ifunc));
	}

	return ir;
}
