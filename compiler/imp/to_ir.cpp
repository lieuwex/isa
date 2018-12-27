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
	void buildStore(const Stmt &stmt, Id endbb);
	void buildIf(const Stmt &stmt, Id endbb);
	void buildWhile(const Stmt &stmt, Id endbb);
	void buildDo(const Stmt &stmt, Id endbb);
	void buildCall(const Stmt &stmt, Id endbb, bool hasRet);
	void buildReturn(const Stmt &stmt, Id endbb);
	void buildReturnX(const Stmt &stmt, Id endbb);
	void buildBreak(const Stmt &stmt, Id endbb);

	vector<unordered_map<string, Loc>> stk;
	vector<Id> loopStk;

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
	if (args.size() > 0) {
		Loc r8 = B.genReg();
		B.add(IRIns::li(r8, 8));

		for (int i = args.size() - 1; i >= 0; i--) {
			Id bb1 = B.newBB();
			Loc loc = build(args[i], bb1);

			B.switchBB(bb1);
			B.add(IRIns::arith(Arith::SUB, Loc::reg(RSP), Loc::reg(RSP), r8));
			B.add(IRIns::store(Loc::reg(RSP), loc, args[i].restype.size()));
		}
	}

	B.add(IRIns::call(name));

	if (args.size() > 0) {
		Loc sizereg = B.genReg();
		B.add(IRIns::li(sizereg, 8 * args.size()));
		B.add(IRIns::arith(Arith::ADD, Loc::reg(RSP), Loc::reg(RSP), sizereg));
	}

	B.setTerm(IRTerm::jmp(endbb));

	return Loc::reg(RRET);
}

void ToIR::build(const Stmt &stmt, Id endbb) {
	switch (stmt.tag) {
		case Stmt::DECL: buildDecl(stmt, endbb); break;
		case Stmt::ASSIGN: buildAssign(stmt, endbb); break;
		case Stmt::STORE: buildStore(stmt, endbb); break;
		case Stmt::IF: buildIf(stmt, endbb); break;
		case Stmt::WHILE: buildWhile(stmt, endbb); break;
		case Stmt::DO: buildDo(stmt, endbb); break;
		case Stmt::CALL: buildCall(stmt, endbb, false); break;
		case Stmt::CALLR: buildCall(stmt, endbb, true); break;
		case Stmt::RETURN: buildReturn(stmt, endbb); break;
		case Stmt::RETURNX: buildReturnX(stmt, endbb); break;
		case Stmt::BREAK: buildBreak(stmt, endbb); break;
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

void ToIR::buildStore(const Stmt &stmt, Id endbb) {
	Id bb1 = B.newBB();
	Loc loc = build(stmt.targetexpr, bb1);

	B.switchBB(bb1);
	Id bb2 = B.newBB();
	Loc eloc = build(stmt.expr, bb2);

	B.switchBB(bb2);
	B.add(IRIns::store(loc, eloc, stmt.expr.restype.size()));
	B.setTerm(IRTerm::jmp(endbb));
}

void ToIR::buildIf(const Stmt &stmt, Id endbb) {
	Id bb1 = B.newBB();
	Loc eloc = build(stmt.expr, bb1);

	B.switchBB(bb1);
	Id bbT = B.newBB();
	Id bbF = B.newBB();
	B.setTerm(IRTerm::jz(eloc, bbF, bbT));

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
	loopStk.push_back(endbb);
	build(stmt.ch[0], bbCond);
	loopStk.pop_back();
}

void ToIR::buildDo(const Stmt &stmt, Id endbb) {
	if (stmt.ch.size() == 0) {
		B.setTerm(IRTerm::jmp(endbb));
		return;
	}

	pushScope();

	for (size_t i = 0; i < stmt.ch.size() - 1; i++) {
		Id bb = B.newBB();
		build(stmt.ch[i], bb);
		B.switchBB(bb);
	}
	build(stmt.ch.back(), endbb);

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

void ToIR::buildReturnX(const Stmt&, Id) {
	B.setTerm(IRTerm::ret());
}

void ToIR::buildReturn(const Stmt &stmt, Id) {
	Id bb1 = B.newBB();
	Loc loc = build(stmt.expr, bb1);

	B.switchBB(bb1);
	B.add(IRIns::mov(Loc::reg(RRET), loc));
	B.setTerm(IRTerm::ret());
}

void ToIR::buildBreak(const Stmt&, Id) {
	B.setTerm(IRTerm::jmp(loopStk.back()));
}

static bool supportedUnsignedArith(const Expr &expr) {
	switch (expr.tag) {
		case Expr::PLUS:
		case Expr::MINUS:
		case Expr::EQUAL:
		case Expr::UNEQUAL:
		case Expr::BOOLAND:
		case Expr::BOOLOR:
			return true;

		default:
			return false;
	}
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
		case Expr::LESSEQUAL:
		case Expr::EQUAL:
		case Expr::UNEQUAL:
		case Expr::BOOLAND:
		case Expr::BOOLOR: {
			Id bb1 = B.newBB();
			Loc l1 = build(*expr.e1, bb1);

			B.switchBB(bb1);
			Id bb2 = B.newBB();
			Loc l2 = build(*expr.e2, bb2);

			assert(expr.e1->restype.tag == expr.e2->restype.tag);
			assert(expr.e1->restype.size() == expr.e2->restype.size());
			if (expr.e1->restype.tag == Type::UINT && !supportedUnsignedArith(expr)) {
				throw runtime_error("Unsupported arithmetic operation on unsigned operands");
			}

			B.switchBB(bb2);
			Loc resreg = B.genReg();;
			Arith op;
			switch (expr.tag) {
				case Expr::PLUS: op = Arith::ADD; goto native_arith_instr;
				case Expr::MINUS: op = Arith::SUB; goto native_arith_instr;
				case Expr::TIMES: op = Arith::MUL; goto native_arith_instr;
				case Expr::DIVIDE: op = Arith::DIV; goto native_arith_instr;
				case Expr::LESS: op = Arith::LT; goto native_arith_instr;
				case Expr::LESSEQUAL: op = Arith::LTE; goto native_arith_instr;
				case Expr::BOOLAND: op = Arith::AND; goto native_arith_instr;
				case Expr::BOOLOR: op = Arith::OR; goto native_arith_instr;
				native_arith_instr:
					B.add(IRIns::arith(op, resreg, l1, l2));
					break;

				case Expr::EQUAL: {
					Loc r1 = B.genReg(), r2 = B.genReg();
					B.add(IRIns::arith(Arith::LTE, r1, l1, l2));
					B.add(IRIns::arith(Arith::LTE, r2, l2, l1));
					B.add(IRIns::arith(Arith::AND, resreg, r1, r2));
					break;
				}

				case Expr::UNEQUAL: {
					Loc r1 = B.genReg(), r2 = B.genReg();
					B.add(IRIns::arith(Arith::LT, r1, l1, l2));
					B.add(IRIns::arith(Arith::LT, r2, l2, l1));
					B.add(IRIns::arith(Arith::OR, resreg, r1, r2));
					break;
				}

				default:
					assert(false);
			}

			B.setTerm(IRTerm::jmp(endbb));
			return resreg;
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

		case Expr::GET:
		case Expr::REF: {
			Id bb1 = B.newBB();
			Loc ptrloc = build(*expr.e1, bb1);

			B.switchBB(bb1);
			Id bb2 = B.newBB();
			Loc idxloc = build(*expr.e2, bb2);

			int size = expr.e1->restype.contained->size();

			B.switchBB(bb2);
			Loc multiplier = B.genReg();
			B.add(IRIns::li(multiplier, size));
			Loc offset = B.genReg();
			B.add(IRIns::arith(Arith::MUL, offset, idxloc, multiplier));
			Loc resptr = B.genReg();
			B.add(IRIns::arith(Arith::ADD, resptr, ptrloc, offset));

			Loc resloc;
			if (expr.tag == Expr::GET) {
				resloc = B.genReg();
				B.add(IRIns::load(resloc, resptr, size));
			} else {
				resloc = resptr;
			}

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
