#include <iostream>
#include <vector>
#include <unordered_map>
#include <stdexcept>
#include <cassert>
#include "typecheck.h"
#include "error.h"

using namespace std;


class TypeCheck {
public:
	~TypeCheck();

	void check(Program &program);
	void check(Function &func);
	void check(Stmt &stmt);
	void check(Expr &expr);

private:
	using FuncDesc = pair<Type, vector<Type>>;
	vector<unordered_map<string, Type>> stk;
	unordered_map<string, FuncDesc> functions;
	int loopDepth = 0;

	string currentFunction;

	void enterScope();
	void leaveScope();
	void addBinding(const Decl &decl);
	Type lookup(const string &name);  // returns tag==-1 if not found

	Type checkCall(const Site &site, const string &name, vector<Expr> &args);
};

TypeCheck::~TypeCheck() {
	assert(stk.size() == 0);
}

void TypeCheck::enterScope() {
	stk.emplace_back();
}

void TypeCheck::leaveScope() {
	stk.pop_back();
}

void TypeCheck::addBinding(const Decl &decl) {
	if (!stk.back().emplace(decl.name, decl.type).second) {
		throw TypeError(decl.site, "Variable already declared in scope");
	}
}

Type TypeCheck::lookup(const string &name) {
	for (int i = stk.size() - 1; i >= 0; i--) {
		auto it = stk[i].find(name);
		if (it != stk[i].end()) return it->second;
	}
	Type type;
	type.tag = -1;
	return type;
}

void TypeCheck::check(Program &program) {
	for (const Function &f : program.functions) {
		vector<Type> argtypes;
		for (const Decl &decl : f.args) argtypes.push_back(decl.type);
		if (!functions.emplace(f.name, make_pair(f.ret, argtypes)).second) {
			throw TypeError(f.site, "Duplicate function declaration");
		}
	}

	functions.emplace("putchar", make_pair(Type::makeInt(0), vector<Type>({Type::makeUInt(8)})));
	functions.emplace("getchar", make_pair(Type::makeInt(16), vector<Type>()));

	for (Function &f : program.functions) {
		check(f);
	}
}

void TypeCheck::check(Function &func) {
	enterScope();
	currentFunction = func.name;

	for (const Decl &decl : func.args) {
		addBinding(decl);
	}

	check(func.body);
	leaveScope();
}

void possiblyCoerceInt(Expr &expr, const Type &wanted) {
	if (expr.restype == wanted) return;
	if (expr.mintype.tag == wanted.tag &&
			(wanted.tag == Type::INT || wanted.tag == Type::UINT) &&
			expr.mintype.bits <= wanted.bits) {
		expr = Expr::makeConvert(make_unique<Expr>(move(expr)), wanted);
		expr.mintype = wanted;
		expr.restype = wanted;
	}
}

Type TypeCheck::checkCall(const Site &site, const string &name, vector<Expr> &args) {
	auto it = functions.find(name);
	if (it == functions.end()) {
		throw TypeError(site, "Call to undefined function");
	}

	const FuncDesc &descr = it->second;
	Type rettype = descr.first;

	if (args.size() != descr.second.size()) {
		throw TypeError(site, "Invalid number of arguments in function call");
	}

	for (size_t i = 0; i < args.size(); i++) {
		check(args[i]);
		possiblyCoerceInt(args[i], descr.second[i]);
		if (args[i].restype != descr.second[i]) {
			throw TypeError(site, "Invalid type in argument in function call");
		}
	}

	return rettype;
}

void TypeCheck::check(Stmt &stmt) {
	switch (stmt.tag) {
		case Stmt::DECL:
			check(stmt.expr);
			possiblyCoerceInt(stmt.expr, stmt.decl.type);
			if (stmt.expr.restype != stmt.decl.type) {
				throw TypeError(stmt.site, "Unequal types in variable initialisation");
			}
			addBinding(stmt.decl);
			break;

		case Stmt::ASSIGN: {
			Type type = lookup(stmt.target);
			if (type.tag == -1) {
				throw TypeError(stmt.site, "Assignment to undeclared variable");
			}
			check(stmt.expr);
			possiblyCoerceInt(stmt.expr, type);
			if (stmt.expr.restype != type) {
				throw TypeError(stmt.site, "Unequal types in variable assignment");
			}
			break;
		}

		case Stmt::STORE: {
			check(stmt.targetexpr);
			check(stmt.expr);
			assert(stmt.targetexpr.restype.tag == Type::PTR);
			possiblyCoerceInt(stmt.expr, *stmt.targetexpr.restype.contained);
			if (stmt.targetexpr.restype != Type::makePointer(stmt.expr.restype)) {
				throw TypeError(stmt.site, "Unequal types in storing assignment");
			}
			break;
		}

		case Stmt::IF:
			check(stmt.expr);
			possiblyCoerceInt(stmt.expr, Type::makeUInt(1));
			if (stmt.expr.restype != Type::makeUInt(1)) {
				throw TypeError(stmt.site, "Invalid type in if condition");
			}
			enterScope(); check(stmt.ch[0]); leaveScope();
			enterScope(); check(stmt.ch[1]); leaveScope();
			break;

		case Stmt::WHILE:
			check(stmt.expr);
			possiblyCoerceInt(stmt.expr, Type::makeUInt(1));
			if (stmt.expr.restype != Type::makeUInt(1)) {
				throw TypeError(stmt.site, "Invalid type in while condition");
			}
			loopDepth++;
			enterScope(); check(stmt.ch[0]); leaveScope();
			loopDepth--;
			break;

		case Stmt::DO:
			enterScope();
			for (Stmt &s : stmt.ch) check(s);
			leaveScope();
			break;

		case Stmt::CALL:
			checkCall(stmt.site, stmt.name, stmt.args);
			break;

		case Stmt::CALLR: {
			Type rettype = checkCall(stmt.site, stmt.name, stmt.args);
			Type vartype = lookup(stmt.target);
			if (vartype.tag == -1) {
				throw TypeError(stmt.site, "Call assignment to undeclared variable");
			}
			if (rettype != vartype) {
				throw TypeError(stmt.site, "Invalid types in call assignment");
			}
			break;
		}

		case Stmt::RETURN: {
			const FuncDesc &descr = functions.find(currentFunction)->second;
			check(stmt.expr);
			if (stmt.expr.restype != descr.first) {
				throw TypeError(stmt.site, "Invalid type in return statement");
			}
			break;
		}

		case Stmt::RETURNX: {
			const FuncDesc &descr = functions.find(currentFunction)->second;
			if (descr.first.tag != Type::VOID) {
				throw TypeError(stmt.site, "Cannot return void in non-void function");
			}
			break;
		}

		case Stmt::BREAK:
			if (loopDepth == 0) {
				throw TypeError(stmt.site, "Break statement outside loop");
			}
			break;

		case Stmt::DEBUG:
			break;

		default: assert(false);
	}
}

void TypeCheck::check(Expr &expr) {
	switch (expr.tag) {
		case Expr::NUMBER: {
			expr.restype = Type::makeInt(64);
			expr.mintype = Type::makeInt(1);
			break;
		}

		case Expr::VARIABLE: {
			Type type = lookup(expr.name);
			if (type.tag == -1) {
				throw TypeError(expr.site, "Use of undeclared variable");
			}
			Type largetype = type.growInt();
			if (largetype != type) {
				expr = Expr::makeConvert(make_unique<Expr>(move(expr)), largetype);
				expr.e1->mintype = type;
				expr.e1->restype = type;
			}
			expr.mintype = type;
			expr.restype = largetype;
			break;
		}

		case Expr::PLUS:
		case Expr::MINUS:
		case Expr::TIMES:
		case Expr::DIVIDE:
			check(*expr.e1);
			check(*expr.e2);
			if (expr.e1->restype.tag == Type::INT && expr.e2->restype.tag == Type::INT) {
				expr.restype = Type::makeInt(64);
				const auto mmintype = Type::maxType(expr.e1->mintype, expr.e2->mintype);
				if (mmintype) expr.mintype = *mmintype;
				else throw TypeError(expr.site, "Cannot compute maximum type of incompatible types");
			} else if (expr.e1->restype.tag == Type::UINT && expr.e2->restype.tag == Type::UINT) {
				expr.restype = Type::makeUInt(64);
				const auto mmintype = Type::maxType(expr.e1->mintype, expr.e2->mintype);
				if (mmintype) expr.mintype = *mmintype;
				else throw TypeError(expr.site, "Cannot compute maximum type of incompatible types");
			} else {
				throw TypeError(expr.site, "Invalid types in binary arithmetic operator");
			}

			if (expr.e1->restype != expr.restype) {
				assert(expr.e1->restype.bits <= expr.restype.bits);
				Expr e = Expr::makeConvert(move(expr.e1), expr.restype);
				expr.e1 = make_unique<Expr>(move(e));
				expr.e1->restype = expr.restype;
				expr.e1->mintype = expr.restype;
			}
			if (expr.e2->restype != expr.restype) {
				assert(expr.e2->restype.bits <= expr.restype.bits);
				Expr e = Expr::makeConvert(move(expr.e2), expr.restype);
				expr.e2 = make_unique<Expr>(move(e));
				expr.e2->restype = expr.restype;
				expr.e2->mintype = expr.restype;
			}
			break;

		case Expr::LESS:
		case Expr::LESSEQUAL: {
			check(*expr.e1);
			check(*expr.e2);
			const Type &t1 = expr.e1->restype;
			const Type &t2 = expr.e2->restype;
			if (t1 == t2 && t1.isIntegral()) {
				expr.restype = Type::makeUInt(1);
				expr.mintype = Type::makeUInt(1);
			} else {
				throw TypeError(expr.site, "Invalid types in binary comparison operator");
			}
			break;
		}

		case Expr::EQUAL:
		case Expr::UNEQUAL: {
			check(*expr.e1);
			check(*expr.e2);
			const Type &t1 = expr.e1->restype;
			const Type &t2 = expr.e2->restype;
			if (t1 == t2 && (t1.isIntegral() || t1.tag == Type::PTR)) {
				expr.restype = Type::makeUInt(1);
				expr.mintype = Type::makeUInt(1);
			} else {
				throw TypeError(expr.site, "Invalid types in binary equality operator");
			}
			break;
		}

		case Expr::BOOLAND:
		case Expr::BOOLOR: {
			check(*expr.e1);
			check(*expr.e2);
			const Type &t1 = expr.e1->restype;
			const Type &t2 = expr.e2->restype;
			if (t1 == Type::makeUInt(1) && t2 == Type::makeUInt(1)) {
				expr.restype = Type::makeUInt(1);
				expr.mintype = Type::makeUInt(1);
			} else {
				throw TypeError(expr.site, "Invalid types in binary boolean operator");
			}
			break;
		}

		case Expr::CAST:
			check(*expr.e1);
			if (expr.e1->restype == expr.type) {
				expr = move(*expr.e1);
			} else if (expr.type.isIntegral() && expr.e1->restype.isIntegral()) {
				expr = Expr::makeConvert(move(expr.e1), expr.type);
				expr.restype = expr.type;
				expr.mintype = expr.type;
			} else {
				throw TypeError(expr.site, "Invalid types in cast");
			}
			break;

		case Expr::PTRCAST: {
			check(*expr.e1);
			if (expr.type.tag != Type::PTR) {
				throw TypeError(expr.site, "Pointer cast to non-pointer type");
			}
			Type destType = expr.type;
			if (expr.e1->restype.isIntegral()) {
				expr = Expr::makeConvert(move(expr.e1), Type::makeInt(64));
				expr.restype = destType.growInt();
				expr.mintype = destType;
			} else if (expr.e1->restype.tag == Type::PTR) {
				Expr e = move(*expr.e1);
				expr = move(e);
				expr.restype = destType;
				expr.mintype = destType;
			} else {
				throw TypeError(expr.site, "Pointer cast from non-number-like type");
			}
			break;
		}

		case Expr::CALL: {
			Type rettype = checkCall(expr.site, expr.name, expr.args);
			expr.restype = rettype.growInt();
			expr.mintype = rettype;
			break;
		}

		case Expr::GET: {
			check(*expr.e1);
			check(*expr.e2);
			if (!expr.e2->restype.isIntegral()) {
				throw TypeError(expr.site, "Cannot index pointer with non-integer type");
			}
			if (expr.e1->restype.tag != Type::PTR) {
				throw TypeError(expr.site, "Cannot index non-pointer");
			}
			expr.restype = expr.e1->restype.contained->growInt();
			expr.mintype = *expr.e1->restype.contained;
			break;
		}

		case Expr::REF: {
			check(*expr.e1);
			check(*expr.e2);
			if (!expr.e2->restype.isIntegral()) {
				throw TypeError(expr.site, "Cannot ref-index pointer with non-integer type");
			}
			if (expr.e1->restype.tag != Type::PTR) {
				throw TypeError(expr.site, "Cannot ref-index non-pointer");
			}
			expr.restype = expr.e1->restype;
			expr.mintype = expr.e1->restype;
			break;
		}

		default: assert(false);
	}
}

void typecheck(Program &program) {
	TypeCheck().check(program);
}
