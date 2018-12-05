#include <iostream>
#include <vector>
#include <unordered_map>
#include <stdexcept>
#include <cassert>
#include "typecheck.h"

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

	string currentFunction;

	void enterScope();
	void leaveScope();
	void addBinding(const string &name, const Type &type);
	Type lookup(const string &name);  // returns tag==-1 if not found
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

void TypeCheck::addBinding(const string &name, const Type &type) {
	if (!stk.back().emplace(name, type).second) {
		throw runtime_error("Variable already declared in scope");
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
			throw runtime_error("Duplicate function declaration");
		}
	}

	functions.emplace("putchar", make_pair(Type::makeInt(0), vector<Type>({Type::makeInt(8)})));

	for (Function &f : program.functions) {
		check(f);
	}
}

void TypeCheck::check(Function &func) {
	enterScope();
	currentFunction = func.name;
	check(func.body);
	leaveScope();
}

void TypeCheck::check(Stmt &stmt) {
	switch (stmt.tag) {
		case Stmt::DECL:
			check(stmt.expr);
			if (stmt.expr.restype != stmt.decl.type) {
				throw runtime_error("Unequal types in variable initialisation");
			}
			addBinding(stmt.decl.name, stmt.decl.type);
			break;

		case Stmt::ASSIGN: {
			Type type = lookup(stmt.target);
			if (type.tag == -1) {
				throw runtime_error("Assignment to undeclared variable");
			}
			check(stmt.expr);
			if (stmt.expr.restype != type) {
				throw runtime_error("Unequal types in variable assignment");
			}
			break;
		}

		case Stmt::IF:
			check(stmt.expr);
			if (!stmt.expr.restype.isIntegral()) {
				throw runtime_error("Invalid type in if condition");
			}
			enterScope(); check(stmt.ch[0]); leaveScope();
			enterScope(); check(stmt.ch[1]); leaveScope();
			break;

		case Stmt::WHILE:
			check(stmt.expr);
			if (!stmt.expr.restype.isIntegral()) {
				throw runtime_error("Invalid type in while condition");
			}
			enterScope(); check(stmt.ch[0]); leaveScope();
			break;

		case Stmt::DO:
			enterScope();
			for (Stmt &s : stmt.ch) check(s);
			leaveScope();
			break;

		case Stmt::CALL:
		case Stmt::CALLR: {
			auto it = functions.find(stmt.name);
			if (it == functions.end()) {
				throw runtime_error("Call to undefined function");
			}
			if (stmt.tag == Stmt::CALLR) {
				Type type = lookup(stmt.target);
				if (type.tag == -1) {
					throw runtime_error("Call assignment to undeclared variable");
				}
				if (it->second.first != type) {
					throw runtime_error("Invalid types in call assignment");
				}
			}
			const FuncDesc &descr = it->second;
			if (stmt.args.size() != descr.second.size()) {
				throw runtime_error("Invalid number of arguments in function call");
			}
			for (size_t i = 0; i < stmt.args.size(); i++) {
				check(stmt.args[i]);
				if (stmt.args[i].restype != descr.second[i]) {
					throw runtime_error("Invalid type in argument in function call");
				}
			}
			break;
		}

		case Stmt::RETURN: {
			const FuncDesc &descr = functions.find(currentFunction)->second;
			check(stmt.expr);
			if (stmt.expr.restype != descr.first) {
				throw runtime_error("Invalid type in return statement");
			}
			break;
		}

		default: assert(false);
	}
}

void TypeCheck::check(Expr &expr) {
	switch (expr.tag) {
		case Expr::NUMBER:
			expr.restype = Type::makeInt();
			break;

		case Expr::VARIABLE: {
			Type type = lookup(expr.variable);
			if (type.tag == -1) {
				throw runtime_error("Use of undeclared variable");
			}
			expr.restype = type;
			break;
		}

		case Expr::PLUS:
		case Expr::MINUS:
		case Expr::TIMES:
		case Expr::DIVIDE:
		case Expr::LESS:
		case Expr::LESSEQUAL: {
			check(*expr.e1);
			check(*expr.e2);
			if (expr.e1->restype.isIntegral() && expr.e2->restype.isIntegral()) {
				expr.restype = Type::makeInt();
			} else {
				throw runtime_error("Invalid types in binary operator");
			}
			break;
		}

		default: assert(false);
	}
}

void typecheck(Program &program) {
	TypeCheck().check(program);
}
