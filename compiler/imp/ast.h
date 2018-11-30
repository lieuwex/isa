#pragma once

#include <vector>
#include <string>
#include <memory>
#include <cstdint>

using namespace std;


using INT = int64_t;


class Type {
public:
	enum {
		INT,
		ARRAY,
	};

	int tag;
	unique_ptr<Type> contained;

	Type() = default;
	Type(int tag);
	Type(int tag, unique_ptr<Type> contained);

	int size() const;
};

class Decl {
public:
	string name;
	Type type;
};

class Expr {
public:
	enum {
		NUMBER,     // number
		VARIABLE,   // variable

		PLUS,       // e1, e2
		MINUS,      // e1, e2
		TIMES,      // e1, e2
		DIVIDE,     // e1, e2
		LESS,       // e1, e2
		LESSEQUAL,  // e1, e2
	};

	int tag;
	INT number;
	string variable;
	unique_ptr<Expr> e1, e2;

	Expr() = default;
	Expr(INT number);
	Expr(const string &variable);
	Expr(int tag, unique_ptr<Expr> e1, unique_ptr<Expr> e2);
};

class Stmt {
public:
	enum {
		DECL,    // decl, expr
		ASSIGN,  // decl, expr
		IF,      // expr, ch[0], ch[1]
		WHILE,   // expr, ch[0]
		DO,      // ch
		CALL,    // decl, name, args
	};

	int tag;
	Decl decl;
	Expr expr;
	vector<Stmt> ch;
	string name;
	vector<pair<Expr, Type>> args;
};

class Function {
public:
	string name;
	vector<Decl> args;
	Type ret;
	Stmt body;
};

class Program {
public:
	vector<Function> functions;
};
