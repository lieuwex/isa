#pragma once

#include <vector>
#include <string>
#include <memory>
#include <optional>
#include <cstdint>

using namespace std;


using INT = int64_t;


class Type {
public:
	enum {
		INT_SIZED,  // bits
		INT,
		ARRAY,      // contained
	};

	int tag;
	int bits;
	Type *contained = nullptr;

	Type() = default;
	Type(const Type &other);
	Type(Type &&other);
	~Type();
	Type& operator=(const Type &other);
	Type& operator=(Type &&other);

	static Type makeInt();
	static Type makeIntSized(int bits);
	static Type makeArray(const Type &contained);

	int size() const;
	bool isIntegral() const;

	bool operator==(const Type &other) const;
	bool operator!=(const Type &other) const;
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

	// The type of the expression, annotated by the type checker
	Type restype;

	Expr() = default;
	Expr(INT number);
	Expr(const string &variable);
	Expr(int tag, unique_ptr<Expr> e1, unique_ptr<Expr> e2);
};

class Stmt {
public:
	enum {
		DECL,    // decl, expr
		ASSIGN,  // target, expr
		IF,      // expr, ch[0], ch[1]
		WHILE,   // expr, ch[0]
		DO,      // ch
		CALL,    // name, args
		CALLR,   // target, name, args
		RETURN,  // expr
	};

	int tag;
	Decl decl;
	Expr expr;
	vector<Stmt> ch;
	string name, target;
	vector<Expr> args;
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
