#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <memory>
#include <optional>
#include <cstdint>

using namespace std;


using i64 = int64_t;


class Type {
public:
	enum {
		INT,    // bits
		UINT,   // bits
		PTR,    // contained
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

	static Type makeInt(int bits);
	static Type makeUInt(int bits);
	static Type makePointer(const Type &contained);
	static Type maxType(const Type &a, const Type &b);

	bool isIntegral() const;
	int size() const;
	Type growInt() const;

	bool operator==(const Type &other) const;
	bool operator!=(const Type &other) const;

	void writeProlog(ostream &os) const;
};

class Decl {
public:
	string name;
	Type type;

	void writeProlog(ostream &os) const;
};

class Expr {
public:
	enum {
		NUMBER,     // number
		VARIABLE,   // name

		PLUS,       // e1, e2
		MINUS,      // e1, e2
		TIMES,      // e1, e2
		DIVIDE,     // e1, e2
		LESS,       // e1, e2
		LESSEQUAL,  // e1, e2
		EQUAL,      // e1, e2
		UNEQUAL,    // e1, e2
		BOOLAND,    // e1, e2
		BOOLOR,     // e1, e2
		CAST,       // e1, type
		PTRCAST,    // e1, type
		CALL,       // name, args
		GET,        // e1, e2
		REF,        // e1, e2

		CONVERT,    // e1, type
	};

	int tag;
	i64 number;
	string name;
	unique_ptr<Expr> e1, e2;
	vector<Expr> args;
	Type type;

	// The type of the expression, annotated by the type checker
	Type restype;
	// The smallest type necessary to store the result, annotated and used
	// internally by the type checker
	Type mintype;

	Expr() = default;
	Expr(i64 number);
	Expr(const string &name);
	Expr(int tag, unique_ptr<Expr> &&e1, unique_ptr<Expr> &&e2);

	static Expr makeCast(unique_ptr<Expr> &&e1, const Type &type);
	static Expr makePtrCast(unique_ptr<Expr> &&e1, const Type &type);
	static Expr makeCall(const string &name, vector<Expr> &&args);
	static Expr makeConvert(unique_ptr<Expr> &&e1, const Type &type);

	void writeProlog(ostream &os) const;
};

class Stmt {
public:
	enum {
		DECL,    // decl, expr
		ASSIGN,  // target, expr
		STORE,   // targetexpr, expr
		IF,      // expr, ch[0], ch[1]
		WHILE,   // expr, ch[0]
		DO,      // ch
		CALL,    // name, args
		CALLR,   // target, name, args
		RETURN,  // expr
	};

	int tag;
	Decl decl;
	Expr targetexpr, expr;
	vector<Stmt> ch;
	string name, target;
	vector<Expr> args;

	void writeProlog(ostream &os) const;
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
