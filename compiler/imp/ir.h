#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <set>
#include <functional>
#include "ast.h"

using namespace std;


// Valid blocks are always >=0
using Id = int;

const int RRET = -10;
const int RLINK = -11;
const int RSP = -12;
const int RPC = -13;

class Loc {
public:
	enum {
		REG,  // n
		ARG,  // n
	};

	int tag = -1;
	int n;

	Loc() = default;

	static Loc reg(int n);
	static Loc arg(int n);

	bool operator==(const Loc &other) const;
	bool operator!=(const Loc &other) const;
	// Nonsensical order, just for set<>
	bool operator<(const Loc &other) const;
};

ostream& operator<<(ostream &os, const Loc &loc);

namespace std {
	template <>
	struct hash<Loc> {
		size_t operator()(const Loc &loc) const;
	};
}

bool isIRReg(const Loc &loc);
set<Loc> onlyIRRegs(const set<Loc> &locs);

enum class Arith {
	ADD,
	SUB,
	MUL,
	DIV,
	LT,
	LTE,
	SLL,
	SLR,
	SAR,
};

class IRIns {
public:
	enum {
		NOP,
		LI,     // rd, number
		MOV,    // rd, r1
		STORE,  // [rd], r1, size
		LOAD,   // rd, [r1], size
		ARITH,  // op, rd, r1, r2
		CALL,   // name
		SEXT,   // rd, r1, sizeto, sizefrom
	};

	int tag = -1;
	Arith op;
	Loc rd, r1, r2;
	i64 number = 0;
	int size = -1;
	int sizeto = -1, sizefrom = -1;
	string name;

	static IRIns nop();
	static IRIns li(Loc rd, i64 number);
	static IRIns mov(Loc rd, Loc r1);
	static IRIns store(Loc rd, Loc r1, int size);
	static IRIns load(Loc rd, Loc r1, int size);
	static IRIns arith(Arith op, Loc rd, Loc r1, Loc r2);
	static IRIns call(const string &name);
	static IRIns signExtend(Loc rd, Loc r1, int sizeto, int sizefrom);

	set<Loc> written() const;
	set<Loc> read() const;
	void forEachRead(function<void(Loc&)> f);
	void forEachWrite(function<void(Loc&)> f);
};

class IRTerm {
public:
	enum {
		JMP,  // id
		JZ,  // r1, idT, idF
		RET,
		UNREACH,
	};

	int tag = -1;
	Id id = -1;
	Id idT = -1, idF = -1;
	Loc r1;

	static IRTerm jmp(Id id);
	static IRTerm jz(Loc r1, Id idT, Id idF);
	static IRTerm ret();
	static IRTerm unreach();

	set<Loc> read() const;
	set<Id> nexts() const;
	void forEachRead(function<void(Loc&)> fr);
};

class BB {
public:
	Id id;
	vector<IRIns> inss;
	IRTerm term;
};

class IFunc {
public:
	string name;
	unordered_map<Id, BB> BBs;
	vector<Id> bbOrder;
};

class IR {
public:
	unordered_map<string, IFunc> funcs;
};

// Inserts given instructions before every 'ret' terminator in the function.
void insertAtRet(IFunc &ifunc, const vector<IRIns> &inss);


ostream& operator<<(ostream &os, const IRIns&);
ostream& operator<<(ostream &os, const IRTerm&);
ostream& operator<<(ostream &os, const BB&);
ostream& operator<<(ostream &os, const IFunc&);
ostream& operator<<(ostream &os, const IR&);

IR buildIR(const Program &program);
