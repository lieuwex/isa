#pragma once

#include <iostream>
#include <string>
#include <string_view>
#include <cstdint>
#include "ast.h"

using namespace std;


class SExpr {
public:
	enum {
		WORD,    // word
		NUMBER,  // number
		LIST,    // list
	};

	int tag;
	string word;
	INT number;
	vector<SExpr> list;

	static SExpr parse(string_view source);

	SExpr() = default;
	SExpr(INT number);
	SExpr(const string &word);
	SExpr(const vector<SExpr> &list);

	bool matchList(const vector<SExpr> &l) const;

	bool operator==(const SExpr &o) const;
	bool operator!=(const SExpr &o) const;
};

ostream& operator<<(ostream &os, const SExpr &s);

Program parseProgram(const SExpr &sexpr);
