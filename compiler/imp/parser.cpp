#include <stdexcept>
#include <cctype>
#include <cassert>
#include "parser.h"

using namespace std;


static void skipWhite(string_view &s) {
	s.remove_prefix(min(s.size(), s.find_first_not_of(" \n\t")));
}

static i64 svtoI(string_view &s) {
	skipWhite(s);
	if (s.size() == 0) throw invalid_argument("Cannot parse number");

	bool neg = false;
	if (s[0] == '-') {
		s.remove_prefix(1);
		neg = true;
	}

	size_t idx = s.find_first_not_of("0123456789");
	if (idx == string_view::npos) idx = s.size();

	string str(s.begin(), s.begin() + idx);
	s.remove_prefix(idx);

	return (neg ? -1 : 1) * stoll(str);
}

static SExpr goParse(string_view &s) {
	skipWhite(s);
	if (s.size() == 0) throw runtime_error("Expected s-expression");

	SExpr res;
	if (s[0] == '(') {
		s.remove_prefix(1);
		res.tag = SExpr::LIST;

		while (true) {
			skipWhite(s);
			if (s.size() > 0 && s[0] == ')') {
				s.remove_prefix(1);
				break;
			}
			res.list.push_back(goParse(s));
		}
	} else {
		try {
			res.number = svtoI(s);
			res.tag = SExpr::NUMBER;
		} catch (...) {
			size_t idx = s.find_first_of("() \n\t");
			if (idx == string_view::npos) idx = s.size();
			res.word = string(s.begin(), s.begin() + idx);
			res.tag = SExpr::WORD;
			s.remove_prefix(idx);
		}
	}

	return res;
}

SExpr SExpr::parse(string_view source) {
	return goParse(source);
}

SExpr::SExpr(i64 number)
		: tag(NUMBER), number(number) {}

SExpr::SExpr(const string &word)
		: tag(WORD), word(word) {}

SExpr::SExpr(const vector<SExpr> &list)
		: tag(LIST), list(list) {}

bool SExpr::matchList(const vector<SExpr> &l) const {
	if (tag != LIST) return false;
	if (list.size() < l.size()) return false;
	for (size_t i = 0; i < l.size(); i++) {
		if (l[i] != list[i]) return false;
	}
	return true;
}

bool SExpr::operator==(const SExpr &o) const {
	if (tag != o.tag) return false;
	switch (tag) {
		case NUMBER: return number == o.number;
		case WORD: return word == o.word;
		case LIST:
			if (list.size() != o.list.size()) return false;
			for (size_t i = 0; i < list.size(); i++) {
				if (list[i] != o.list[i]) return false;
			}
			return true;

		default:
			assert(false);
	}
}

bool SExpr::operator!=(const SExpr &o) const {
	return !(*this == o);
}

ostream& operator<<(ostream &os, const SExpr &s) {
	switch (s.tag) {
		case SExpr::NUMBER:
			return os << s.number;

		case SExpr::WORD:
			return os << s.word;

		case SExpr::LIST:
			os << '(';
			if (s.list.size() >= 1) os << s.list[0];
			for (size_t i = 1; i < s.list.size(); i++) os << ' ' << s.list[i];
			return os << ')';

		default:
			assert(false);
	}
}


static Type parseType(const SExpr &sexpr) {
	if (sexpr.tag == SExpr::WORD) {
		if (sexpr.word == "i8") return Type::makeInt(8);
		else if (sexpr.word == "i16") return Type::makeInt(16);
		else if (sexpr.word == "i32") return Type::makeInt(32);
		else if (sexpr.word == "i64") return Type::makeInt(64);
		else if (sexpr.word == "u8") return Type::makeUInt(8);
		else if (sexpr.word == "u16") return Type::makeUInt(16);
		else if (sexpr.word == "u32") return Type::makeUInt(32);
		else if (sexpr.word == "u64") return Type::makeUInt(64);
	} else if (sexpr.matchList({SExpr("array")})) {
		if (sexpr.list.size() != 2) throw runtime_error("Invalid array type");
		return Type::makeArray(parseType(sexpr.list[1]));
	}
	throw runtime_error("Unknown type");
}

static Decl parseDecl(const SExpr &sexpr) {
	if (sexpr.tag != SExpr::LIST ||
			sexpr.list.size() != 2 ||
			sexpr.list[0].tag != SExpr::WORD) {
		throw runtime_error("Invalid decl");
	}

	Decl decl;
	decl.name = sexpr.list[0].word;
	decl.type = parseType(sexpr.list[1]);
	return decl;
}

static Expr parseExpr(const SExpr &sexpr) {
	switch (sexpr.tag) {
		case SExpr::NUMBER:
			return Expr(sexpr.number);

		case SExpr::WORD:
			return Expr(sexpr.word);

		case SExpr::LIST:
			if (sexpr.list.size() == 3) {
				if (sexpr.list[0] == SExpr("cast")) {
					return Expr::makeCast(
							make_unique<Expr>(parseExpr(sexpr.list[2])),
							parseType(sexpr.list[1]));
				}

				int tag = -1;
				if (sexpr.list[0] == SExpr("+")) tag = Expr::PLUS;
				else if (sexpr.list[0] == SExpr("-")) tag = Expr::MINUS;
				else if (sexpr.list[0] == SExpr("*")) tag = Expr::TIMES;
				else if (sexpr.list[0] == SExpr("/")) tag = Expr::DIVIDE;
				else if (sexpr.list[0] == SExpr("<")) tag = Expr::LESS;
				else if (sexpr.list[0] == SExpr("<=")) tag = Expr::LESSEQUAL;

				if (tag != -1) {
					return Expr(tag,
							make_unique<Expr>(parseExpr(sexpr.list[1])),
							make_unique<Expr>(parseExpr(sexpr.list[2])));
				}
			}

			throw runtime_error("Invalid expr term");

		default:
			assert(false);
	}
}

static Stmt parseStmt(const SExpr &sexpr) {
	Stmt stmt;

	if (sexpr.matchList({SExpr("decl")})) {
		if (sexpr.list.size() != 3) throw runtime_error("Invalid var decl");
		stmt.tag = Stmt::DECL;
		stmt.decl = parseDecl(sexpr.list[1]);
		stmt.expr = parseExpr(sexpr.list[2]);
	} else if (sexpr.matchList({SExpr("asg")})) {
		if (sexpr.list.size() != 3 || sexpr.list[1].tag != SExpr::WORD) {
			throw runtime_error("Invalid assignment");
		}
		stmt.tag = Stmt::ASSIGN;
		stmt.target = sexpr.list[1].word;
		stmt.expr = parseExpr(sexpr.list[2]);
	} else if (sexpr.matchList({SExpr("if")})) {
		if (sexpr.list.size() != 4) throw runtime_error("Invalid if");
		stmt.tag = Stmt::IF;
		stmt.expr = parseExpr(sexpr.list[1]);
		stmt.ch.push_back(parseStmt(sexpr.list[2]));
		stmt.ch.push_back(parseStmt(sexpr.list[3]));
	} else if (sexpr.matchList({SExpr("while")})) {
		if (sexpr.list.size() != 3) throw runtime_error("Invalid while");
		stmt.tag = Stmt::WHILE;
		stmt.expr = parseExpr(sexpr.list[1]);
		stmt.ch.push_back(parseStmt(sexpr.list[2]));
	} else if (sexpr.matchList({SExpr("do")})) {
		stmt.tag = Stmt::DO;
		for (size_t i = 1; i < sexpr.list.size(); i++) {
			stmt.ch.push_back(parseStmt(sexpr.list[i]));
		}
	} else if (sexpr.matchList({SExpr("call")})) {
		if (sexpr.list.size() != 3 ||
				sexpr.list[1].tag != SExpr::WORD ||
				sexpr.list[2].tag != SExpr::LIST) {
			throw runtime_error("Invalid call");
		}
		stmt.tag = Stmt::CALL;
		stmt.name = sexpr.list[1].word;
		for (const SExpr &s : sexpr.list[2].list) {
			stmt.args.push_back(parseExpr(s));
		}
	} else if (sexpr.matchList({SExpr("callr")})) {
		if (sexpr.list.size() != 4 ||
				sexpr.list[1].tag != SExpr::WORD ||
				sexpr.list[2].tag != SExpr::WORD ||
				sexpr.list[3].tag != SExpr::LIST) {
			throw runtime_error("Invalid call");
		}
		stmt.tag = Stmt::CALLR;
		stmt.target = sexpr.list[1].word;
		stmt.name = sexpr.list[2].word;
		for (const SExpr &s : sexpr.list[3].list) {
			stmt.args.push_back(parseExpr(s));
		}
	} else if (sexpr.matchList({SExpr("return")})) {
		if (sexpr.list.size() != 2) throw runtime_error("Invalid return");
		stmt.tag = Stmt::RETURN;
		stmt.expr = parseExpr(sexpr.list[1]);
	} else {
		throw runtime_error("Invalid statement");
	}

	return stmt;
}

static Function parseFunction(const SExpr &sexpr) {
	if (!sexpr.matchList({SExpr("func")}) ||
			sexpr.list.size() != 5 ||
			sexpr.list[1].tag != SExpr::WORD ||
			sexpr.list[2].tag != SExpr::LIST) {
		throw runtime_error("Invalid function decl");
	}

	Function f;
	f.name = sexpr.list[1].word;
	for (const SExpr &s : sexpr.list[2].list) {
		f.args.push_back(parseDecl(s));
	}
	f.ret = parseType(sexpr.list[3]);
	f.body = parseStmt(sexpr.list[4]);
	return f;
}

Program parseProgram(const SExpr &sexpr) {
	if (!sexpr.matchList({SExpr("imp")})) throw runtime_error("Invalid program");

	Program program;
	for (size_t i = 1; i < sexpr.list.size(); i++) {
		program.functions.push_back(parseFunction(sexpr.list[i]));
	}

	return program;
}
