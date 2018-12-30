#include <stdexcept>
#include <cctype>
#include <cassert>
#include "parser.h"
#include "error.h"

using namespace std;


class SourceView {
public:
	SourceView(string_view sv, const string &fname);

	void skip(size_t num);
	string substr(size_t from, size_t len) const;
	size_t remaining() const;
	size_t find(char ch) const;
	size_t find_first_of(const char *set) const;
	size_t find_first_not_of(const char *set) const;
	Site site() const;
	char operator[](size_t idx) const;

	static const size_t npos = string_view::npos;

private:
	string_view sv;
	Site siteval;
};

SourceView::SourceView(string_view sv, const string &fname)
		: sv(sv), siteval(fname, 1, 1) {}

void SourceView::skip(size_t num) {
	num = min(num, remaining());

	for (size_t i = 0; i < num; i++) {
		if (sv[i] == '\n') {
			siteval.line++;
			siteval.col = 1;
		} else {
			siteval.col++;
		}
	}

	sv.remove_prefix(num);
}

string SourceView::substr(size_t from, size_t len) const {
	return string(sv.begin() + from, sv.begin() + from + len);
}

size_t SourceView::remaining() const {
	return sv.size();
}

size_t SourceView::find(char ch) const {
	return sv.find(ch);
}

size_t SourceView::find_first_of(const char *set) const {
	return sv.find_first_of(set);
}

size_t SourceView::find_first_not_of(const char *set) const {
	return sv.find_first_not_of(set);
}

Site SourceView::site() const {
	return siteval;
}

char SourceView::operator[](size_t idx) const {
	return sv[idx];
}


static void skipWhite(SourceView &s) {
	s.skip(min(s.remaining(), s.find_first_not_of(" \n\t")));
}

static void skipWhiteComment(SourceView &s) {
	while (true) {
		skipWhite(s);
		if (s.remaining() == 0 || s[0] != ';') break;
		s.skip(min(s.remaining(), s.find('\n')));
	}
}

static i64 svtoI(SourceView &s) {
	skipWhite(s);
	if (s.remaining() == 0) throw ParseError(s.site(), "Cannot parse number");

	bool neg = false;
	if (s[0] == '-') {
		if (s.remaining() == 1 || !isdigit(s[1])) throw ParseError(s.site(), "Cannot parse number");
		s.skip(1);
		neg = true;
	}

	size_t idx = s.find_first_not_of("0123456789");
	if (idx == SourceView::npos) idx = s.remaining();

	string str = s.substr(0, idx);
	s.skip(idx);

	return (neg ? -1 : 1) * stoll(str);
}

static SExpr goParse(const string &fname, SourceView &s) {
	skipWhiteComment(s);
	if (s.remaining() == 0) {
		throw ParseError(s.site(), "Expected s-expression: maybe unterminated s-list");
	}

	SExpr res;
	if (s[0] == '(') {
		res.site = s.site();
		s.skip(1);
		res.tag = SExpr::LIST;

		while (true) {
			skipWhiteComment(s);
			if (s.remaining() > 0 && s[0] == ')') {
				s.skip(1);
				break;
			}
			res.list.push_back(goParse(fname, s));
		}
	} else {
		res.site = s.site();
		try {
			res.number = svtoI(s);
			res.tag = SExpr::NUMBER;
		} catch (...) {
			size_t idx = min(s.remaining(), s.find_first_of("() \n\t"));
			res.word = s.substr(0, idx);
			res.tag = SExpr::WORD;
			s.skip(idx);
		}
	}

	return res;
}

SExpr SExpr::parse(const string &fname, string_view source) {
	SourceView sv(source, fname);
	return goParse(fname, sv);
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
		Type res;
		if (sexpr.word == "i8") res = Type::makeInt(8);
		else if (sexpr.word == "i16") res = Type::makeInt(16);
		else if (sexpr.word == "i32") res = Type::makeInt(32);
		else if (sexpr.word == "i64") res = Type::makeInt(64);
		else if (sexpr.word == "u8") res = Type::makeUInt(8);
		else if (sexpr.word == "u16") res = Type::makeUInt(16);
		else if (sexpr.word == "u32") res = Type::makeUInt(32);
		else if (sexpr.word == "u64") res = Type::makeUInt(64);
		else throw ParseError(sexpr.site, "Unknown type name");
		res.site = sexpr.site;
		return res;
	} else if (sexpr.matchList({SExpr("ptr")})) {
		if (sexpr.list.size() != 2) throw ParseError(sexpr.site, "Invalid pointer type");
		Type res = Type::makePointer(parseType(sexpr.list[1]));
		res.site = sexpr.site;
		return res;
	} else if (sexpr.tag == SExpr::LIST && sexpr.list.size() == 0) {
		Type res = Type::makeVoid();
		res.site = sexpr.site;
		return res;
	}
	throw ParseError(sexpr.site, "Unknown type");
}

static Decl parseDecl(const SExpr &sexpr) {
	if (sexpr.tag != SExpr::LIST ||
			sexpr.list.size() != 2 ||
			sexpr.list[0].tag != SExpr::WORD) {
		throw ParseError(sexpr.site, "Invalid decl");
	}

	Decl decl;
	decl.name = sexpr.list[0].word;
	decl.type = parseType(sexpr.list[1]);
	decl.site = sexpr.site;
	return decl;
}

static Expr parseExpr(const SExpr &sexpr) {
	switch (sexpr.tag) {
		case SExpr::NUMBER: {
			Expr e = Expr(sexpr.number);
			e.site = {sexpr.site};
			return e;
		}

		case SExpr::WORD: {
			Expr e = Expr(sexpr.word);
			e.site = {sexpr.site};
			return e;
		}

		case SExpr::LIST:
			if (sexpr.list.size() == 3) {
				if (sexpr.list[0] == SExpr("cast")) {
					Expr e = Expr::makeCast(
							make_unique<Expr>(parseExpr(sexpr.list[2])),
							parseType(sexpr.list[1]));
					e.site = {sexpr.site};
					return e;
				}

				if (sexpr.list[0] == SExpr("unsafe-ptr-cast")) {
					Expr e = Expr::makePtrCast(
							make_unique<Expr>(parseExpr(sexpr.list[2])),
							parseType(sexpr.list[1]));
					e.site = {sexpr.site};
					return e;
				}

				if (sexpr.list[0] == SExpr("call")) {
					if (sexpr.list[1].tag != SExpr::WORD ||
							sexpr.list[2].tag != SExpr::LIST) {
						throw ParseError(sexpr.site, "Invalid call in expression");
					}

					vector<Expr> args;
					for (const SExpr &s : sexpr.list[2].list) {
						args.push_back(parseExpr(s));
					}
					Expr e = Expr::makeCall(sexpr.list[1].word, move(args));
					e.site = {sexpr.site};
					return e;
				}

				if (sexpr.list[0] == SExpr("get")) {
					Expr e = Expr(Expr::GET,
							make_unique<Expr>(parseExpr(sexpr.list[1])),
							make_unique<Expr>(parseExpr(sexpr.list[2])));
					e.site = {sexpr.site};
					return e;
				}

				if (sexpr.list[0] == SExpr("ref")) {
					Expr e = Expr(Expr::REF,
							make_unique<Expr>(parseExpr(sexpr.list[1])),
							make_unique<Expr>(parseExpr(sexpr.list[2])));
					e.site = {sexpr.site};
					return e;
				}

				int tag = -1;
				int idx1 = 1, idx2 = 2;
				if (sexpr.list[0] == SExpr("+")) tag = Expr::PLUS;
				else if (sexpr.list[0] == SExpr("-")) tag = Expr::MINUS;
				else if (sexpr.list[0] == SExpr("*")) tag = Expr::TIMES;
				else if (sexpr.list[0] == SExpr("/")) tag = Expr::DIVIDE;
				else if (sexpr.list[0] == SExpr("<")) tag = Expr::LESS;
				else if (sexpr.list[0] == SExpr("<=")) tag = Expr::LESSEQUAL;
				else if (sexpr.list[0] == SExpr(">"))
					{ tag = Expr::LESS; swap(idx1, idx2); }
				else if (sexpr.list[0] == SExpr(">="))
					{ tag = Expr::LESSEQUAL; swap(idx1, idx2); }
				else if (sexpr.list[0] == SExpr("=")) tag = Expr::EQUAL;
				else if (sexpr.list[0] == SExpr("!=")) tag = Expr::UNEQUAL;
				else if (sexpr.list[0] == SExpr("&&")) tag = Expr::BOOLAND;
				else if (sexpr.list[0] == SExpr("||")) tag = Expr::BOOLOR;

				if (tag != -1) {
					Expr e = Expr(tag,
							make_unique<Expr>(parseExpr(sexpr.list[idx1])),
							make_unique<Expr>(parseExpr(sexpr.list[idx2])));
					e.site = {sexpr.site};
					return e;
				}
			}

			throw ParseError(sexpr.site, "Invalid expr term");

		default:
			assert(false);
	}
}

static Stmt parseStmt(const SExpr &sexpr) {
	Stmt stmt;

	if (sexpr.matchList({SExpr("decl")})) {
		if (sexpr.list.size() != 3) throw ParseError(sexpr.site, "Invalid var decl");
		stmt.tag = Stmt::DECL;
		stmt.decl = parseDecl(sexpr.list[1]);
		stmt.expr = parseExpr(sexpr.list[2]);
	} else if (sexpr.matchList({SExpr("asg")})) {
		if (sexpr.list.size() != 3 || sexpr.list[1].tag != SExpr::WORD) {
			throw ParseError(sexpr.site, "Invalid assignment");
		}
		stmt.tag = Stmt::ASSIGN;
		stmt.target = sexpr.list[1].word;
		stmt.expr = parseExpr(sexpr.list[2]);
	} else if (sexpr.matchList({SExpr("store")})) {
		if (sexpr.list.size() != 3) throw ParseError(sexpr.site, "Invalid store");
		stmt.tag = Stmt::STORE;
		stmt.targetexpr = parseExpr(sexpr.list[1]);
		stmt.expr = parseExpr(sexpr.list[2]);
	} else if (sexpr.matchList({SExpr("if")})) {
		if (sexpr.list.size() != 4) throw ParseError(sexpr.site, "Invalid if");
		stmt.tag = Stmt::IF;
		stmt.expr = parseExpr(sexpr.list[1]);
		stmt.ch.push_back(parseStmt(sexpr.list[2]));
		stmt.ch.push_back(parseStmt(sexpr.list[3]));
	} else if (sexpr.matchList({SExpr("while")})) {
		if (sexpr.list.size() != 3) throw ParseError(sexpr.site, "Invalid while");
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
			throw ParseError(sexpr.site, "Invalid call");
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
			throw ParseError(sexpr.site, "Invalid call");
		}
		stmt.tag = Stmt::CALLR;
		stmt.target = sexpr.list[1].word;
		stmt.name = sexpr.list[2].word;
		for (const SExpr &s : sexpr.list[3].list) {
			stmt.args.push_back(parseExpr(s));
		}
	} else if (sexpr.matchList({SExpr("return")})) {
		if (sexpr.list.size() == 1) {
			stmt.tag = Stmt::RETURNX;
		} else if (sexpr.list.size() == 2) {
			stmt.tag = Stmt::RETURN;
			stmt.expr = parseExpr(sexpr.list[1]);
		} else {
			throw ParseError(sexpr.site, "Invalid return");
		}
	} else if (sexpr.matchList({SExpr("break")})) {
		if (sexpr.list.size() != 1) throw ParseError(sexpr.site, "Invalid break");
		stmt.tag = Stmt::BREAK;
	} else if (sexpr.matchList({SExpr("debugger")})) {
		if (sexpr.list.size() != 1) throw ParseError(sexpr.site, "Invalid debugger");
		stmt.tag = Stmt::DEBUG;
	} else {
		throw ParseError(sexpr.site, "Invalid statement");
	}

	stmt.site = sexpr.site;

	return stmt;
}

static Function parseFunction(const SExpr &sexpr) {
	if (!sexpr.matchList({SExpr("func")}) ||
			sexpr.list.size() != 5 ||
			sexpr.list[1].tag != SExpr::WORD ||
			sexpr.list[2].tag != SExpr::LIST) {
		throw ParseError(sexpr.site, "Invalid function decl");
	}

	Function f;
	f.name = sexpr.list[1].word;
	for (const SExpr &s : sexpr.list[2].list) {
		f.args.push_back(parseDecl(s));
	}
	f.ret = parseType(sexpr.list[3]);
	f.body = parseStmt(sexpr.list[4]);
	f.site = sexpr.site;
	return f;
}

Program parseProgram(const SExpr &sexpr) {
	if (!sexpr.matchList({SExpr("imp")})) throw ParseError(sexpr.site, "Invalid program");

	Program program;
	for (size_t i = 1; i < sexpr.list.size(); i++) {
		program.functions.push_back(parseFunction(sexpr.list[i]));
	}

	return program;
}
