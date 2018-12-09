#include <cassert>
#include "ast.h"

using namespace std;


Type::Type(const Type &other)
		: tag(other.tag), bits(other.bits) {
	if (other.contained) contained = new Type(*other.contained);
}

Type::Type(Type &&other)
		: tag(other.tag), bits(other.bits), contained(other.contained) {
	other.tag = Type::INT;
	other.bits = 64;
	other.contained = nullptr;
}

Type::~Type() {
	if (contained) delete contained;
}

Type& Type::operator=(const Type &other) {
	tag = other.tag;
	bits = other.bits;
	contained = other.contained;
	if (contained) contained = new Type(*contained);
	return *this;
}

Type& Type::operator=(Type &&other) {
	tag = other.tag;
	bits = other.bits;
	contained = other.contained;
	other.tag = Type::INT;
	other.bits = 64;
	other.contained = nullptr;
	return *this;
}

Type Type::makeInt(int bits) {
	Type type;
	type.tag = Type::INT;
	type.bits = bits;
	return type;
}

Type Type::makeUInt(int bits) {
	Type type;
	type.tag = Type::UINT;
	type.bits = bits;
	return type;
}

Type Type::makeArray(const Type &contained) {
	Type type;
	type.tag = ARRAY;
	type.contained = new Type(move(contained));
	return type;
}

Type Type::maxType(const Type &a, const Type &b) {
	if (a == b) return a;
	if (a.tag == INT && b.tag == INT) return makeInt(max(a.bits, b.bits));
	if (a.tag == UINT && b.tag == UINT) return makeUInt(max(a.bits, b.bits));
	cerr << "maxType(";
	a.writeProlog(cerr);
	cerr << ",";
	b.writeProlog(cerr);
	cerr << ")" << endl;
	throw runtime_error("Cannot compute maximum type of incompatible types");
}

bool Type::isIntegral() const {
	return tag == INT || tag == UINT;
}

int Type::size() const {
	switch (tag) {
		case INT:
		case UINT:
			assert(bits == 1 || bits == 8 || bits == 16 || bits == 32 || bits == 64);
			return bits / 8;
		case ARRAY: return 8;  // pointer
		default: assert(false);
	}
}

Type Type::growInt() const {
	switch (tag) {
		case INT: return makeInt(64);
		case UINT: return makeUInt(64);
		default: return *this;
	}
}

bool Type::operator==(const Type &other) const {
	if (tag != other.tag) return false;
	switch (tag) {
		case INT:
		case UINT:
			return bits == other.bits;
		case ARRAY:
			return *contained == *other.contained;
		default:
			assert(false);
	}
}

bool Type::operator!=(const Type &other) const {
	return !(*this == other);
}


Expr::Expr(i64 number)
		: tag(NUMBER), number(number) {}

Expr::Expr(const string &variable)
		: tag(VARIABLE), variable(variable) {}

Expr::Expr(int tag, unique_ptr<Expr> &&e1, unique_ptr<Expr> &&e2)
		: tag(tag), e1(move(e1)), e2(move(e2)) {}

Expr Expr::makeCast(unique_ptr<Expr> &&e1, const Type &type) {
	Expr e = Expr(CAST, move(e1), unique_ptr<Expr>());
	e.type = type;
	return e;
}

Expr Expr::makeConvert(unique_ptr<Expr> &&e1, const Type &type) {
	Expr e = Expr(CONVERT, move(e1), unique_ptr<Expr>());
	e.type = type;
	return e;
}


void Type::writeProlog(ostream &os) const {
	switch (tag) {
		case INT: os << "type_int_sized(b" << bits << ")"; break;
		case UINT: os << "type_uint_sized(b" << bits << ")"; break;
		case ARRAY: os << "type_array("; contained->writeProlog(os); os << ")"; break;
	}
}

void Decl::writeProlog(ostream &os) const {
	os << "decl(" << name << ",";
	type.writeProlog(os);
	os << ")";
}

void Expr::writeProlog(ostream &os) const {
	switch (tag) {
		case NUMBER:
			os << "expr_num(" << number << ")";
			break;

		case VARIABLE:
			os << "expr_var(" << variable << ")";
			break;

		case PLUS: os << "expr_plus"; goto case_binop_label;
		case MINUS: os << "expr_minus"; goto case_binop_label;
		case TIMES: os << "expr_times"; goto case_binop_label;
		case DIVIDE: os << "expr_divide"; goto case_binop_label;
		case LESS: os << "expr_less"; goto case_binop_label;
		case LESSEQUAL: os << "expr_lessequal"; goto case_binop_label;
case_binop_label:
			os << "(";
			e1->writeProlog(os);
			os << ",";
			e2->writeProlog(os);
			os << ")";
			break;
	}
}

void Stmt::writeProlog(ostream &os) const {
	switch (tag) {
		case DECL:
			os << "stmt_decl(";
			decl.writeProlog(os); os << ",";
			expr.writeProlog(os); os << ")";
			break;

		case ASSIGN:
			os << "stmt_assign(" << target << ",";
			expr.writeProlog(os); os << ")";
			break;

		case IF:
			os << "stmt_if(";
			expr.writeProlog(os); os << ",";
			ch[0].writeProlog(os); os << ",";
			ch[1].writeProlog(os); os << ")";
			break;

		case WHILE:
			os << "stmt_while(";
			expr.writeProlog(os); os << ",";
			ch[0].writeProlog(os); os << ")";
			break;

		case DO:
			os << "stmt_do([";
			for (size_t i = 0; i < ch.size(); i++) {
				if (i != 0) os << ",";
				ch[i].writeProlog(os);
			}
			os << "])";
			break;

		case CALL:
		case CALLR:
			if (tag == CALL) os << "stmt_call(" << name << ",[";
			else os << "stmt_callr(" << target << "," << name << ",[";
			for (size_t i = 0; i < args.size(); i++) {
				if (i != 0) os << ",";
				args[i].writeProlog(os);
			}
			os << "])";
			break;

		case RETURN:
			os << "stmt_return(";
			expr.writeProlog(os); os << ")";
			break;
	}
}
