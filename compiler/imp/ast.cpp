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
	other.contained = nullptr;
	return *this;
}

Type Type::makeInt() {
	Type type;
	type.tag = Type::INT;
	return type;
}

Type Type::makeIntSized(int bits) {
	Type type;
	type.tag = INT_SIZED;
	type.bits = bits;
	return type;
}

Type Type::makeArray(const Type &contained) {
	Type type;
	type.tag = ARRAY;
	type.contained = new Type(move(contained));
	return type;
}

int Type::size() const {
	switch (tag) {
		case INT_SIZED:
			assert(bits % 8 == 0 && 0 <= bits && bits <= 64);
			return bits / 8;
		case INT: return 8;
		case ARRAY: return 8;  // pointer
		default: assert(false);
	}
}

bool Type::isIntegral() const {
	return tag == INT || tag == INT_SIZED;
}

bool Type::operator==(const Type &other) const {
	if (tag != other.tag) return false;
	switch (tag) {
		case INT: return true;
		case INT_SIZED: return bits == other.bits;
		case ARRAY: return *contained == *other.contained;
		default: assert(false);
	}
}

bool Type::operator!=(const Type &other) const {
	return !(*this == other);
}


Expr::Expr(INT number)
		: tag(NUMBER), number(number) {}

Expr::Expr(const string &variable)
		: tag(VARIABLE), variable(variable) {}

Expr::Expr(int tag, unique_ptr<Expr> e1, unique_ptr<Expr> e2)
		: tag(tag), e1(move(e1)), e2(move(e2)) {}
