#include <cassert>
#include "ast.h"

using namespace std;


Type::Type(int tag)
		: tag(tag) {}

Type::Type(int tag, unique_ptr<Type> contained)
		: tag(tag), contained(move(contained)) {}

int Type::size() const {
	switch (tag) {
		case INT: return 8;
		case ARRAY: return 8;  // pointer
		default: assert(false);
	}
}


Expr::Expr(INT number)
		: tag(NUMBER), number(number) {}

Expr::Expr(const string &variable)
		: tag(VARIABLE), variable(variable) {}

Expr::Expr(int tag, unique_ptr<Expr> e1, unique_ptr<Expr> e2)
		: tag(tag), e1(move(e1)), e2(move(e2)) {}
