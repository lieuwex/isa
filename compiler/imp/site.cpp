#include "site.h"

using namespace std;


Site::Site(const string &fname, int line, int col)
		: fname(fname), line(line), col(col) {}

ostream& operator<<(ostream &os, const Site &site) {
	return os << site.fname << ':' << site.line << ':' << site.col;
}
