#pragma once

#include <iostream>
#include <string>

using namespace std;


class Site {
public:
	string fname;
	int line = -1, col = -1;

	Site() = default;
	Site(const string &fname, int line, int col);
};

ostream& operator<<(ostream &os, const Site &site);
