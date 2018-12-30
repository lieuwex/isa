#include <stdexcept>
#include <string>
#include "site.h"

using namespace std;


class SitedError : public runtime_error {
public:
	SitedError(const Site &site, const string &msg);

	const Site& site() const;

private:
	Site _site;
};

class ParseError : public SitedError {
public:
	using SitedError::SitedError;
};

class TypeError : public SitedError {
public:
	using SitedError::SitedError;
};
