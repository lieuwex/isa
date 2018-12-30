#include "error.h"

using namespace std;


SitedError::SitedError(const Site &site_arg, const string &msg)
		: runtime_error(msg), _site(site_arg) {}

const Site& SitedError::site() const {
	return _site;
}
