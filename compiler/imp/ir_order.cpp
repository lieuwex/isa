#include "ir_order.h"

using namespace std;


// TODO: improve this
vector<Id> orderBBs(const IFunc &ifunc) {
	vector<Id> order;

	for (const auto &p : ifunc.BBs) {
		order.push_back(p.second.id);
	}

	return order;
}
