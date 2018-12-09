#include "ir_order.h"

using namespace std;


// TODO: improve this
vector<Id> orderBBs(const IFunc &ifunc) {
	vector<Id> order;
	order.push_back(0);

	for (const auto &p : ifunc.BBs) {
		if (p.second.id != 0) {
			order.push_back(p.second.id);
		}
	}

	return order;
}
