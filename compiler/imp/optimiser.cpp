#include <queue>
#include <unordered_set>
#include <unordered_map>
#include <cassert>
#include "optimiser.h"

using namespace std;


// All blocks in 'chain', except possibly the last, should have a JMP terminator
static void mergeChain(IFunc &ifunc, const vector<Id> &chain) {
	if (chain.size() <= 1) return;

	BB &bb0 = ifunc.BBs[chain[0]];
	assert(bb0.term.tag == IRTerm::JMP);

	for (size_t i = 1; i < chain.size(); i++) {
		const BB &bb1 = ifunc.BBs.find(chain[i])->second;
		if (i < chain.size() - 1) assert(bb1.term.tag == IRTerm::JMP);
		bb0.inss.insert(bb0.inss.end(), bb1.inss.begin(), bb1.inss.end());
	}

	bb0.term = ifunc.BBs.find(chain.back())->second.term;

	for (size_t i = 1; i < chain.size(); i++) {
		ifunc.BBs.erase(ifunc.BBs.find(chain[i]));
	}
}

static void mergeBlocks(IFunc &ifunc) {
	unordered_map<Id, Id> singleNexts, singlePrevs;
	unordered_map<Id, vector<Id>> allPrevs;

	for (const auto &p : ifunc.BBs) {
		if (p.second.term.tag == IRTerm::JMP) {
			singleNexts.emplace(p.first, p.second.term.id);
		}
		for (Id n : p.second.term.nexts()) {
			allPrevs[n].push_back(p.first);
		}
	}

	for (const auto &p : allPrevs) {
		if (p.second.size() == 1) {
			singlePrevs.emplace(p.first, p.second[0]);
		}
	}

	vector<Id> sources;
	for (const auto &p : singleNexts) sources.push_back(p.first);

	unordered_set<Id> seen;
	for (Id bid : sources) {
		if (seen.count(bid) != 0) continue;

		cerr << "source: " << bid << endl;

		while (true) {
			auto it = singlePrevs.find(bid);
			if (it == singlePrevs.end()) break;
			if (singleNexts.find(it->second) == singleNexts.end()) break;
			bid = it->second;
		}

		vector<Id> chain;

		while (true) {
			chain.push_back(bid);
			seen.insert(bid);

			auto it = singleNexts.find(bid);
			if (it == singleNexts.end()) break;
			if (singlePrevs.find(it->second) == singlePrevs.end()) break;
			bid = it->second;
		}

		if (chain.size() == 1) continue;

		mergeChain(ifunc, chain);
	}
}

static void deadCode(IFunc &ifunc) {
	unordered_set<Id> reachable;
	queue<Id> q;
	q.push(0);
	while (q.size() > 0) {
		Id id = q.front(); q.pop();
		reachable.insert(id);

		for (Id n : ifunc.BBs[id].term.nexts()) {
			q.push(n);
		}
	}

	vector<Id> unreachable;
	for (const auto &p : ifunc.BBs) {
		if (reachable.count(p.first) == 0) {
			unreachable.push_back(p.first);
		}
	}

	for (Id id : unreachable) {
		ifunc.BBs.erase(ifunc.BBs.find(id));
	}
}

static void optimise(IFunc &ifunc) {
	mergeBlocks(ifunc);
	deadCode(ifunc);
}

void optimise(IR &ir) {
	for (auto &p : ir.funcs) {
		optimise(p.second);
	}
}
