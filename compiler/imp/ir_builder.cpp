#include "ir_builder.h"

using namespace std;


Loc IRBuilder::genReg() {
	return Loc::reg(regcounter++);
}

Id IRBuilder::genId() {
	return idcounter++;
}

void IRBuilder::switchBB(Id id) {
	current = id;
}

Id IRBuilder::newBB() {
	Id id = genId();
	auto p = ifunc.BBs.emplace(id, BB());
	assert(p.second);
	p.first->second.id = id;
	return id;
}

void IRBuilder::add(IRIns ins) {
	auto it = ifunc.BBs.find(current);
	if (it == ifunc.BBs.end()) throw logic_error("IRBuilder::add before first switchBB");
	it->second.inss.push_back(ins);
}

void IRBuilder::setTerm(IRTerm term) {
	auto it = ifunc.BBs.find(current);
	if (it == ifunc.BBs.end())
		throw logic_error("IRBuilder::setTerm before first switchBB");
	it->second.term = term;
}

IFunc IRBuilder::finalise() {
	return ifunc;
}
