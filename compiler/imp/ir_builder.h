#pragma once

#include "ir.h"

using namespace std;


class IRBuilder {
public:
	Loc genReg();
	void switchBB(Id id);
	Id newBB();
	void add(IRIns ins);
	void setTerm(IRTerm term);

	IFunc finalise();

private:
	IFunc ifunc;
	Id current = -1;

	int regcounter = 0;
	Id idcounter = 0;

	int genId();
};
