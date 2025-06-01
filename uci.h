#pragma once

#include "chronos.h"
#include "CTranspositionTable.h"
#include "position.h"
#include "search.h"
#include "util.h"

struct SOptions{
	bool ponder = true;
	int hash = 32;
	int elo = 2500;
	int eloMin = 0;
	int eloMax = 2500;
	int multiPV = 1;

	int aspiration = 36;
	int futility = 748;
	int lmr = 183;
	int nullMove = 941;
	int razoring = 529;
	string bishop = "32 55 -36 -4";
	string defense = "11 14 11 20 -6 18 -3 13 -62 13 -46 20";
	string king = "52 39";
	string passed = "-5 8 -49 -4 4";
	string pawn = "3 7 -28 -26 -8 -21 -10 3";
	string tempo = "16 8";


};
extern SOptions options;

void UciCommand(string str);
void UciLoop();
