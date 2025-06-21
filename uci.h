#pragma once

#include "chronos.h"
#include "transposition.h"
#include "position.h"
#include "search.h"

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
	string king = "52 39";
	string tempo = "16 8";


};
extern SOptions options;

void UciCommand(string str);
void UciLoop();
