#pragma once
#include <algorithm>
#include <iostream>
#include <string>
#include <chrono>

#include "position.h"
#include "tables.h"
#include "types.h"
#include "uci.h"
#include "input.h"

#define NAME "Draco"

void PrintSummary(uint64_t time, uint64_t nodes);

//utils
std::string Trim(const std::string& s);
void SplitString(const std::string& txt, std::vector<std::string>& vStr, char ch);
void SplitInt(const std::string& txt, std::vector<int>& vInt, char ch);
std::string ThousandSeparator(uint64_t n);
std::string StrToLower(std::string s);