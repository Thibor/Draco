
#include <iostream>
#include <random>
#include <array>
#include <string>
#include <cstring>
#include <cstdint>
#include <cstdio>
#include <ctime>
#include <vector>
#include <sstream> 

#include <algorithm>
#include <chrono>

#define MATE_SCORE (1 << 15)
#define MAX_DEPTH 128
#define INF (1 << 16)
#define U16 unsigned __int16
#define S32 signed __int32
#define S64 signed __int64
#define U64 unsigned __int64
#define NAME "Draco"
#define DATE "2025-10-27"

S64 start_time = 0;

using namespace std;


string defFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -";
string tstFen = "5rk1/ppp2ppp/8/4pP2/1P2Bb2/2P2K2/8/7R b - - 0 28";

struct SOptions {
	int elo = 2500;
	int eloMin = 0;
	int eloMax = 2500;
	U64 hash = 64ULL << 15;
	string tempo = "16 8";
};

enum Term { PASSED = 6, STRUCTURE, TERM_NB };

int scores[TERM_NB][2];

enum {
	exact, lowerbound, upperbound
};

enum PieceType
{
	PAWN,
	KNIGHT,
	BISHOP,
	ROOK,
	QUEEN,
	KING,
	PT_NB
};

enum Phase {
	MG = 0, EG = 1, PHASE_NB = 2
};

enum Square : int {
	SQ_A1, SQ_B1, SQ_C1, SQ_D1, SQ_E1, SQ_F1, SQ_G1, SQ_H1,
	SQ_A2, SQ_B2, SQ_C2, SQ_D2, SQ_E2, SQ_F2, SQ_G2, SQ_H2,
	SQ_A3, SQ_B3, SQ_C3, SQ_D3, SQ_E3, SQ_F3, SQ_G3, SQ_H3,
	SQ_A4, SQ_B4, SQ_C4, SQ_D4, SQ_E4, SQ_F4, SQ_G4, SQ_H4,
	SQ_A5, SQ_B5, SQ_C5, SQ_D5, SQ_E5, SQ_F5, SQ_G5, SQ_H5,
	SQ_A6, SQ_B6, SQ_C6, SQ_D6, SQ_E6, SQ_F6, SQ_G6, SQ_H6,
	SQ_A7, SQ_B7, SQ_C7, SQ_D7, SQ_E7, SQ_F7, SQ_G7, SQ_H7,
	SQ_A8, SQ_B8, SQ_C8, SQ_D8, SQ_E8, SQ_F8, SQ_G8, SQ_H8,
	SQ_NONE,

	SQUARE_NB = 64
};

enum Value :int {
	VALUE_ZERO = 0,
	PawnValueMg = 136, PawnValueEg = 208,
	KnightValueMg = 782, KnightValueEg = 865,
	BishopValueMg = 830, BishopValueEg = 918,
	RookValueMg = 1289, RookValueEg = 1378,
	QueenValueMg = 2529, QueenValueEg = 2687
};

constexpr Value operator+(Value v, int i) { return Value(int(v) + i); }
constexpr Value operator-(Value v, int i) { return Value(int(v) - i); }
inline Value& operator+=(Value& v, int i) { return v = v + i; }
inline Value& operator-=(Value& v, int i) { return v = v - i; }

int PieceValue[PHASE_NB][PT_NB] = {
  { PawnValueMg, KnightValueMg, BishopValueMg, RookValueMg, QueenValueMg },
  { PawnValueEg, KnightValueEg, BishopValueEg, RookValueEg, QueenValueEg }
};


constexpr U64 FileABB = 0x0101010101010101ULL;
constexpr U64 FileBBB = FileABB << 1;
constexpr U64 FileCBB = FileABB << 2;
constexpr U64 FileDBB = FileABB << 3;
constexpr U64 FileEBB = FileABB << 4;
constexpr U64 FileFBB = FileABB << 5;
constexpr U64 FileGBB = FileABB << 6;
constexpr U64 FileHBB = FileABB << 7;

constexpr U64 Rank1BB = 0xFF;
constexpr U64 Rank2BB = Rank1BB << (8 * 1);
constexpr U64 Rank3BB = Rank1BB << (8 * 2);
constexpr U64 Rank4BB = Rank1BB << (8 * 3);
constexpr U64 Rank5BB = Rank1BB << (8 * 4);
constexpr U64 Rank6BB = Rank1BB << (8 * 5);
constexpr U64 Rank7BB = Rank1BB << (8 * 6);
constexpr U64 Rank8BB = Rank1BB << (8 * 7);

const U64 bbOutpostRanks = Rank4BB | Rank5BB | Rank6BB;
const U64 bbDark = 0x55aa55aa55aa55aaull;
const U64 bbLight = 0xaa55aa55aa55aa55ull;
constexpr U64 queenSide = FileABB | FileBBB | FileCBB | FileDBB;
constexpr U64 centerFiles = FileCBB | FileDBB | FileEBB | FileFBB;
constexpr U64 kingSide = FileEBB | FileFBB | FileGBB | FileHBB;
constexpr U64 center = (FileDBB | FileEBB) & (Rank4BB | Rank5BB);

enum File : int { FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, FILE_NB };

static constexpr File operator++(File& f) { return f = File(int(f) + 1); }

static constexpr File operator~(File& f) {
	return File(f ^ FILE_H); // Horizontal flip FILE_A -> FILE_H
}

enum Rank : int { RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8, RANK_NB };

const U64 MASK_FILE[8] = {
	0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
	0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080
};

U64 bbDistanceRing[64][8];

inline static S64 SqToBb(int sq) {
	if (sq < 0 || sq > 63)
		return 0;
	return 1ULL << sq;
}

static S64 Now() {
	return (clock() * 1000) / CLOCKS_PER_SEC;
}

struct Position {
	array<int, 4> castling = { true, true, true, true };
	array<U64, 2> color = { 0xFFFFULL, 0xFFFF000000000000ULL };
	array<U64, 6> pieces = { 0xFF00000000FF00ULL,
							0x4200000000000042ULL,
							0x2400000000000024ULL,
							0x8100000000000081ULL,
							0x800000000000008ULL,
							0x1000000000000010ULL };
	U64 ep = 0x0ULL;
	int flipped = false;
};

struct Move {
	int from = 0;
	int to = 0;
	int promo = 0;
};

auto operator==(const Move& lhs, const Move& rhs) {
	return !memcmp(&rhs, &lhs, sizeof(Move));
}

const Move no_move{};

struct Stack {
	Move moves[218];
	Move move;
	Move killer;
	int score;
};

struct TT_Entry {
	U64 key;
	Move move;
	int score;
	int depth;
	U16 flag;
};

int phase = 0;

static int S(const int mg, const int eg) {
	return (eg << 16) + mg;
}

const int phases[] = { 0, 1, 1, 2, 4, 0 };
int max_material[PT_NB] = {};
int material[PT_NB] = {};
int outsideFile[PT_NB] = {};
int outsideRank[PT_NB] = {};
int bonus[PT_NB][RANK_NB][FILE_NB] = {};
int bonusMax[PT_NB][RANK_NB][FILE_NB] = {};
int tempo = 0;
int contempt = 0;

// Connected pawn bonus by opposed, phalanx, #support and rank
int Connected[2][2][3][RANK_NB];
int BishopPawns = S(3, 8);
int LongDiagonalBishop = S(44, 0);

int Backward = S(9, 24);
int Doubled = S(11, 56);
int Isolated = S(5, 15);

// PassedRank[Rank] contains a bonus according to the rank of a passed pawn
int PassedRank[RANK_NB] = {
  S(0, 0), S(5, 18), S(12, 23), S(10, 31), S(57, 62), S(163, 167), S(271, 250)
};

// PassedFile[File] contains a bonus according to the file of a passed pawn
int PassedFile[FILE_NB] = {
  S(-1,  7), S(0,  9), S(-9, -8), S(-30,-14),S(-30,-14), S(-9, -8), S(0,  9), S(-1,  7)
};

int RookOnFile[] = { S(18, 7), S(44, 20) };

int outpost[2][2] = {
	{ S(22, 6), S(36,12) }, // Knight
	{ S(9, 2), S(15, 5) }  // Bishop
};

int BonusOrg[PT_NB][RANK_NB][int(FILE_NB) / 2] = {
	//int BonusOrg[PT_NB][RANK_NB][4] = {
	  { // Pawn
	   { S(0, 0), S(0,  0), S(0, 0), S(0, 0) },
	   { S(-11,-3), S(7, -1), S(7, 7), S(17, 2) },
	   { S(-16,-2), S(-3,  2), S(23, 6), S(23,-1) },
	   { S(-14, 7), S(-7, -4), S(20,-8), S(24, 2) },
	   { S(-5,13), S(-2, 10), S(-1,-1), S(12,-8) },
	   { S(-11,16), S(-12,  6), S(-2, 1), S(4,16) },
	   { S(-2, 1), S(20,-12), S(-10, 6), S(-2,25) }
	  },
	  { // Knight
	   { S(-169,-105), S(-96,-74), S(-80,-46), S(-79,-18) },
	   { S(-79, -70), S(-39,-56), S(-24,-15), S(-9,  6) },
	   { S(-64, -38), S(-20,-33), S(4, -5), S(19, 27) },
	   { S(-28, -36), S(5,  0), S(41, 13), S(47, 34) },
	   { S(-29, -41), S(13,-20), S(42,  4), S(52, 35) },
	   { S(-11, -51), S(28,-38), S(63,-17), S(55, 19) },
	   { S(-67, -64), S(-21,-45), S(6,-37), S(37, 16) },
	   { S(-200, -98), S(-80,-89), S(-53,-53), S(-32,-16) }
	  },
	  { // Bishop
	   { S(-49,-58), S(-7,-31), S(-10,-37), S(-34,-19) },
	   { S(-24,-34), S(9, -9), S(15,-14), S(1,  4) },
	   { S(-9,-23), S(22,  0), S(-3, -3), S(12, 16) },
	   { S(4,-26), S(9, -3), S(18, -5), S(40, 16) },
	   { S(-8,-26), S(27, -4), S(13, -7), S(30, 14) },
	   { S(-17,-24), S(14, -2), S(-6,  0), S(6, 13) },
	   { S(-19,-34), S(-13,-10), S(7,-12), S(-11,  6) },
	   { S(-47,-55), S(-7,-32), S(-17,-36), S(-29,-17) }
	  },
	  { // Rook
	   { S(-24, 0), S(-15, 3), S(-8, 0), S(0, 3) },
	   { S(-18,-7), S(-5,-5), S(-1,-5), S(1,-1) },
	   { S(-19, 6), S(-10,-7), S(1, 3), S(0, 3) },
	   { S(-21, 0), S(-7, 4), S(-4,-2), S(-4, 1) },
	   { S(-21,-7), S(-12, 5), S(-1,-5), S(4,-7) },
	   { S(-23, 3), S(-10, 2), S(1,-1), S(6, 3) },
	   { S(-11,-1), S(8, 7), S(9,11), S(12,-1) },
	   { S(-25, 6), S(-18, 4), S(-11, 6), S(2, 2) }
	  },
	  { // Queen
	   { S(3,-69), S(-5,-57), S(-5,-47), S(4,-26) },
	   { S(-3,-55), S(5,-31), S(8,-22), S(12, -4) },
	   { S(-3,-39), S(6,-18), S(13, -9), S(7,  3) },
	   { S(4,-23), S(5, -3), S(9, 13), S(8, 24) },
	   { S(0,-29), S(14, -6), S(12,  9), S(5, 21) },
	   { S(-4,-38), S(10,-18), S(6,-12), S(8,  1) },
	   { S(-5,-50), S(6,-27), S(10,-24), S(8, -8) },
	   { S(-2,-75), S(-2,-52), S(1,-43), S(-2,-36) }
	  },
	  { // King
	   { S(272,  0), S(325, 41), S(273, 80), S(190, 93) },
	   { S(277, 57), S(305, 98), S(241,138), S(183,131) },
	   { S(198, 86), S(253,138), S(168,165), S(120,173) },
	   { S(169,103), S(191,152), S(136,168), S(108,169) },
	   { S(145, 98), S(176,166), S(112,197), S(69, 194) },
	   { S(122, 87), S(159,164), S(85, 174), S(36, 189) },
	   { S(87,  40), S(120, 99), S(64, 128), S(25, 141) },
	   { S(64,   5), S(87,  60), S(49,  75), S(0,   75) }
	  }
};

// MobilityBonus[PieceType-2][attacked] contains bonuses for middle and end game,
// indexed by piece type and number of attacked squares in the mobility area.
int MobilityBonus[][32] = {
  { S(-62,-81), S(-53,-56), S(-12,-30), S(-4,-14), S(3,  8), S(13, 15), // Knights
	S(22, 23), S(28, 27), S(33, 33) },
  { S(-48,-59), S(-20,-23), S(16, -3), S(26, 13), S(38, 24), S(51, 42), // Bishops
	S(55, 54), S(63, 57), S(63, 65), S(68, 73), S(81, 78), S(81, 86),
	S(91, 88), S(98, 97) },
  { S(-58,-76), S(-27,-18), S(-15, 28), S(-10, 55), S(-5, 69), S(-2, 82), // Rooks
	S(9,112), S(16,118), S(30,132), S(29,142), S(32,155), S(38,165),
	S(46,166), S(48,169), S(58,171) },
  { S(-39,-36), S(-21,-15), S(3,  8), S(3, 18), S(14, 34), S(22, 54), // Queens
	S(28, 61), S(41, 73), S(43, 79), S(48, 92), S(56, 94), S(60,104),
	S(60,113), S(66,120), S(67,123), S(70,126), S(71,133), S(73,136),
	S(79,140), S(88,143), S(88,148), S(99,166), S(102,170), S(102,175),
	S(106,184), S(109,191), S(113,206), S(116,212) }
};

#define V Value

// Strength of pawn shelter for our king by [distance from edge][rank].
// RANK_1 = 0 is used for files where we have no pawn, or pawn is behind our king.
constexpr Value ShelterStrength[int(FILE_NB) / 2][RANK_NB] = {
  { V(-6), V(81), V(93), V(58), V(39), V(18), V(25) },
  { V(-43), V(61), V(35), V(-49), V(-29), V(-11), V(-63) },
  { V(-10), V(75), V(23), V(-2), V(32), V(3), V(-45) },
  { V(-39), V(-13), V(-29), V(-52), V(-48), V(-67), V(-166) }
};

// Danger of enemy pawns moving toward our king by [distance from edge][rank].
// RANK_1 = 0 is used for files where the enemy has no pawn, or their pawn
// is behind our king.
constexpr Value UnblockedStorm[int(FILE_NB) / 2][RANK_NB] = {
  { V(89), V(107), V(123), V(93), V(57), V(45), V(51) },
  { V(44), V(-18), V(123), V(46), V(39), V(-7), V(23) },
  { V(4), V(52), V(162), V(37), V(7), V(-14), V(-2) },
  { V(-10), V(-14), V(90), V(15), V(2), V(-7), V(-16) }
};

#undef V

// Polynomial material imbalance parameters

constexpr int QuadraticOurs[][6] = {
	//            OUR PIECES
	// pair pawn knight bishop rook queen
	{1438                               }, // Bishop pair
	{  40,   38                         }, // Pawn
	{  32,  255, -62                    }, // Knight      OUR PIECES
	{   0,  104,   4,    0              }, // Bishop
	{ -26,   -2,  47,   105,  -208      }, // Rook
	{-189,   24, 117,   133,  -134, -6  }  // Queen
};

constexpr int QuadraticTheirs[][6] = {
	//           THEIR PIECES
	// pair pawn knight bishop rook queen
	{   0                               }, // Bishop pair
	{  36,    0                         }, // Pawn
	{   9,   63,   0                    }, // Knight      OUR PIECES
	{  59,   65,  42,     0             }, // Bishop
	{  46,   39,  24,   -24,    0       }, // Rook
	{  97,  100, -42,   137,  268,    0 }  // Queen
};

U64 bbAdjacentFiles[FILE_NB];
U64 bbRanks[RANK_NB];
U64 bbFiles[FILE_NB];
U64 bbForwardRanks[RANK_NB];

SOptions options;
Position pos;
vector<U64> hash_history;

const auto keys = []() {
	mt19937_64 r;

	// pieces from 1-12 multiplied the square + ep squares + castling rights
	// 12 * 64 + 64 + 16 = 848
	array<U64, 848> values;
	for (auto& val : values) {
		val = r();
	}

	return values;
	}();

// Engine options
auto num_tt_entries = 64ULL << 15;  // The first value is the size in megabytes

vector<TT_Entry> transposition_table;

static void PrintWelcome() {
	cout << NAME << " " << DATE << endl;
}

void TranspositionClear() {
	memset(transposition_table.data(), 0, sizeof(TT_Entry) * transposition_table.size());
}

// less significant bit
inline static Square lsb(U64 b) {
	unsigned long idx;
	_BitScanForward64(&idx, b);
	return (Square)idx;
}

// most significant bit
inline static Square msb(U64 b) {
	unsigned long idx;
	_BitScanReverse64(&idx, b);
	return (Square)idx;
}

static U64 FlipBitboard(const U64 bb) {
	return _byteswap_uint64(bb);
}


static int Popcount(const U64 bb) {
	return (int)__popcnt64(bb);
}

static U64 East(const U64 bb) {
	return (bb << 1) & 0xfefefefefefefefe;
}

static U64 West(const U64 bb) {
	return (bb >> 1) & 0x7f7f7f7f7f7f7f7f;
}

static U64 North(const U64 bb) {
	return bb << 8;
}

static U64 South(const U64 bb) {
	return bb >> 8;
}

static U64 ss(const U64 bb) {
	return bb >> 16;
}

static U64 nw(const U64 bb) {
	return (bb << 7) & 0x7f7f7f7f7f7f7f7f;
}

static U64 ne(const U64 bb) {
	return (bb << 9) & 0xfefefefefefefefe;
}

static U64 sw(const U64 bb) {
	return (bb >> 9) & 0x7f7f7f7f7f7f7f7f;
}

static U64 se(const U64 bb) {
	return (bb >> 7) & 0xfefefefefefefefe;
}

static constexpr Rank RankOf(Square sq) { return Rank(sq >> 3); }
static constexpr File FileOf(Square sq) { return File(sq & 0b111); }

static int Distance(Square sq1, Square sq2) {
	return max(abs(FileOf(sq1) - FileOf(sq2)), abs(RankOf(sq1) - RankOf(sq2)));
};

static int KingDistance(Square sq1, Square sq2) {
	return min(Distance(sq1, sq2), 5);
}

static int Mg(int score) {
	return (short)score;
}

static int Eg(int score) {
	return (score + 0x8000) >> 16;
}

static int ValueMax(int score) {
	return max(Mg(score), Eg(score));
}

static int ValueToCp(int v) {
	return (v * 100) / PawnValueEg;
}

static bool MoreThanOne(U64 b) {
	return b & (b - 1);
}

static string SquareToUci(const int sq, const int flip) {
	string str;
	str += 'a' + (sq % 8);
	str += '1' + (flip ? (7 - sq / 8) : (sq / 8));
	return str;
}

static auto MoveToUci(const Move& move, const int flip) {
	string str = SquareToUci(move.from, flip);
	str += SquareToUci(move.to, flip);
	if (move.promo != PT_NB) {
		str += "\0nbrq\0\0"[move.promo];
	}
	return str;
}

static int PieceTypeOn(const Position& pos, const int sq) {
	const U64 bb = 1ULL << sq;
	for (int i = 0; i < 6; ++i) {
		if (pos.pieces[i] & bb) {
			return i;
		}
	}
	return PT_NB;
}

static void flip(Position& pos) {
	pos.color[0] = FlipBitboard(pos.color[0]);
	pos.color[1] = FlipBitboard(pos.color[1]);
	for (int i = 0; i < 6; ++i) {
		pos.pieces[i] = FlipBitboard(pos.pieces[i]);
	}
	pos.ep = FlipBitboard(pos.ep);
	swap(pos.color[0], pos.color[1]);
	swap(pos.castling[0], pos.castling[2]);
	swap(pos.castling[1], pos.castling[3]);
	pos.flipped = !pos.flipped;
}

template <typename F>
U64 Ray(const int sq, const U64 blockers, F f) {
	U64 mask = f(1ULL << sq);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	return mask;
}

static U64 KnightAttack(const int sq, const U64) {
	const U64 bb = 1ULL << sq;
	return (((bb << 15) | (bb >> 17)) & 0x7F7F7F7F7F7F7F7FULL) | (((bb << 17) | (bb >> 15)) & 0xFEFEFEFEFEFEFEFEULL) |
		(((bb << 10) | (bb >> 6)) & 0xFCFCFCFCFCFCFCFCULL) | (((bb << 6) | (bb >> 10)) & 0x3F3F3F3F3F3F3F3FULL);
}

static U64 BishopAttack(const int sq, const U64 blockers) {
	return Ray(sq, blockers, nw) | Ray(sq, blockers, ne) | Ray(sq, blockers, sw) | Ray(sq, blockers, se);
}

static U64 RookAttack(const int sq, const U64 blockers) {
	return Ray(sq, blockers, North) | Ray(sq, blockers, East) | Ray(sq, blockers, South) | Ray(sq, blockers, West);
}

static U64 KingAttack(const int sq, const U64) {
	const U64 bb = 1ULL << sq;
	return (bb << 8) | (bb >> 8) | (((bb >> 1) | (bb >> 9) | (bb << 7)) & 0x7F7F7F7F7F7F7F7FULL) |
		(((bb << 1) | (bb << 9) | (bb >> 7)) & 0xFEFEFEFEFEFEFEFEULL);
}

static bool Attacked(const Position& pos, const int sq, const int them = true) {
	const U64 bb = 1ULL << sq;
	const U64 kt = pos.color[them] & pos.pieces[KNIGHT];
	const U64 BQ = pos.pieces[BISHOP] | pos.pieces[QUEEN];
	const U64 RQ = pos.pieces[ROOK] | pos.pieces[QUEEN];
	const U64 pawns = pos.color[them] & pos.pieces[PAWN];
	const U64 pawn_attacks = them ? sw(pawns) | se(pawns) : nw(pawns) | ne(pawns);
	return (pawn_attacks & bb) | (kt & KnightAttack(sq, 0)) |
		(BishopAttack(sq, pos.color[0] | pos.color[1]) & pos.color[them] & BQ) |
		(RookAttack(sq, pos.color[0] | pos.color[1]) & pos.color[them] & RQ) |
		(KingAttack(sq, 0) & pos.color[them] & pos.pieces[KING]);
}

static auto MakeMove(Position& pos, const Move& move) {
	const int piece = PieceTypeOn(pos, move.from);
	const int captured = PieceTypeOn(pos, move.to);
	const U64 to = 1ULL << move.to;
	const U64 from = 1ULL << move.from;

	// Move the piece
	pos.color[0] ^= from | to;
	pos.pieces[piece] ^= from | to;

	// En passant
	if (piece == PAWN && to == pos.ep) {
		pos.color[1] ^= to >> 8;
		pos.pieces[PAWN] ^= to >> 8;
	}

	pos.ep = 0x0ULL;

	// Pawn double move
	if (piece == PAWN && move.to - move.from == 16) {
		pos.ep = to >> 8;
	}

	// Captures
	if (captured != PT_NB) {
		pos.color[1] ^= to;
		pos.pieces[captured] ^= to;
	}

	// Castling
	if (piece == KING) {
		const U64 bb = move.to - move.from == 2 ? 0xa0ULL : move.to - move.from == -2 ? 0x9ULL : 0x0ULL;
		pos.color[0] ^= bb;
		pos.pieces[ROOK] ^= bb;
	}

	// Promotions
	if (piece == PAWN && move.to >= 56) {
		pos.pieces[PAWN] ^= to;
		pos.pieces[move.promo] ^= to;
	}

	// Update castling permissions
	pos.castling[0] &= !((from | to) & 0x90ULL);
	pos.castling[1] &= !((from | to) & 0x11ULL);
	pos.castling[2] &= !((from | to) & 0x9000000000000000ULL);
	pos.castling[3] &= !((from | to) & 0x1100000000000000ULL);

	flip(pos);

	// Return move legality
	return !Attacked(pos, lsb(pos.color[1] & pos.pieces[KING]), false);
}

void add_move(Move* const movelist, int& num_moves, const int from, const int to, const int promo = PT_NB) {
	movelist[num_moves++] = Move{ from, to, promo };
}

void generate_pawn_moves(Move* const movelist, int& num_moves, U64 to_mask, const int offset) {
	while (to_mask) {
		const int to = lsb(to_mask);
		to_mask &= to_mask - 1;
		if (to >= 56) {
			add_move(movelist, num_moves, to + offset, to, QUEEN);
			add_move(movelist, num_moves, to + offset, to, ROOK);
			add_move(movelist, num_moves, to + offset, to, BISHOP);
			add_move(movelist, num_moves, to + offset, to, KNIGHT);
		}
		else {
			add_move(movelist, num_moves, to + offset, to);
		}
	}
}

void generate_piece_moves(Move* const movelist,
	int& num_moves,
	const Position& pos,
	const int piece,
	const U64 to_mask,
	U64(*func)(int, U64)) {
	U64 copy = pos.color[0] & pos.pieces[piece];
	while (copy) {
		const int fr = lsb(copy);
		copy &= copy - 1;
		U64 moves = func(fr, pos.color[0] | pos.color[1]) & to_mask;
		while (moves) {
			const int to = lsb(moves);
			moves &= moves - 1;
			add_move(movelist, num_moves, fr, to);
		}
	}
}

static int MoveGen(const Position& pos, Move* const movelist, const bool only_captures) {
	int num_moves = 0;
	const U64 all = pos.color[0] | pos.color[1];
	const U64 to_mask = only_captures ? pos.color[1] : ~pos.color[0];
	const U64 pawns = pos.color[0] & pos.pieces[PAWN];
	generate_pawn_moves(
		movelist, num_moves, North(pawns) & ~all & (only_captures ? 0xFF00000000000000ULL : 0xFFFFFFFFFFFF0000ULL), -8);
	if (!only_captures) {
		generate_pawn_moves(movelist, num_moves, North(North(pawns & 0xFF00ULL) & ~all) & ~all, -16);
	}
	generate_pawn_moves(movelist, num_moves, nw(pawns) & (pos.color[1] | pos.ep), -7);
	generate_pawn_moves(movelist, num_moves, ne(pawns) & (pos.color[1] | pos.ep), -9);
	generate_piece_moves(movelist, num_moves, pos, KNIGHT, to_mask, KnightAttack);
	generate_piece_moves(movelist, num_moves, pos, BISHOP, to_mask, BishopAttack);
	generate_piece_moves(movelist, num_moves, pos, QUEEN, to_mask, BishopAttack);
	generate_piece_moves(movelist, num_moves, pos, ROOK, to_mask, RookAttack);
	generate_piece_moves(movelist, num_moves, pos, QUEEN, to_mask, RookAttack);
	generate_piece_moves(movelist, num_moves, pos, KING, to_mask, KingAttack);
	if (!only_captures && pos.castling[0] && !(all & 0x60ULL) && !Attacked(pos, 4) && !Attacked(pos, 5)) {
		add_move(movelist, num_moves, 4, 6);
	}
	if (!only_captures && pos.castling[1] && !(all & 0xEULL) && !Attacked(pos, 4) && !Attacked(pos, 3)) {
		add_move(movelist, num_moves, 4, 2);
	}
	return num_moves;
}

//Pretty-prints the position (including FEN and hash key)
static void PrintBoard(Position& pos) {
	bool r = pos.flipped;
	if (r)
		flip(pos);
	const char* s = "   +---+---+---+---+---+---+---+---+\n";
	const char* t = "     A   B   C   D   E   F   G   H\n";
	cout << t;
	for (int i = 56; i >= 0; i -= 8) {
		cout << s << " " << i / 8 + 1 << " ";
		for (int j = 0; j < 8; j++) {
			int sq = i + j;
			int piece = PieceTypeOn(pos, sq);
			if (pos.color[0] & 1ull << sq)
				cout << "| " << "ANBRQK "[piece] << " ";
			else
				cout << "| " << "anbrqk "[piece] << " ";
		}
		cout << "| " << i / 8 + 1 << endl;
	}
	cout << s;
	cout << t << endl;
	if (r)
		flip(pos);
}

static void PrintBitboard(U64 bb) {
	const char* s = "   +---+---+---+---+---+---+---+---+\n";
	const char* t = "     A   B   C   D   E   F   G   H\n";
	cout << t;
	for (int i = 56; i >= 0; i -= 8) {
		cout << s << " " << i / 8 + 1 << " ";
		for (int j = 0; j < 8; j++) {
			int sq = i + j;
			U64 bbSq = 1ull << sq;
			if (bb & bbSq) {
				cout << "| X ";
			}
			else
			{
				cout << "|   ";
			}
		}
		cout << "| " << i / 8 + 1 << endl;
	}
	cout << s;
	cout << t << endl;
}

static int TotalScore(int c) {
	int score = 0;
	for (int n = 0; n < TERM_NB; n++)
		score += scores[n][c];
	return score;
}

U64 Span(U64 bb) {
	return bb | bb >> 8 | bb >> 16 | bb >> 24 | bb >> 32;
}

U64 SpanNorth(U64 bb) {
	return bb | bb << 8 | bb << 16 | bb << 24 | bb << 32;
}

static constexpr U64 Attacks(int pt, int sq, U64 blockers) {
	switch (pt) {
	case ROOK:
		return RookAttack(sq, blockers);
	case BISHOP:
		return BishopAttack(sq, blockers);
	case QUEEN:
		return RookAttack(sq, blockers) | BishopAttack(sq, blockers);
	case KNIGHT:
		return KnightAttack(sq, blockers);
	case KING:
		return KingAttack(sq, blockers);
	default:
		return 0;
	}
}

static int Imbalance(int us, const int pieceCount[][6]) {
	int en = us ? 0 : 1;
	int bonus = 0;
	for (int pt1 = 0; pt1 < 6; ++pt1)
	{
		if (!pieceCount[us][pt1])
			continue;
		int v = 0;
		for (int pt2 = 0; pt2 <= pt1; ++pt2)
			v += QuadraticOurs[pt1][pt2] * pieceCount[us][pt2]
			+ QuadraticTheirs[pt1][pt2] * pieceCount[en][pt2];

		bonus += pieceCount[us][pt1] * v;
	}
	return bonus;
}

static Value EvalShelter(Position& pos, Square ksq) {
	U64 bb = pos.pieces[PAWN] & bbForwardRanks[RankOf(ksq)];
	const U64 bbPawnsUs = bb & pos.color[0];
	const U64 bbPawnsEn = bb & pos.color[1];
	File center = std::max(FILE_B, std::min(FILE_G, FileOf(ksq)));
	Value safety = (South(bbPawnsEn) & (FileABB | FileHBB) & (Rank1BB | Rank2BB) & ksq) ? Value(374) : Value(5);
	for (File f = File(center - 1); f <= File(center + 1); ++f)
	{
		bb = bbPawnsUs & bbFiles[f];
		int rankUs = bb ? RankOf(lsb(bb)) : 0;

		bb = bbPawnsEn & bbFiles[f];
		int rankEn = bb ? RankOf(lsb(bb)) : 0;

		int d = std::min(f, ~f);
		safety += ShelterStrength[d][rankUs];
		safety -= (rankUs && (rankUs == rankEn - 1)) ? 66 * (rankEn == RANK_3) : UnblockedStorm[d][rankEn];
	}
	return safety;
}

static Value KingSafety(Position& pos, Square ksq) {
	Value bonus = EvalShelter(pos, ksq);
	if (pos.castling[0])
		bonus = std::max(bonus, EvalShelter(pos, SQ_G1));
	if (pos.castling[1])
		bonus = std::max(bonus, EvalShelter(pos, SQ_C1));
	return bonus;
}

static int Eval(Position& pos) {
	std::memset(scores, 0, sizeof(scores));
	int score = tempo;
	int ptCount[2][6] = {};
	phase = 0;
	for (int c = 0; c < 2; ++c) {
		U64 bbAll = pos.color[0] | pos.color[1];
		const U64 bbPawnsUs = pos.color[0] & pos.pieces[PAWN];
		const U64 bbPawnsEn = pos.color[1] & pos.pieces[PAWN];
		const U64 bbPawnDefense = nw(bbPawnsUs) | ne(bbPawnsUs);
		const U64 bbPawnAttack = se(bbPawnsEn) | sw(bbPawnsEn);
		const U64 bbSpan = Span(bbPawnAttack);
		const U64 bbOutpost = ~bbSpan & bbOutpostRanks;
		const Square sqKUs = lsb(pos.color[0] & pos.pieces[KING]);
		const Square sqKEn = lsb(pos.color[1] & pos.pieces[KING]);
		U64 bbConnected = bbPawnDefense | South(bbPawnDefense);
		bbConnected |= South(bbConnected);
		U64 lowRanks = Rank2BB | Rank3BB;
		U64 bbBlocked = bbPawnsUs & (South(bbAll) | lowRanks);
		U64 bbMobilityArea = ~(bbBlocked | ((pos.pieces[QUEEN] | pos.pieces[KING]) & pos.color[0]) | bbPawnAttack);
		for (int pt = 0; pt < PT_NB; ++pt) {
			auto copy = pos.color[0] & pos.pieces[pt];
			while (copy) {
				phase += phases[pt];
				ptCount[c][pt]++;
				const Square sq = lsb(copy);
				copy &= copy - 1;
				const int rank = sq / 8;
				const int file = sq % 8;
				int score = bonus[pt][rank][file];
				const U64 bbPiece = 1ULL << sq;
				if (pt == PAWN) {
					// Passed pawns
					U64 bbFile = 0x101010101010101ULL << file;
					U64 bbForward = 0x101010101010100ULL << sq;
					U64 blockers = bbForward | West(bbForward) | East(bbForward);
					if (!(blockers & bbPawnsEn)) {
						int passed = PassedFile[file];
						passed += PassedRank[rank];
						if (rank > RANK_3)
						{
							int w = (rank - 2) * (rank - 2) + 2;
							Square sq2 = Square(sq + 8);
							passed += S(0, (KingDistance(sqKEn, sq2) * 5 - KingDistance(sqKUs, sq2) * 2) * w);
							if (rank != RANK_7)
								passed -= S(0, KingDistance(sqKUs, Square(sq2 + 8)) * w);
						}
						scores[PASSED][pos.flipped] += passed;
					}
					int structure = 0;
					U64 opposed = bbPawnsEn & bbForward;
					U64 doubled = sq > 8 ? bbPawnsUs & SqToBb(sq - 8) : 0;
					U64 neighbors = bbPawnsUs & bbAdjacentFiles[file];
					U64 phalanx = neighbors & bbRanks[rank];
					U64 supported = rank > 0 ? neighbors & bbRanks[rank - 1] : 0;
					if (supported | phalanx)
						structure += Connected[bool(opposed)][bool(phalanx)][Popcount(supported)][rank];
					else if (!neighbors)
						structure -= Isolated;
					else {
						U64 bbFront = North(bbPiece);
						bbFront |= ne(bbFront) | nw(bbFront);
						U64 bbBack = Span(South(East(bbPiece) | West(bbPiece)));
						if ((!(bbBack & bbPawnsUs)) && (bbFront & bbPawnsEn)) {
							structure -= Backward;
						}
					}
					if (doubled && !supported)
						structure -= Doubled;
					scores[STRUCTURE][pos.flipped] += structure;
				}
				else if (pt == KING) {
					int minKingPawnDistance = 0;
					if (bbPawnsUs)
						while (!(bbDistanceRing[sq][++minKingPawnDistance] & bbPawnsUs)) {}
					scores[pt][pos.flipped] += S(KingSafety(pos, sq), -16 * minKingPawnDistance);
				}
				else {
					U64 bbAttacks = Attacks(pt, sq, bbAll);
					score += MobilityBonus[pt - KNIGHT][Popcount(bbAttacks & bbMobilityArea)];
					if (pt == ROOK) {
						const U64 file_bb = 0x101010101010101ULL << file;
						if (!(file_bb & bbPawnsUs))score += RookOnFile[!(file_bb & bbPawnsEn)];
					}
					else if ((pt == KNIGHT) || (pt == BISHOP)) {
						if (bbOutpost & bbPiece)
							score += outpost[pt == BISHOP][bool(bbPawnDefense & bbPiece)] * 2;
						else {
							U64 bbMoves = (pt == KNIGHT) ? KnightAttack(sq, pos.color[0]) : BishopAttack(sq, pos.color[0] | pos.color[1]);
							U64 bb = bbMoves & bbOutpost & ~pos.color[0];
							if (bb)
								score += outpost[pt == BISHOP][bool(bbPawnDefense & bb)];
						}
						if (pt == BISHOP) {
							U64 blocked = bbPawnsUs & South(bbAll);
							score -= BishopPawns * Popcount(bbPawnsUs & (bbPiece & bbLight ? bbLight : bbDark)) * (1 + Popcount(blocked & centerFiles));
							if (MoreThanOne(bbAttacks & center))score += LongDiagonalBishop;
						}
					}
				}
				scores[pt][pos.flipped] += score;
			}

		}
		score += TotalScore(pos.flipped);
		flip(pos);
		score = -score;
	}
	const int pieceCount[2][6] = {
	{ptCount[0][2] > 1,ptCount[0][0],ptCount[0][1],ptCount[0][2],ptCount[0][3],ptCount[0][4]},
	{ptCount[1][2] > 1,ptCount[1][0],ptCount[1][1],ptCount[1][2],ptCount[1][3],ptCount[1][4]} };
	int imbalanceUs = Imbalance(0, pieceCount);
	int imbalanceEn = Imbalance(1, pieceCount);
	int imbalance = (imbalanceUs - imbalanceEn) / 16;
	score += S(imbalance, imbalance);
	//cout << Mg(score) << " " << Eg(score) << endl;
	return (Mg(score) * phase + Eg(score) * (24 - phase)) / 24;
}

static auto GetHash(const Position& pos) {
	U64 hash = pos.flipped;

	// Pieces
	U64 copy = pos.color[0] | pos.color[1];
	while (copy) {
		const int sq = lsb(copy);
		copy &= copy - 1;
		hash ^= keys[(PieceTypeOn(pos, sq) + 6 * ((pos.color[pos.flipped] >> sq) & 1)) * 64 + sq];
	}

	// En passant square
	if (pos.ep) {
		hash ^= keys[768 + lsb(pos.ep)];
	}

	// Castling permissions
	hash ^= keys[832 + (pos.castling[0] | pos.castling[1] << 1 | pos.castling[2] << 2 | pos.castling[3] << 3)];

	return hash;
}

static bool IsPseudolegalMove(const Position& pos, const Move& move) {
	Move moves[256];
	const int num_moves = MoveGen(pos, moves, false);
	for (int i = 0; i < num_moves; ++i) {
		if (moves[i] == move) {
			return true;
		}
	}
	return false;
}

static void PrintPv(const Position& pos, const Move move, vector<U64>& hash_history) {
	// Check move pseudolegality
	if (!IsPseudolegalMove(pos, move)) {
		return;
	}

	// Check move legality
	auto npos = pos;
	if (!MakeMove(npos, move)) {
		return;
	}

	// Print current move
	cout << " " << MoveToUci(move, pos.flipped);

	// Probe the TT in the resulting position
	const U64 tt_key = GetHash(npos);
	const TT_Entry& tt_entry = transposition_table[tt_key % num_tt_entries];

	// Only continue if the move was valid and comes from a PV search
	if (tt_entry.key != tt_key || tt_entry.move == Move{} || tt_entry.flag != 0) {
		return;
	}

	// Avoid infinite recursion on a repetition
	for (const auto old_hash : hash_history) {
		if (old_hash == tt_key) {
			return;
		}
	}

	hash_history.emplace_back(tt_key);
	PrintPv(npos, tt_entry.move, hash_history);
	hash_history.pop_back();
}

static int Permill() {
	int pm = 0;
	for (int n = 0; n < 1000; n++) {
		if (transposition_table[n].key)
			pm++;
	}
	return pm;
}

static string ShowScore(string result) {
	int len = 16 - (int)result.length();
	if (len < 0)
		len = 0;
	result.append(len, ' ');
	return result;
}

static int ScoreToValue(int score) {
	int mgWeight = phase;
	int egWeight = 24 - mgWeight;
	return (mgWeight * Mg(score) + egWeight * Eg(score)) / 24;
}

static string ShowScore(int s) {
	int v = ScoreToValue(s);
	return ShowScore(to_string(v) + " (" + to_string(Mg(s)) + " " + to_string((int)Eg(s)) + ")");
}

static void PrintTerm(string name, int idx) {
	int sw = scores[idx][0];
	int sb = scores[idx][1];
	std::cout << ShowScore(name) << ShowScore(sw) << " " << ShowScore(sb) << " " << ShowScore(sw - sb) << endl;
}

static void PrintEval(Position &pos) {
	PrintBoard(pos);
	cout << "side " << (pos.flipped ? "black" : "white") << endl;
	int score = Eval(pos);
	PrintTerm("Pawn", PAWN);
	PrintTerm("Knight", KNIGHT);
	PrintTerm("Bishop", BISHOP);
	PrintTerm("Rook", ROOK);
	PrintTerm("Queen", QUEEN);
	PrintTerm("King", KING);
	PrintTerm("Passed", PASSED);
	PrintTerm("Structure", STRUCTURE);
	cout << "phase " << phase << endl;
	cout << "score " << score << endl;
}

static int SearchQuiescence(Position& pos,
	int alpha,
	const int beta,
	const int ply,
	U64& nodes,
	const int64_t stop_time,
	Stack* const stack,
	int64_t(&hh_table)[2][64][64]) {

	// Exit early if out of time
	if (ply > 0 && Now() >= stop_time) { return 0; }

	const int static_eval = Eval(pos);
	//cout << "sq static " << static_eval << endl;
	// Don't overflow the stack
	if (ply > 127) {
		return static_eval;
	}
	stack[ply].score = static_eval;
	// Check extensions
	const auto in_check = Attacked(pos, lsb(pos.color[0] & pos.pieces[KING]));
	const int improving = ply > 1 && static_eval > stack[ply - 2].score;
	if (static_eval > alpha) {
		if (static_eval >= beta) {
			return beta;
		}
		alpha = static_eval;
	}

	const U64 tt_key = GetHash(pos);

	// TT Probing
	TT_Entry& tt_entry = transposition_table[tt_key % num_tt_entries];
	Move tt_move{};
	if (tt_entry.key == tt_key) {
		tt_move = tt_entry.move;
		if (ply > 0) {
			if (tt_entry.flag == 0) {
				return tt_entry.score;
			}
			if (tt_entry.flag == 1 && tt_entry.score <= alpha) {
				return tt_entry.score;
			}
			if (tt_entry.flag == 2 && tt_entry.score >= beta) {
				return tt_entry.score;
			}
		}
	}

	auto& moves = stack[ply].moves;
	const int num_moves = MoveGen(pos, moves, true);

	// Score moves
	int64_t move_scores[256];
	for (int j = 0; j < num_moves; ++j) {
		const int capture = PieceTypeOn(pos, moves[j].to);
		if (moves[j] == tt_move) {
			move_scores[j] = 1LL << 62;
		}
		else if (capture != PT_NB) {
			move_scores[j] = ((capture + 1) * (1LL << 54)) - PieceTypeOn(pos, moves[j].from);
		}
		else if (moves[j] == stack[ply].killer) {
			move_scores[j] = 1LL << 50;
		}
		else {
			move_scores[j] = hh_table[pos.flipped][moves[j].from][moves[j].to];
		}
	}

	int moves_evaluated = 0;
	int best_score = -INF;
	Move best_move{};
	uint16_t tt_flag = lowerbound;
	hash_history.emplace_back(tt_key);
	for (int i = 0; i < num_moves; ++i) {
		// Find best move remaining
		int best_move_index = i;
		for (int j = i; j < num_moves; ++j) {
			if (move_scores[j] > move_scores[best_move_index]) {
				best_move_index = j;
			}
		}

		const auto move = moves[best_move_index];
		const auto best_move_score = move_scores[best_move_index];

		moves[best_move_index] = moves[i];
		move_scores[best_move_index] = move_scores[i];

		// Delta pruning
		/*if (!in_check && static_eval + 50 + max_material[PieceTypeOn(pos, move.to)] < alpha) {
			best_score = alpha;
			break;
		}*/

		Position npos = pos;
		if (!MakeMove(npos, move)) {
			continue;
		}
		nodes++;

		int score = -SearchQuiescence(npos,
			-beta,
			-alpha,
			ply + 1,
			nodes,
			stop_time,
			stack,
			hh_table);
		moves_evaluated++;

		// Exit early if out of time
		if (Now() >= stop_time) { hash_history.pop_back(); return 0; }

		if (score > best_score) {
			best_score = score;
			best_move = move;
			if (score > alpha) {
				tt_flag = exact;
				alpha = score;
				stack[ply].move = move;
			}
		}

		if (alpha >= beta) {
			tt_flag = upperbound;
			break;
		}
	}
	hash_history.pop_back();

	// Return mate or draw scores if no moves found
	//if (best_score == -INF)return in_check ? ply - MATE_SCORE : 0;

	// Save to TT
	if (tt_entry.key != tt_key || !tt_entry.depth || tt_flag == exact) {
		tt_entry = TT_Entry{ tt_key, best_move == no_move ? tt_move : best_move, best_score,0, tt_flag };
	}
	//cout <<"sq alpha " << alpha << endl;
	return alpha;
}

static int SearchAlpha(Position& pos,
	int alpha,
	int beta,
	int depth,
	const int ply,
	U64& nodes,
	const int64_t stop_time,
	Stack* const stack,
	int64_t(&hh_table)[2][64][64],
	vector<U64>& hash_history,
	const int do_null = true) {
	const int static_eval = Eval(pos);
	// Don't overflow the stack
	if (ply > 127) {
		return static_eval;
	}
	stack[ply].score = static_eval;
	// Check extensions
	const auto in_check = Attacked(pos, lsb(pos.color[0] & pos.pieces[KING]));
	if (in_check)
		depth++;
	if (depth <= 0)
		return SearchQuiescence(pos, alpha, beta, ply, nodes, stop_time, stack, hh_table);

	// Exit early if out of time
	if (ply > 0 && Now() >= stop_time) { return 0; }

	const int improving = ply > 1 && static_eval > stack[ply - 2].score;

	int  mate_value = MATE_SCORE - ply;
	if (alpha < -mate_value)
		alpha = -mate_value;
	if (beta > mate_value - 1)
		beta = mate_value - 1;
	if (alpha >= beta) return alpha;

	const U64 tt_key = GetHash(pos);

	if (ply > 0) {
		// Repetition detection
		for (const auto old_hash : hash_history) {
			if (old_hash == tt_key) {
				return 0;
			}
		}
	}

	// TT Probing
	TT_Entry& tt_entry = transposition_table[tt_key % num_tt_entries];
	Move tt_move{};
	if (tt_entry.key == tt_key) {
		tt_move = tt_entry.move;
		if (ply > 0 && tt_entry.depth >= depth) {
			if (tt_entry.flag == 0) {
				return tt_entry.score;
			}
			if (tt_entry.flag == 1 && tt_entry.score <= alpha) {
				return tt_entry.score;
			}
			if (tt_entry.flag == 2 && tt_entry.score >= beta) {
				return tt_entry.score;
			}
		}
	}
	// Internal iterative reduction
	else if (depth > 3) {
		depth--;
	}

	auto& moves = stack[ply].moves;
	const int num_moves = MoveGen(pos, moves, false);

	// Score moves
	int64_t move_scores[256];
	for (int j = 0; j < num_moves; ++j) {
		const int capture = PieceTypeOn(pos, moves[j].to);
		if (moves[j] == tt_move) {
			move_scores[j] = 1LL << 62;
		}
		else if (capture != PT_NB) {
			move_scores[j] = ((capture + 1) * (1LL << 54)) - PieceTypeOn(pos, moves[j].from);
		}
		else if (moves[j] == stack[ply].killer) {
			move_scores[j] = 1LL << 50;
		}
		else {
			move_scores[j] = hh_table[pos.flipped][moves[j].from][moves[j].to];
		}
	}

	int quiet_moves_evaluated = 0;
	int moves_evaluated = 0;
	int best_score = -INF;
	Move best_move{};
	uint16_t tt_flag = lowerbound;
	hash_history.emplace_back(tt_key);
	for (int i = 0; i < num_moves; ++i) {
		// Find best move remaining
		int best_move_index = i;
		for (int j = i; j < num_moves; ++j) {
			if (move_scores[j] > move_scores[best_move_index]) {
				best_move_index = j;
			}
		}

		const auto move = moves[best_move_index];
		const auto best_move_score = move_scores[best_move_index];

		moves[best_move_index] = moves[i];
		move_scores[best_move_index] = move_scores[i];


		// Forward futility pruning
		/*if (!in_check && !(move == tt_move) &&
			static_eval + 150 * depth + max_material[PieceTypeOn(pos, move.to)] < alpha) {
			best_score = alpha;
			break;
		}*/

		Position npos = pos;
		if (!MakeMove(npos, move)) {
			continue;
		}

		nodes++;

		int score = -SearchAlpha(npos,
			-beta,
			-alpha,
			depth - 1,
			ply + 1,
			nodes,
			stop_time,
			stack,
			hh_table,
			hash_history);

		//cout <<"alpha " <<score << endl;
		moves_evaluated++;
		if (PieceTypeOn(pos, move.to) == PT_NB) {
			quiet_moves_evaluated++;
		}

		//if(!ply)cout << score << endl;

		// Exit early if out of time
		if (Now() >= stop_time) { hash_history.pop_back(); return 0; }

		if (score > best_score) {
			best_score = score;
			best_move = move;
			if (score > alpha) {
				tt_flag = exact;
				alpha = score;
				stack[ply].move = move;

				if (!ply) {
					cout << "info";
					cout << " depth " << depth;
					if (abs(score) < MATE_SCORE - MAX_DEPTH)
						cout << " score cp " << ValueToCp(score);
					else
						cout << " score mate " << (score > 0 ? (MATE_SCORE - score + 1) >> 1 : -(MATE_SCORE + score) >> 1);
					const auto elapsed = Now() - start_time;
					cout << " time " << elapsed;
					cout << " nodes " << nodes;
					if (elapsed > 0) {
						cout << " nps " << nodes * 1000 / elapsed;
					}
					cout << " hashfull " << Permill();
					cout << " currmovenumber " << moves_evaluated;
					cout << " pv";
					PrintPv(pos, stack[0].move, hash_history);
					cout << endl;
				}
			}
		}
		/*else if (!in_check && alpha == beta - 1 && depth <= 3 && moves_evaluated >= (depth * 3) + 2 &&
			static_eval < alpha - (50 * depth) && best_move_score < (1LL << 50)) {
			best_score = alpha;
			break;
		}*/

		if (alpha >= beta) {
			tt_flag = upperbound;
			const int capture = PieceTypeOn(pos, move.to);
			if (capture == PT_NB) {
				hh_table[pos.flipped][move.from][move.to] += depth * depth;
				stack[ply].killer = move;
			}
			break;
		}

		// Late move pruning based on quiet move count
		/*if (!in_check && alpha == beta - 1 && quiet_moves_evaluated > 3 + 2 * depth * depth) {
			break;
		}*/
	}
	hash_history.pop_back();

	// Return mate or draw scores if no moves found
	if (best_score == -INF) {
		return in_check ? ply - MATE_SCORE : 0;
	}

	// Save to TT
	if (tt_entry.key != tt_key || depth >= tt_entry.depth || tt_flag == exact) {
		tt_entry =
			TT_Entry{ tt_key, best_move == no_move ? tt_move : best_move, best_score, depth, tt_flag };
	}

	return alpha;
}

static Move SearchIteratively(Position& pos,vector<U64>& hash_history,U64 &nodes,const int allocated_time) {
	start_time = Now();
	TranspositionClear();
	Stack stack[128] = {};
	int64_t hh_table[2][64][64] = {};

	int score = 0;
	for (int i = 1; i < MAX_DEPTH; ++i) {
		auto window = 40;
		auto research = 0;
	research:
		const int newscore = SearchAlpha(pos,
			score - window,
			score + window,
			i,
			0,
			nodes,
			start_time + allocated_time,
			stack,
			hh_table,
			hash_history);

		// Hard time limit exceeded
		if (Now() >= start_time + allocated_time) {
			break;
		}
		U64 elapsed = Now() - start_time;

		cout << "info";
		cout << " depth " << i;
		if (abs(newscore) < MATE_SCORE - MAX_DEPTH)
			cout << " score cp " << ValueToCp(newscore);
		else
			cout << " score mate " << (newscore > 0 ? (MATE_SCORE - newscore + 1) >> 1 : -(MATE_SCORE + newscore) >> 1);
		if (newscore >= score + window) {
			cout << " lowerbound";
		}
		else if (newscore <= score - window) {
			cout << " upperbound";
		}
		cout << " time " << elapsed;
		cout << " nodes " << nodes;
		if (elapsed > 0) {
			cout << " nps " << nodes * 1000 / elapsed;
		}
		cout << " hashfull " << Permill();
		// Not a lowerbound - a fail low won't have a meaningful PV.
		if (newscore > score - window) {
			cout << " pv";
			PrintPv(pos, stack[0].move, hash_history);
		}
		cout << endl;

		if (newscore >= score + window || newscore <= score - window) {
			window <<= ++research;
			score = newscore;
			goto research;
		}

		score = newscore;

		// Early exit after completed ply
		if (!research && Now() > start_time + allocated_time / 2) {
			break;
		}
	}
	return stack[0].move;
}

static void SetFen(Position& pos, const string& fen) {
	if (fen == "startpos") {
		pos = Position();
		return;
	}

	// Clear
	pos.color = {};
	pos.pieces = {};
	pos.castling = {};

	stringstream ss{ fen };
	string word;

	ss >> word;
	int i = 56;
	for (const auto c : word) {
		if (c >= '1' && c <= '8') {
			i += c - '1' + 1;
		}
		else if (c == '/') {
			i -= 16;
		}
		else {
			const int side = c == 'p' || c == 'n' || c == 'b' || c == 'r' || c == 'q' || c == 'k';
			const int piece = (c == 'p' || c == 'P') ? PAWN
				: (c == 'n' || c == 'N') ? KNIGHT
				: (c == 'b' || c == 'B') ? BISHOP
				: (c == 'r' || c == 'R') ? ROOK
				: (c == 'q' || c == 'Q') ? QUEEN
				: KING;
			pos.color.at(side) ^= 1ULL << i;
			pos.pieces.at(piece) ^= 1ULL << i;
			i++;
		}
	}

	// Side to move
	ss >> word;
	const bool black_move = word == "b";

	// Castling permissions
	ss >> word;
	for (const auto c : word) {
		pos.castling[0] |= c == 'K';
		pos.castling[1] |= c == 'Q';
		pos.castling[2] |= c == 'k';
		pos.castling[3] |= c == 'q';
	}

	// En passant
	ss >> word;
	if (word != "-") {
		const int sq = word[0] - 'a' + 8 * (word[1] - '1');
		pos.ep = 1ULL << sq;
	}

	// Flip the board if necessary
	if (black_move) {
		flip(pos);
	}
}

static void SplitStr(const std::string& txt, std::vector<std::string>& vStr, char ch) {
	vStr.clear();
	if (txt == "")
		return;
	size_t pos = txt.find(ch);
	size_t initialPos = 0;
	while (pos != std::string::npos) {
		vStr.push_back(txt.substr(initialPos, pos - initialPos));
		initialPos = pos + 1;
		pos = txt.find(ch, initialPos);
	}
	vStr.push_back(txt.substr(initialPos, std::min(pos, txt.size()) - initialPos + 1));
}

static void SplitInt(const string& txt, vector<int>& vInt, char ch) {
	vInt.clear();
	vector<string> vs;
	SplitStr(txt, vs, ch);
	for (string s : vs)
		vInt.push_back(stoi(s));
}

static int GetVal(vector<int> v, int i) {
	if (i >= 0 && i < v.size())
		return v[i];
	return 0;
}

static void Init() {
	for (int f = FILE_A; f <= FILE_H; ++f)
		bbAdjacentFiles[f] = (f > FILE_A ? MASK_FILE[f - 1] : 0) | (f < FILE_H ? MASK_FILE[f + 1] : 0);
	for (int r = RANK_1; r <= RANK_8; ++r)
		bbRanks[r] = 0xFFULL << (r * 8);
	for (int f = FILE_A; f <= FILE_H; ++f)
		bbFiles[f] = FileABB << f;
	U64 bb = ~0ULL;
	for (int r = RANK_1; r <= RANK_8; ++r) {
		bb &= ~bbRanks[r];
		bbForwardRanks[r] = bb | bbRanks[r];
	}
	int mg, eg;
	vector<int> split{};
	int eloMod = 600 - (600 * options.elo) / 2500;
	for (int pt = PAWN; pt < PT_NB; pt++) {
		mg = PieceValue[0][pt] - eloMod;
		eg = PieceValue[1][pt];
		material[pt] = S(mg, eg);
		max_material[pt] = max(mg, eg);
	}

	SplitInt(options.tempo, split, ' ');
	mg = GetVal(split, 0);
	eg = GetVal(split, 1);
	tempo = S(mg, eg);

	for (int pt = PAWN; pt < PT_NB; ++pt)
		for (int r = RANK_1; r < RANK_NB; ++r)
			for (int f = FILE_A; f < FILE_NB; ++f)
			{
				int fi = std::min(int(f), 7 - f);
				bonus[pt][r][f] = material[pt];
				bonus[pt][r][f] += BonusOrg[pt][r][fi];
				bonusMax[pt][r][f] = ValueMax(bonus[pt][r][f]);
			}

	static constexpr int Seed[RANK_NB] = { 0, 13, 24, 18, 65, 100, 175, 330 };
	for (int opposed = 0; opposed <= 1; ++opposed)
		for (int phalanx = 0; phalanx <= 1; ++phalanx)
			for (int support = 0; support <= 2; ++support)
				for (int r = RANK_2; r < RANK_8; ++r)
				{
					int v = 17 * support;
					v += (Seed[r] + (phalanx ? (Seed[r + 1] - Seed[r]) / 2 : 0)) >> opposed;

					Connected[opposed][phalanx][support][r] = S(v, v * (r - 2) / 4);
				}
	for (int s1 = 0; s1 <= 63; ++s1)
		for (int s2 = 0; s2 <= 63; ++s2)
			if (s1 != s2)
			{
				int df = std::abs(FileOf(Square(s1)) - FileOf(Square(s2)));
				int dr = std::abs(RankOf(Square(s1)) - RankOf(Square(s2)));
				int d = std::max(df, dr);
				bbDistanceRing[s1][d] |= (1ULL << s2);
			}
}

static string StrToLower(string s) {
	transform(s.begin(), s.end(), s.begin(), ::tolower);
	return s;
}

static std::string trim(const std::string& s)
{
	if (s.empty())
		return s;
	auto start = s.begin();
	while (start != s.end() && std::isspace(*start)) {
		start++;
	}

	auto end = s.end();
	do {
		end--;
	} while (std::distance(start, end) > 0 && std::isspace(*end));

	return std::string(start, end + 1);
}

//Get next word after uci command
static bool UciValue(vector<string> list, string command, string& value) {
	value = "";
	for (size_t n = 0; n < list.size() - 1; n++)
		if (list[n] == command) {
			value = list[n + 1];
			return true;
		}
	return false;
}

static bool UciValues(vector<string> list, string command, string& value) {
	bool result = false;
	value = "";
	for (size_t n = 0; n < list.size(); n++) {
		if (result)
			value += " " + list[n];
		else if (list[n] == command)
			result = true;
	}
	value = trim(value);
	return result;
}

// Function to put thousands
// separators in the given integer
string ThousandSeparator(uint64_t n)
{
	string ans = "";

	// Convert the given integer
	// to equivalent string
	string num = to_string(n);

	// Initialise count
	int count = 0;

	// Traverse the string in reverse
	for (int i = (int)num.size() - 1; i >= 0; i--) {
		ans.push_back(num[i]);

		// If three characters
		// are traversed
		if (++count == 3) {
			ans.push_back(' ');
			count = 0;
		}
	}

	// Reverse the string to get
	// the desired output
	reverse(ans.begin(), ans.end());

	// If the given string is
	// less than 1000
	if (ans.size() % 4 == 0) {

		// Remove ','
		ans.erase(ans.begin());
	}

	return ans;
}

//Displays a summary
static void ShowInfo(uint64_t time, uint64_t nodes) {
	if (time < 1)
		time = 1;
	uint64_t nps = (nodes * 1000) / time;
	printf("-----------------------------\n");
	cout << "Time        : " << ThousandSeparator(time) << endl;
	cout << "Nodes       : " << ThousandSeparator(nodes) << endl;
	cout << "Nps         : " << ThousandSeparator(nps) << endl;
	printf("-----------------------------\n");
}

static void UciBench() {
	int stop = false;
	const auto start = Now();
	const auto allocated_time = 10000;
	U64 nodes = 0;
	SearchIteratively(pos,hash_history,nodes,allocated_time);
	ShowInfo(Now() - start, nodes);
}

static void UciQuit() {
	exit(0);
}

static void UciCommand(string str) {
	str = trim(str);
	string value;
	vector<string> split{};
	SplitStr(str, split, ' ');
	if (split.empty())
		return;
	string command = split[0];
	if (command == "uci")
	{
		cout << "id name " << NAME << endl;
		cout << "option name UCI_Elo type spin default " << options.eloMax << " min " << options.eloMin << " max " << options.eloMax << endl;
		cout << "option name hash type spin default " << (options.hash >> 15) << " min 1 max 65536" << endl;
		cout << "uciok" << endl;
	}
	else if (command == "setoption")
	{
		string name;
		bool isValue = UciValues(split, "value", value);
		if (isValue && UciValue(split, "name", name)) {
			name = StrToLower(name);
			if (name == "uci_elo") {
				options.elo = stoi(value);
				Init();
			}
			else if (name == "hash") {
				options.hash = stoi(value);
				options.hash = min(max(options.hash, 1ULL), 0xffffULL) * 1000000 / sizeof(TT_Entry);
				transposition_table.clear();
				transposition_table.resize(options.hash);
				transposition_table.shrink_to_fit();
			}
		}
	}
	else if (command == "isready") {
		cout << "readyok" << endl;
	}
	else if (command == "ucinewgame") {
		memset(transposition_table.data(), 0, sizeof(TT_Entry) * transposition_table.size());
	}
	else if (command == "position") {
		pos = Position();
		hash_history.clear();
		int mark = 0;
		string pFen = "";
		vector<string> pMoves = {};
		for (int i = 1; i < split.size(); i++) {
			if (mark == 1)
				pFen += ' ' + split[i];
			if (mark == 2)
				pMoves.push_back(split[i]);
			if (split[i] == "fen")
				mark = 1;
			else if (split[i] == "moves")
				mark = 2;
		}
		pFen = trim(pFen);
		if (!pFen.empty())
			SetFen(pos, pFen == "" ? defFen : pFen);
		Move moves[256];
		for (string uci : pMoves) {
			const int num_moves = MoveGen(pos, moves, false);
			for (int i = 0; i < num_moves; ++i) {
				if (uci == MoveToUci(moves[i], pos.flipped)) {
					if (PieceTypeOn(pos, moves[i].to) != PT_NB || PieceTypeOn(pos, moves[i].from) == PAWN) {
						hash_history.clear();
					}
					else {
						hash_history.emplace_back(GetHash(pos));
					}
					MakeMove(pos, moves[i]);
					break;
				}
			}
		}
	}
	else if (command == "go") {
		int wtime = 0;
		int btime = 0;
		int mtime = 0;
		if (UciValue(split, "wtime", value))
			wtime = stoi(value);
		if (UciValue(split, "btime", value))
			btime = stoi(value);
		if (UciValue(split, "movetime", value))
			mtime = stoi(value);
		const auto allocated_time = mtime ? mtime : ((pos.flipped ? btime : wtime) / 30);
		U64 nodes = 0;
		const auto best_move = SearchIteratively(pos,hash_history,nodes,allocated_time);
		cout << "bestmove " << MoveToUci(best_move, pos.flipped) << endl << flush;
	}
	else if (command == "bench")
		UciBench();
	else if (command == "print")
		PrintBoard(pos);
	else if (command == "eval") {
		//SetFen(pos,tstFen);
		PrintEval(pos);
	}
	else if (command == "quit")
		UciQuit();
}

//main uci loop
static void UciLoop() {
	string line;
	while (true) {
		getline(cin, line);
		UciCommand(line);
	}
}

int main(const int argc, const char** argv) {
	PrintWelcome();
	Init();
	transposition_table.resize(options.hash);
	//UciCommand("position startpos moves c2c4 e7e5 b1c3 g8f6 g1f3 b8c6 e2e3 f8b4 d2d4 e5d4 e3d4 e8g8 f1e2 d7d5 e1g1 d5c4 e2c4 c8g4 a2a3 g4f3 d1f3 d8d4 a3b4 d4c4 b4b5 c6d8 a1a4 c4b3 f1d1 f8e8 c1g5 f6d7 g5d8 a8d8 f3d3 d7f8 d3d8 e8d8 d1d8 b3b2 c3d1 b2f6 d8a8 f6e5 d1e3 e5b5 a4a7 b5b1 e3f1 b1b6 f1e3 f7f6 h2h3 g8f7 e3d5 b6b1 g1h2 f8e6 d5c7 e6f4 c7e8 f7g6 a7a4 f4e2 a4a1 b1f5 f2f3 f5e5 h2h1 e2g3 h1h2 b7b5 a1a7 g3e2 h2h1 g6h6 a7d7 b5b4 d7d1 b4b3 e8d6 e2g3 h1g1 e5e2 d6f7 h6g6 f7h8 g6h5 a8d8 b3b2 d8d5 f6f5 h3h4 h5h4 d5d4 f5f4 d4f4 h4h5 f4d4 e2e3 g1h2 b2b1q d4d5 b1f5 h2g3 e3g5 g3f2 f5c2 f2e1 c2d1 d5d1 g5e3 e1f1 e3e8 d1d5 g7g5 d5d4 e8h8 d4e4 h5g6 f1e1 h7h5 e1f1 h5h4 f1e1 h8c3 e1f2 c3c5 f2f1 g6f5 e4g4 c5c1 f1f2 c1d2 f2f1 d2d3 f1f2 d3c2 f2f1 c2c3 f1e2 c3b2 e2f1 b2a3 f1e2 a3a2 e2f1 a2b3 f1e2 b3b5 e2f2 b5e5 f2f1 e5e3 g4e4 e3c1 f1f2 c1b2 f2g1 b2c3 g1f2 c3c5 f2f1 c5b5 f1e1 b5d3 e1f2 d3d6 f2f1 d6a6 f1e1 a6b6 e1f1 b6b1 f1f2 b1c2 f2f1 c2c7 f1e2 c7g3 e4g4 g3h2 e2f1 h2c7 f1e2 f5f6 g4e4 c7g3 e4g4 g3e5 g4e4 e5b2 e2f1 b2c1 f1f2 c1c5 f2f1 c5b5 f1e1 b5b1 e1f2 b1c2 f2g1 c2d1 g1f2 d1d2 f2f1 f6g6 f1g1 d2c1 g1f2 g6h5 e4g4 c1d2 f2f1 d2c3 f1f2 c3b2 f2f1 h4h3 g2h3 b2h2 f1e1 h2h3 e1e2 h3h2 e2e3 h5g6 g4c4 h2e5 e3f2 g6f5 c4g4 e5h2 f2f1 h2d2 g4e4 d2b2 f1g1 b2c1 g1g2 c1c6 g2f2 c6c2 f2g1 c2b2 g1f1 b2d2 f1g1 d2d1 g1f2 d1b3 f2g2 b3b8 g2h3 b8b2 h3g3 b2c2 e4e8 c2c7 g3g2 c7d6 e8e4 d6d3 g2f2 f5f6 f2g2 d3d2 g2g1 d2c2 g1f1 c2h2 e4g4 h2a2 g4e4 f6f5 f1g1 a2a3 g1f2 a3c5 f2g2 c5c3 g2f2 c3d3 f2g2 d3b1 e4e2 b1b7 g2g3 b7a6 e2d2 a6c4 g3f2 c4c5 f2g2 c5e5 d2d7 e5e2 g2g3 e2e3 g3g2 f5e6");
	//UciCommand("go wtime 1 btime 9146 winc 0 binc 0");
	//UciCommand("go movetime 3000");
	UciLoop();
}
