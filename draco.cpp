#include <iostream>
#include <sstream> 
#include <random>

using namespace std;

#define MAX_DEPTH 100
#define MATE 30000
#define INF 32000
#define U8 unsigned __int8
#define S16 signed __int16
#define U16 unsigned __int16
#define S32 signed __int32
#define S64 signed __int64
#define U64 unsigned __int64
#define NAME "Draco"
#define VERSION "2025-12-18"
#define START_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

enum Color { WHITE, BLACK, COLOR_NB };
enum PieceType { PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING, PT_NB };
enum Bound { LOWER, UPPER, EXACT };
enum Phase { MG, EG, PHASE_NB };
enum Term { PASSED = 6, STRUCTURE, TERM_NB };

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
const U64 MASK_FILE[8] = {
	0x101010101010101, 0x202020202020202, 0x404040404040404, 0x808080808080808,
	0x1010101010101010, 0x2020202020202020, 0x4040404040404040, 0x8080808080808080
};
enum File : int { FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H, FILE_NB };

static constexpr File operator++(File& f) { return f = File(int(f) + 1); }

static constexpr File operator~(File& f) {
	return File(f ^ FILE_H); // Horizontal flip FILE_A -> FILE_H
}

enum Rank : int { RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8, RANK_NB };

struct Position {
	int castling[4]{};
	U64 color[2]{};
	U64 pieces[6]{};
	U64 ep = 0x0ULL;
	bool flipped = false;
}pos;

struct Move {
	U8 from = 0;
	U8 to = 0;
	U8 promo = 0;
};

const Move no_move{};

struct Stack {
	Move moves[256];
	Move moves_evaluated[256];
	S64 moves_scores[256];
	Move move;
	Move killer;
	S32 score;
};

struct TT_Entry {
	U64 key;
	Move move;
	U8 flag;
	S16 score;
	S16 depth;
};

struct SSearchInfo {
	bool post = true;
	bool stop = false;
	int depthLimit = MAX_DEPTH;
	S64 timeStart = 0;
	S64 timeLimit = 0;
	U64 nodes = 0;
	U64 nodesLimit = 0;
}info;

struct SOptions {
	int elo = 2500;
	int eloMin = 0;
	int eloMax = 2500;
	int ttMb = 64;
	string tempo = "16 8";
}options;

int phase = 0;

static int S(const int mg, const int eg) {
	return (eg << 16) + mg;
}

const int phases[] = { 0, 1, 1, 2, 4, 0 };
int material[PT_NB] = {};
int max_material[PT_NB] = {};
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

U64 filesBB[8] = { FileABB,FileBBB,FileCBB,FileDBB,FileEBB,FileFBB,FileGBB,FileHBB };
U64 bbAdjacentFiles[FILE_NB];
U64 bbRanks[RANK_NB];
U64 bbFiles[FILE_NB];
U64 bbForwardRanks[RANK_NB];

U64 tt_count = 64ULL << 15;
vector<TT_Entry> tt;
U64 keys[848];
Stack stack[128]{};
S32 hh_table[2][2][64][64]{};
int hash_count = 0;
U64 hash_history[1024]{};
int scores[TERM_NB][2];
U64 bbDistanceRing[64][8];

void TTClear() {
	memset(tt.data(), 0, sizeof(TT_Entry) * tt.size());
}

void InitTT() {
	tt_count = (options.ttMb * 1000000) / sizeof(TT_Entry);
	tt.resize(tt_count);
	TTClear();
}

static bool IsRepetition(U64 hash) {
	for (int n = hash_count - 2; n >= 0; n -= 2)
		if (hash_history[n] == hash)
			return true;
	return false;
}

inline static S64 SqToBb(int sq) {
	if (sq < 0 || sq > 63)
		return 0;
	return 1ULL << sq;
}

static S64 GetTimeMs() {
	return (clock() * 1000) / CLOCKS_PER_SEC;
}

static U64 Flip(const U64 bb) {
	return _byteswap_uint64(bb);
}

inline static Square LSB(const U64 bb) {
	return (Square)_tzcnt_u64(bb);
}

static U64 Count(const U64 bb) {
	return _mm_popcnt_u64(bb);
}

static U64 East(const U64 bb) {
	return (bb << 1) & ~0x0101010101010101ULL;
}

static U64 West(const U64 bb) {
	return (bb >> 1) & ~0x8080808080808080ULL;
}

static U64 North(const U64 bb) {
	return bb << 8;
}

static U64 South(const U64 bb) {
	return bb >> 8;
}

static U64 NW(const U64 bb) {
	return North(West(bb));
}

static U64 NE(const U64 bb) {
	return North(East(bb));
}

static U64 SW(const U64 bb) {
	return South(West(bb));
}

static U64 SE(const U64 bb) {
	return South(East(bb));
}

U64 Span(U64 bb) {
	return bb | bb >> 8 | bb >> 16 | bb >> 24 | bb >> 32;
}

U64 SpanNorth(U64 bb) {
	return bb | bb << 8 | bb << 16 | bb << 24 | bb << 32;
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

static bool MoreThanOne(U64 b) {
	return b & (b - 1);
}

static int TotalScore(int c) {
	int score = 0;
	for (int n = 0; n < TERM_NB; n++)
		score += scores[n][c];
	return score;
}

static void FlipPosition(Position& pos) {
	pos.color[0] = Flip(pos.color[0]);
	pos.color[1] = Flip(pos.color[1]);
	for (int i = 0; i < 6; ++i) {
		pos.pieces[i] = Flip(pos.pieces[i]);
	}
	pos.ep = Flip(pos.ep);
	swap(pos.color[0], pos.color[1]);
	swap(pos.castling[0], pos.castling[2]);
	swap(pos.castling[1], pos.castling[3]);
	pos.flipped = !pos.flipped;
}

auto operator==(const Move& lhs, const Move& rhs) {
	return !memcmp(&rhs, &lhs, sizeof(Move));
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

static Move UciToMove(string& uci, int flip) {
	Move m;
	m.from = (uci[0] - 'a');
	int f = (uci[1] - '1');
	m.from += 8 * (flip ? 7 - f : f);
	m.to = (uci[2] - 'a');
	f = (uci[3] - '1');
	m.to += 8 * (flip ? 7 - f : f);
	m.promo = PT_NB;
	switch (uci[4]) {
	case 'N':
	case 'n':
		m.promo = KNIGHT;
		break;
	case 'B':
	case 'b':
		m.promo = BISHOP;
		break;
	case 'R':
	case 'r':
		m.promo = ROOK;
		break;
	case 'Q':
	case 'q':
		m.promo = QUEEN;
		break;
	}
	return m;
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

static void ResetLimit() {
	info.post = true;
	info.stop = false;
	info.nodes = 0;
	info.depthLimit = MAX_DEPTH;
	info.nodesLimit = 0;
	info.timeLimit = 0;
	info.timeStart = GetTimeMs();
}

template <typename F>
U64 Ray(const U64 bb, const U64 blockers, F f) {
	U64 mask = f(bb);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	mask |= f(mask & ~blockers);
	return mask;
}

static U64 BbKnightAttack(const U64 bb) {
	return (((bb << 15) | (bb >> 17)) & 0x7F7F7F7F7F7F7F7FULL) | (((bb << 17) | (bb >> 15)) & 0xFEFEFEFEFEFEFEFEULL) |
		(((bb << 10) | (bb >> 6)) & 0xFCFCFCFCFCFCFCFCULL) | (((bb << 6) | (bb >> 10)) & 0x3F3F3F3F3F3F3F3FULL);
}

static U64 KnightAttack(const int sq, const U64) {
	return BbKnightAttack(1ULL << sq);
}

static U64 BbBishopAttack(const U64 bb, const U64 blockers) {
	return Ray(bb, blockers, NW) | Ray(bb, blockers, NE) | Ray(bb, blockers, SW) | Ray(bb, blockers, SE);
}

static U64 BishopAttack(const int sq, const U64 blockers) {
	return BbBishopAttack(1ULL << sq, blockers);
}

static U64 BbRookAttack(const U64 bb, const U64 blockers) {
	return Ray(bb, blockers, North) | Ray(bb, blockers, East) | Ray(bb, blockers, South) | Ray(bb, blockers, West);
}

static U64 RookAttack(const int sq, const U64 blockers) {
	return BbRookAttack(1ULL << sq, blockers);
}

static U64 KingAttack(const int sq, const U64) {
	const U64 bb = 1ULL << sq;
	return (bb << 8) | (bb >> 8) |
		(((bb >> 1) | (bb >> 9) | (bb << 7)) & 0x7F7F7F7F7F7F7F7FULL) |
		(((bb << 1) | (bb << 9) | (bb >> 7)) & 0xFEFEFEFEFEFEFEFEULL);
}

static bool IsAttacked(const Position& pos, const int sq, const int them = true) {
	const U64 bb = 1ULL << sq;
	const U64 kt = pos.color[them] & pos.pieces[KNIGHT];
	const U64 BQ = pos.pieces[BISHOP] | pos.pieces[QUEEN];
	const U64 RQ = pos.pieces[ROOK] | pos.pieces[QUEEN];
	const U64 pawns = pos.color[them] & pos.pieces[PAWN];
	const U64 pawn_attacks = them ? SW(pawns) | SE(pawns) : NW(pawns) | NE(pawns);
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
	pos.color[0] ^= from | to;
	pos.pieces[piece] ^= from | to;
	if (piece == PAWN && to == pos.ep) {
		pos.color[1] ^= to >> 8;
		pos.pieces[PAWN] ^= to >> 8;
	}
	pos.ep = 0x0ULL;
	if (piece == PAWN && move.to - move.from == 16) {
		pos.ep = to >> 8;
	}
	if (captured != PT_NB) {
		pos.color[1] ^= to;
		pos.pieces[captured] ^= to;
	}
	if (piece == KING) {
		const U64 bb = move.to - move.from == 2 ? 0xa0ULL : move.to - move.from == -2 ? 0x9ULL : 0x0ULL;
		pos.color[0] ^= bb;
		pos.pieces[ROOK] ^= bb;
	}
	if (piece == PAWN && move.to >= 56) {
		pos.pieces[PAWN] ^= to;
		pos.pieces[move.promo] ^= to;
	}
	pos.castling[0] &= !((from | to) & 0x90ULL);
	pos.castling[1] &= !((from | to) & 0x11ULL);
	pos.castling[2] &= !((from | to) & 0x9000000000000000ULL);
	pos.castling[3] &= !((from | to) & 0x1100000000000000ULL);
	FlipPosition(pos);
	return !IsAttacked(pos, (int)LSB(pos.color[1] & pos.pieces[KING]), false);
}

static void add_move(Move* const movelist, int& num_moves, const U8 from, const U8 to, const U8 promo = PT_NB) {
	movelist[num_moves++] = Move{ from, to, promo };
}

static void generate_pawn_moves(Move* const movelist, int& num_moves, U64 to_mask, const int offset) {
	while (to_mask) {
		const int to = (int)LSB(to_mask);
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

static void generate_piece_moves(Move* const movelist, int& num_moves, const Position& pos, const int piece, const U64 to_mask, U64(*func)(int, U64)) {
	U64 copy = pos.color[0] & pos.pieces[piece];
	while (copy) {
		const int fr = LSB(copy);
		copy &= copy - 1;
		U64 moves = func(fr, pos.color[0] | pos.color[1]) & to_mask;
		while (moves) {
			const int to = LSB(moves);
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
	generate_pawn_moves(movelist, num_moves, NW(pawns) & (pos.color[1] | pos.ep), -7);
	generate_pawn_moves(movelist, num_moves, NE(pawns) & (pos.color[1] | pos.ep), -9);
	generate_piece_moves(movelist, num_moves, pos, KNIGHT, to_mask, KnightAttack);
	generate_piece_moves(movelist, num_moves, pos, BISHOP, to_mask, BishopAttack);
	generate_piece_moves(movelist, num_moves, pos, QUEEN, to_mask, BishopAttack);
	generate_piece_moves(movelist, num_moves, pos, ROOK, to_mask, RookAttack);
	generate_piece_moves(movelist, num_moves, pos, QUEEN, to_mask, RookAttack);
	generate_piece_moves(movelist, num_moves, pos, KING, to_mask, KingAttack);
	if (!only_captures && pos.castling[0] && !(all & 0x60ULL) && !IsAttacked(pos, 4) && !IsAttacked(pos, 5)) {
		add_move(movelist, num_moves, 4, 6);
	}
	if (!only_captures && pos.castling[1] && !(all & 0xEULL) && !IsAttacked(pos, 4) && !IsAttacked(pos, 3)) {
		add_move(movelist, num_moves, 4, 2);
	}
	return num_moves;
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

static auto GetHash(const Position& pos) {
	U64 hash = pos.flipped;
	for (S32 p = PAWN; p < PT_NB; ++p) {
		U64 copy = pos.pieces[p] & pos.color[0];
		while (copy) {
			const S32 sq = LSB(copy);
			copy &= copy - 1;
			hash ^= keys[p * 64 + sq];
		}
		copy = pos.pieces[p] & pos.color[1];
		while (copy) {
			const S32 sq = LSB(copy);
			copy &= copy - 1;
			hash ^= keys[p * 64 + sq + 6 * 64];
		}
	}
	if (pos.ep)
		hash ^= keys[12 * 64 + LSB(pos.ep)];
	hash ^= keys[13 * 64 + pos.castling[0] + pos.castling[1] * 2 + pos.castling[2] * 4 + pos.castling[3] * 8];
	return hash;
}

static void CheckUp() {
	if ((info.timeLimit && GetTimeMs() - info.timeStart > info.timeLimit) || (info.nodesLimit && info.nodes > info.nodesLimit))
		info.stop = true;
}

static bool IsPseudolegalMove(const Position& pos, const Move& move) {
	Move moves[256];
	const int num_moves = MoveGen(pos, moves, false);
	for (int i = 0; i < num_moves; ++i)
		if (moves[i] == move)
			return true;
	return false;
}

static void PrintPv(const Position& pos, const Move move) {
	if (!IsPseudolegalMove(pos, move))
		return;
	auto npos = pos;
	if (!MakeMove(npos, move))
		return;
	cout << " " << MoveToUci(move, pos.flipped);
	const U64 tt_key = GetHash(npos);
	const TT_Entry& tt_entry = tt[tt_key % tt_count];
	if (tt_entry.key != tt_key || tt_entry.move == Move{} || tt_entry.flag != EXACT) {
		return;
	}
	if (IsRepetition(tt_key))
		return;
	hash_history[hash_count++] = tt_key;
	PrintPv(npos, tt_entry.move);
	hash_count--;
}

static int Popcount(const U64 bb) {
	return (int)__popcnt64(bb);
}

static int Permill() {
	int pm = 0;
	for (int n = 0; n < 1000; n++) {
		if (tt[n].key)
			pm++;
	}
	return pm;
}

//prints the bitboard
static void PrintBitboard(U64 bb) {
	const char* s = "   +---+---+---+---+---+---+---+---+\n";
	const char* t = "     A   B   C   D   E   F   G   H\n";
	cout << t;
	for (int i = 56; i >= 0; i -= 8) {
		cout << s << " " << i / 8 + 1 << " ";
		for (int x = 0; x < 8; x++) {
			const char* c = 1LL << (i + x) & bb ? "x" : " ";
			cout << "| " << c << " ";
		}
		cout << "| " << i / 8 + 1 << endl;
	}
	cout << s;
	cout << t << endl;
}

//prints the board
static void PrintBoard(Position& pos) {
	Position np = pos;
	if (np.flipped)
		FlipPosition(np);
	const char* s = "   +---+---+---+---+---+---+---+---+\n";
	const char* t = "     A   B   C   D   E   F   G   H\n";
	cout << t;
	for (int i = 56; i >= 0; i -= 8) {
		cout << s << " " << i / 8 + 1 << " ";
		for (int j = 0; j < 8; j++) {
			int sq = i + j;
			int piece = PieceTypeOn(np, sq);
			if (np.color[0] & 1ull << sq)
				cout << "| " << "ANBRQK "[piece] << " ";
			else
				cout << "| " << "anbrqk "[piece] << " ";
		}
		cout << "| " << i / 8 + 1 << endl;
	}
	cout << s;
	cout << t << endl;
	char castling[5] = "KQkq";
	for (int n = 0; n < 4; n++)
		if (!np.castling[n])
			castling[n] = '-';
	printf("side     : %10s\n", pos.flipped ? "black" : "white");
	printf("castling : %10s\n", castling);
}

static int ShrinkNumber(U64 n) {
	if (n < 10000)
		return 0;
	if (n < 10000000)
		return 1;
	if (n < 10000000000)
		return 2;
	return 3;
}

//displays a summary
static void PrintSummary(U64 time, U64 nodes) {
	if (time < 1)
		time = 1;
	U64 nps = (nodes * 1000) / time;
	const char* units[] = { "", "k", "m", "g" };
	int sn = ShrinkNumber(nps);
	U64 p = pow(10, sn * 3);
	printf("-----------------------------\n");
	printf("Time        : %llu\n", time);
	printf("Nodes       : %llu\n", nodes);
	printf("Nps         : %llu (%llu%s/s)\n", nps, nps / p, units[sn]);
	printf("-----------------------------\n");
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
	File center = max(FILE_B, min(FILE_G, FileOf(ksq)));
	Value safety = (South(bbPawnsEn) & (FileABB | FileHBB) & (Rank1BB | Rank2BB) & ksq) ? Value(374) : Value(5);
	for (File f = File(center - 1); f <= File(center + 1); ++f)
	{
		bb = bbPawnsUs & bbFiles[f];
		int rankUs = bb ? RankOf(LSB(bb)) : 0;

		bb = bbPawnsEn & bbFiles[f];
		int rankEn = bb ? RankOf(LSB(bb)) : 0;

		int d = min(f, ~f);
		safety += ShelterStrength[d][rankUs];
		safety -= (rankUs && (rankUs == rankEn - 1)) ? 66 * (rankEn == RANK_3) : UnblockedStorm[d][rankEn];
	}
	return safety;
}

static Value KingSafety(Position& pos, Square ksq) {
	Value bonus = EvalShelter(pos, ksq);
	if (pos.castling[0])
		bonus = max(bonus, EvalShelter(pos, SQ_G1));
	if (pos.castling[1])
		bonus = max(bonus, EvalShelter(pos, SQ_C1));
	return bonus;
}

static int EvalPosition(Position& pos) {
	std::memset(scores, 0, sizeof(scores));
	int score = tempo;
	int ptCount[2][6] = {};
	phase = 0;
	for (int c = 0; c < 2; ++c) {
		U64 bbAll = pos.color[0] | pos.color[1];
		const U64 bbPawnsUs = pos.color[0] & pos.pieces[PAWN];
		const U64 bbPawnsEn = pos.color[1] & pos.pieces[PAWN];
		const U64 bbPawnDefense = NW(bbPawnsUs) | NE(bbPawnsUs);
		const U64 bbPawnAttack = SE(bbPawnsEn) | SW(bbPawnsEn);
		const U64 bbSpan = Span(bbPawnAttack);
		const U64 bbOutpost = ~bbSpan & bbOutpostRanks;
		const Square sqKUs = LSB(pos.color[0] & pos.pieces[KING]);
		const Square sqKEn = LSB(pos.color[1] & pos.pieces[KING]);
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
				const Square sq = LSB(copy);
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
						bbFront |= NE(bbFront) | NW(bbFront);
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
		FlipPosition(pos);
		score = -score;
	}
	const int pieceCount[2][6] = {
  { ptCount[0][2] > 1,ptCount[0][0],ptCount[0][1],
	ptCount[0][2],ptCount[0][3],ptCount[0][4]},
	{ ptCount[1][2] > 1,ptCount[1][0],ptCount[1][1],
	ptCount[1][2],ptCount[1][3],ptCount[1][4]} };
	int imbalanceUs = Imbalance(0, pieceCount);
	int imbalanceEn = Imbalance(1, pieceCount);
	int imbalance = (imbalanceUs - imbalanceEn) / 16;
	score += S(imbalance, imbalance);
	return (Mg(score) * phase + Eg(score) * (24 - phase)) / 24;
}

static string StrToLower(string s) {
	transform(s.begin(), s.end(), s.begin(), ::tolower);
	return s;
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
	vStr.push_back(txt.substr(initialPos, min(pos, txt.size()) - initialPos + 1));
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

static void InitEval() {
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
	int elo = options.elo;
	if (elo < options.eloMin)
		elo = options.eloMin;
	if (elo > options.eloMax)
		elo = options.eloMax;
	elo -= options.eloMin;
	int eloRange = options.eloMax - options.eloMin;
	int eloMod = QueenValueMg * 2;
	eloMod -= (eloMod * elo) / eloRange;
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
				int fi = min(int(f), 7 - f);
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
				int d = max(df, dr);
				bbDistanceRing[s1][d] |= (1ULL << s2);
			}
}

static int SearchAlpha(Position& pos, int alpha, const int beta, int depth, const int ply, Stack* const stack, const bool do_null = true) {
	if ((++info.nodes & 0xffff) == 0)
		CheckUp();
	if (info.stop)
		return 0;

	int static_eval = EvalPosition(pos);
	if (ply > 127)
		return static_eval;
	stack[ply].score = static_eval;

	// Check extensions
	const S32 in_check = IsAttacked(pos, LSB(pos.color[0] & pos.pieces[KING]));
	depth += in_check;

	bool in_qsearch = depth <= 0;
	const U64 tt_key = GetHash(pos);

	if (ply > 0 && !in_qsearch)
		if (IsRepetition(tt_key))
			return 0;

	// TT Probing
	TT_Entry& tt_entry = tt[tt_key % tt_count];
	Move tt_move{};
	if (tt_entry.key == tt_key) {
		tt_move = tt_entry.move;
		if (alpha == beta - 1 && tt_entry.depth >= depth) {
			if (tt_entry.flag == EXACT)
				return tt_entry.score;
			if (tt_entry.flag == LOWER && tt_entry.score <= alpha)
				return tt_entry.score;
			if (tt_entry.flag == UPPER && tt_entry.score >= beta)
				return tt_entry.score;
		}
	}
	// Internal iterative reduction
	else
		depth -= depth > 3;

	const S32 improving = ply > 1 && static_eval > stack[ply - 2].score;

	// If static_eval > tt_entry.score, tt_entry.flag cannot be Lower (ie must be Upper or Exact).
	// Otherwise, tt_entry.flag cannot be Upper (ie must be Lower or Exact).
	if (tt_entry.key == tt_key && tt_entry.flag != static_eval > tt_entry.score)
		static_eval = tt_entry.score;

	if (in_qsearch && static_eval > alpha) {
		if (static_eval >= beta)
			return static_eval;
		alpha = static_eval;
	}

	if (ply > 0 && !in_qsearch && !in_check && alpha == beta - 1) {
		// Reverse futility pruning
		if (depth < 8) {
			if (static_eval - 71 * (depth - improving) >= beta)
				return static_eval;

			in_qsearch = static_eval + 238 * depth < alpha;
		}

		// Null move pruning
		if (depth > 2 && static_eval >= beta && static_eval >= stack[ply].score && do_null &&
			pos.color[0] & ~pos.pieces[PAWN] & ~pos.pieces[KING]) {
			Position npos = pos;
			FlipPosition(npos);
			npos.ep = 0;
			if (-SearchAlpha(npos,
				-beta,
				-alpha,
				depth - 4 - depth / 5 - min((static_eval - beta) / 196, 3),
				ply + 1,
				stack,
				false) >= beta)
				return beta;
		}
	}

	hash_history[hash_count++] = tt_key;
	U8 tt_flag = LOWER;

	S32 num_moves_evaluated = 0;
	S32 num_moves_quiets = 0;
	S32 best_score = in_qsearch ? static_eval : -INF;
	auto best_move = tt_move;

	auto& moves = stack[ply].moves;
	auto& moves_scores = stack[ply].moves_scores;
	auto& moves_evaluated = stack[ply].moves_evaluated;
	const S32 num_moves = MoveGen(pos, moves, in_qsearch);

	for (S32 i = 0; i < num_moves; ++i) {
		// Score moves at the first loop, except if we have a hash move,
		// then we'll use that first and delay sorting one iteration.
		if (i == !(no_move == tt_move))
			for (S32 j = 0; j < num_moves; ++j) {
				const S32 gain = max_material[moves[j].promo] + max_material[PieceTypeOn(pos, moves[j].to)];
				moves_scores[j] = hh_table[pos.flipped][!gain][moves[j].from][moves[j].to] +
					(gain || moves[j] == stack[ply].killer) * 2048 + gain;
			}

		// Find best move remaining
		S32 best_move_index = i;
		for (S32 j = i; j < num_moves; ++j) {
			if (moves[j] == tt_move) {
				best_move_index = j;
				break;
			}
			if (moves_scores[j] > moves_scores[best_move_index])
				best_move_index = j;
		}

		const Move move = moves[best_move_index];
		moves[best_move_index] = moves[i];
		moves_scores[best_move_index] = moves_scores[i];

		// Material gain
		const S32 gain = max_material[move.promo] + max_material[PieceTypeOn(pos, move.to)];

		// Delta pruning
		if (in_qsearch && !in_check && static_eval + 50 + gain < alpha)
			break;

		// Forward futility pruning
		if (ply > 0 && depth < 8 && !in_qsearch && !in_check && num_moves_evaluated && static_eval + 105 * depth + gain < alpha)
			break;

		Position npos = pos;
		if (!MakeMove(npos, move))
			continue;

		S32 score;
		S32 reduction = depth > 3 && num_moves_evaluated > 1
			? max(num_moves_evaluated / 13 + depth / 14 + (alpha == beta - 1) + !improving -
				min(max(hh_table[pos.flipped][!gain][move.from][move.to] / 128, -2), 2),
				0)
			: 0;

		while (num_moves_evaluated &&
			(score = -SearchAlpha(npos,
				-alpha - 1,
				-alpha,
				depth - reduction - 1,
				ply + 1,
				stack)) > alpha &&
			reduction > 0)
			reduction = 0;

		if (!num_moves_evaluated || score > alpha && score < beta)
			score = -SearchAlpha(npos,
				-beta,
				-alpha,
				depth - 1,
				ply + 1,
				stack);

		// Exit early if out of time
		if (info.stop) {
			hash_count--;
			return 0;
		}

		if (score > best_score)
			best_score = score;

		if (score > alpha) {
			best_move = move;
			tt_flag = EXACT;
			alpha = score;
			stack[ply].move = move;
			if (!ply && info.post) {
				cout << "info";
				cout << " depth " << depth;
				if (abs(score) < MATE - MAX_DEPTH)
					cout << " score cp " << score;
				else
					cout << " score mate " << (score > 0 ? (MATE - score + 1) >> 1 : -(MATE + score) >> 1);
				const auto elapsed = GetTimeMs() - info.timeStart;
				cout << " time " << elapsed;
				cout << " nodes " << info.nodes;
				cout << " hashfull " << Permill();
				cout << " pv";
				PrintPv(pos, stack[0].move);
				cout << endl;
			}
			if (score >= beta) {
				tt_flag = UPPER;

				if (!gain)
					stack[ply].killer = move;

				hh_table[pos.flipped][!gain][move.from][move.to] +=
					depth * depth - depth * depth * hh_table[pos.flipped][!gain][move.from][move.to] / 512;
				for (S32 j = 0; j < num_moves_evaluated; ++j) {
					const S32 prev_gain = max_material[moves_evaluated[j].promo] + max_material[PieceTypeOn(pos, moves_evaluated[j].to)];
					hh_table[pos.flipped][!prev_gain][moves_evaluated[j].from][moves_evaluated[j].to] -=
						depth * depth +
						depth * depth *
						hh_table[pos.flipped][!prev_gain][moves_evaluated[j].from][moves_evaluated[j].to] / 512;
				}
				break;
			}
		}

		moves_evaluated[num_moves_evaluated++] = move;
		if (!gain)
			num_moves_quiets++;
		if (!in_check && alpha == beta - 1 && num_moves_quiets > 1 + depth * depth >> !improving)
			break;
	}
	hash_count--;
	if (best_score == -INF)
		return in_check ? ply - MATE : 0;
	tt_entry = { tt_key, best_move, tt_flag,S16(best_score), S16(!in_qsearch * depth) };
	return best_score;
}

static void SearchIteratively(Position& pos) {
	memset(stack, 0, sizeof(stack));
	TTClear();
	for (int depth = 1; depth <= info.depthLimit; ++depth) {
		SearchAlpha(pos, -MATE, MATE, depth, 0, stack);
		if (info.stop)
			break;
		if (info.timeLimit && GetTimeMs() - info.timeStart > info.timeLimit / 2) {
			break;
		}
	}
	if (info.post)
		cout << "bestmove " << MoveToUci(stack[0].move, pos.flipped) << endl << flush;
}

static inline void PerftDriver(Position pos, int depth) {
	Move list[256];
	const S32 num_moves = MoveGen(pos, list, false);
	for (int n = 0; n < num_moves; n++) {
		Position npos = pos;
		if (!MakeMove(npos, list[n]))
			continue;
		if (depth)
			PerftDriver(npos, depth - 1);
		else
			info.nodes++;
	}
}

static void SetFen(Position& pos, const string& fen) {
	pos.flipped = false;
	pos.ep = 0;
	memset(pos.color, 0, sizeof(pos.color));
	memset(pos.pieces, 0, sizeof(pos.pieces));
	memset(pos.castling, 0, sizeof(pos.castling));
	stringstream ss(fen);
	string word;
	ss >> word;
	int i = 56;
	for (const auto c : word) {
		if (c >= '1' && c <= '8')
			i += c - '1' + 1;
		else if (c == '/')
			i -= 16;
		else {
			const int side = c == 'p' || c == 'n' || c == 'b' || c == 'r' || c == 'q' || c == 'k';
			const int piece = (c == 'p' || c == 'P') ? PAWN
				: (c == 'n' || c == 'N') ? KNIGHT
				: (c == 'b' || c == 'B') ? BISHOP
				: (c == 'r' || c == 'R') ? ROOK
				: (c == 'q' || c == 'Q') ? QUEEN
				: KING;
			pos.color[side] ^= 1ULL << i;
			pos.pieces[piece] ^= 1ULL << i;
			i++;
		}
	}
	ss >> word;
	const bool black_move = word == "b";
	ss >> word;
	for (const auto c : word) {
		pos.castling[0] |= c == 'K';
		pos.castling[1] |= c == 'Q';
		pos.castling[2] |= c == 'k';
		pos.castling[3] |= c == 'q';
	}
	ss >> word;
	if (word != "-") {
		const int sq = word[0] - 'a' + 8 * (word[1] - '1');
		pos.ep = 1ULL << sq;
	}
	if (black_move)
		FlipPosition(pos);
}

//start benchmark
static void UciBench() {
	ResetLimit();
	printf("Benchmark Test\n");
	SetFen(pos, START_FEN);
	info.depthLimit = 0;
	info.post = false;
	S64 elapsed = 0;
	while (elapsed < 3000)
	{
		++info.depthLimit;
		SearchIteratively(pos);
		elapsed = GetTimeMs() - info.timeStart;
		printf("%2d. %8llu %12llu\n", info.depthLimit, elapsed, info.nodes);
	}
	PrintSummary(GetTimeMs() - info.timeStart, info.nodes);
}

//start performance test
static void UciPerformance()
{
	ResetLimit();
	printf("Performance Test\n");
	int depth = 0;
	SetFen(pos, START_FEN);
	while (GetTimeMs() - info.timeStart < 3000)
	{
		PerftDriver(pos, depth++);
		printf("%2d. %8llu %12llu\n", depth, GetTimeMs() - info.timeStart, info.nodes);
	}
	PrintSummary(GetTimeMs() - info.timeStart, info.nodes);
}

static int ScoreToValue(int score) {
	int mgWeight = phase;
	int egWeight = 24 - mgWeight;
	return (mgWeight * Mg(score) + egWeight * Eg(score)) / 24;
}

static string ShowScore(string result) {
	int len = 16 - (int)result.length();
	if (len < 0)
		len = 0;
	result.append(len, ' ');
	return result;
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

static void UciEval() {
	SetFen(pos, "5rk1/ppp2ppp/8/4pP2/1P2Bb2/2P2K2/8/7R b - - 0 28");
	PrintBoard(pos);
	cout << "side " << (pos.flipped ? "black" : "white") << endl;
	int score = EvalPosition(pos);
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

static void ParsePosition(string command) {
	string fen = START_FEN;
	stringstream ss(command);
	string token;
	ss >> token;
	if (token != "position")
		return;
	ss >> token;
	if (token == "startpos")
		ss >> token;
	else if (token == "fen") {
		fen = "";
		while (ss >> token && token != "moves")
			fen += token + " ";
		fen.pop_back();
	}
	hash_count = 0;
	SetFen(pos, fen);
	while (ss >> token) {
		Move m = UciToMove(token, pos.flipped);
		if (PieceTypeOn(pos, m.to) != PT_NB || PieceTypeOn(pos, m.from) == PAWN)
			hash_count = 0;
		MakeMove(pos, m);
		hash_history[hash_count++] = GetHash(pos);
	}
}

static void ParseGo(string command) {
	stringstream ss(command);
	string token;
	ss >> token;
	if (token != "go")
		return;
	ResetLimit();
	int wtime = 0;
	int btime = 0;
	int winc = 0;
	int binc = 0;
	int movestogo = 32;
	while (ss >> token) {
		if (token == "wtime")
			ss >> wtime;
		else if (token == "btime")
			ss >> btime;
		else if (token == "winc")
			ss >> winc;
		else if (token == "binc")
			ss >> binc;
		else if (token == "movestogo")
			ss >> movestogo;
		else if (token == "movetime")
			ss >> info.timeLimit;
		else if (token == "depth")
			ss >> info.depthLimit;
		else if (token == "nodes")
			ss >> info.nodesLimit;
	}
	int time = pos.flipped ? btime : wtime;
	int inc = pos.flipped ? binc : winc;
	if (time)
		info.timeLimit = min(time / movestogo + inc, time / 2);
	SearchIteratively(pos);
}

static void UciCommand(string command) {
	if (command.empty())
		return;
	if (command == "uci")
	{
		cout << "id name " << NAME << endl;
		cout << "option name UCI_Elo type spin default " << options.eloMax << " min " << options.eloMin << " max " << options.eloMax << endl;
		cout << "option name hash type spin default " << options.ttMb << " min 1 max 1000" << endl;
		cout << "uciok" << endl;
	}
	else if (command == "isready")
		cout << "readyok" << endl;
	else if (command == "ucinewgame")
		memset(hh_table, 0, sizeof(hh_table));
	else if (command.substr(0, 8) == "position")
		ParsePosition(command);
	else if (command.substr(0, 2) == "go")
		ParseGo(command);
	else if (command == "setoption")
	{
		string word;
		cin >> word;
		cin >> word;
		word = StrToLower(word);
		if (word == "uci_elo") {
			cin >> word;
			cin >> options.elo;
			InitEval();
		}
		else if (word == "hash") {
			cin >> word;
			cin >> options.ttMb;
			InitTT();
		}
	}
	else if (command == "bench")
		UciBench();
	else if (command == "perft")
		UciPerformance();
	else if (command == "eval")
		UciEval();
	else if (command == "print")
		PrintBoard(pos);
	else if (command == "quit")
		exit(0);
}

static void UciLoop() {
	string line;
	while (true) {
		getline(cin, line);
		UciCommand(line);
	}
}

int main(const int argc, const char** argv) {
	cout << NAME << " " << VERSION << endl;
	mt19937_64 r;
	for (U64& k : keys)
		k = r();
	SetFen(pos, START_FEN);
	InitEval();
	InitTT();
	UciLoop();
}
