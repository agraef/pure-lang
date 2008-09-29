
#ifndef MATCHER_HH
#define MATCHER_HH

#include <iostream>
#include <string>
#include <cstring>
#include <list>
#include <vector>
#include "expr.hh"

using namespace std;

/* Data structures for the matching automaton. */

struct state;

// transitions
struct trans {
  int32_t tag;	// symbol, VAR or constant tag
  union {
    int32_t     i;	// EXPR::INT
    mpz_t       z;	// EXPR::BIGINT
    double      d;	// EXPR::DBL
    const char *s;	// EXPR::STR
  };
  state *st;	// successor state
  int8_t ttag;	// type tag

  explicit trans(int32_t _tag, int8_t _ttag = 0);
  trans(int32_t _tag, int32_t _i);
  trans(int32_t _tag, const mpz_t& _z);
  trans(int32_t _tag, double _d);
  trans(int32_t _tag, const char *_s);
  trans(const trans& tr);
  trans& operator = (const trans& tr);
  ~trans();

  size_t arity() const { return (tag == EXPR::APP)?2:0; }
  int polarity() const { return (tag <= 0)?-1:1; }

  bool operator == (const trans& tr) const
  {
    if (tag == tr.tag && ttag == tr.ttag)
      switch (tag) {
      case EXPR::INT:
	return i == tr.i;
      case EXPR::BIGINT:
	return mpz_cmp(z, tr.z) < 0;
      case EXPR::DBL:
	return d == tr.d;
      case EXPR::STR:
	return strcmp(s, tr.s) == 0;
      default:
	return true;
      }
    else
      return false;
  }
  bool operator < (const trans& tr) const
  {
    if (polarity() != tr.polarity())
      return polarity() < tr.polarity();
    else if (tag != tr.tag)
      return polarity()*tag < polarity()*tr.tag;
    else if (ttag != tr.ttag)
      return ttag > tr.ttag;
    else {
      assert(tag == tr.tag && ttag == tr.ttag);
      switch (tag) {
      case EXPR::INT:
	return i < tr.i;
      case EXPR::BIGINT:
	return mpz_cmp(z, tr.z) < 0;
      case EXPR::DBL:
	return d < tr.d;
      case EXPR::STR:
	return strcmp(s, tr.s) < 0;
      default:
	return false;
      }
    }
  }
};

// states
typedef uint32_t rulem;
typedef list<rulem> ruleml;
typedef list<trans> transl;
struct state {
  uint32_t s;	// state number
  ruleml r;	// rule markers
  transl tr;	// transitions
  state() :
    s(0), r(ruleml()), tr(transl()) {}
  state(const state& st) :
    s(st.s), r(st.r), tr(st.tr) {}
  state& operator = (const state& st)
  { s = st.s; r = st.r; tr = st.tr; return *this; }
};

/* The term matching automaton. */

typedef vector<state*> statev;
typedef vector<rule> rulev;

struct matcher {
  statev st;	// state table
  rulev r;	// rule table
  uint32_t s;	// number of states
  state *start;	// start state

  matcher()
    : st(statev()), r(rulev()), s(0), start(0) {}
  matcher(const rule& r, uint32_t skip = 0)
    : st(statev()), r(rulev()), s(0), start(0) { make(r, skip); }
  matcher(const rulel& rl, uint32_t skip = 0)
    : st(statev()), r(rulev()), s(0), start(0) { make(rl, skip); }

  /* Construction algorithm for the pattern matching automaton. */

  /* We employ the incremental technique described in Graef: Left-To-Right
     Tree Pattern Matching, Proc. RTA 1991, Springer 1991 (LNCS 488) to
     construct a tree automaton (TA) for the given patterns. The basic outline
     of the technique is as follows. Initially, the automaton is empty. From
     each pattern we produce a trie (considering the pattern as a string of
     variable and function symbols and constants). This trie is then merged
     with the TA obtained so far. The latter process is similar to merging two
     deterministic finite automata, but it also takes into account the
     variables (see the merge_state() routine below).

     The construction algorithm is implemented by the 'make' routine
     below. There are two variations of the routine, one taking a single rule
     and the other one taking an entire list of rules (in the former case, the
     resulting automaton will be just a trie). In either case the routine
     returns the start state of the resulting automaton, and stores it in the
     'start' member.

     The optional second argument 'skip' denotes the number of initial
     transitions to be skipped. This is useful for function definitions, where
     'skip' denotes the number of function parameters plus one and the
     automaton is used for the purpose of matching function arguments. In a
     pattern binding, where a singleton expression is to be matched, this
     value should always be 0.

     The 'make' routine can be invoked any number of times and will construct
     a new (sub-)automaton rooted at a new start state each time. This makes
     it possible to store several independent sub-automata in a single matcher
     instance. */

  state *make(const rule& r, uint32_t skip = 0);
  state *make(const rulel& rl, uint32_t skip = 0);

  /* Run the automaton from a given start state ('start' by default) to match
     a given subject expression (for the pattern binding case), or a given
     list of expressions (for the function definition case). Returns the final
     state if the match was successful, or a null pointer otherwise. */

  state *match(expr x)
  { assert(start!=0); return match(start, x); }
  state *match(const exprl& xs)
  { assert(start!=0); return match(start, xs); }
  state *match(state *st, expr x);
  state *match(state *st, const exprl& x);

private: // these are used internally by the TA construction algorithm

  /* Construct a trie from an expression tree. Takes the "start" and returns
     the "end" state of the (sub-)trie. The meaning of the 'skip' parameter is
     the same as for the 'make' function. */

  state *make_state(state *st, uint32_t r, expr x, uint32_t& skip);

  /* Take a copy of a state and prefix it with n variable transitions. */

  state *make_vstate(int n, state *st);

  /* Merge two tree automata. Merges the tree automaton rooted at st2 into the
     automaton rooted at st1. We assume that st2 is in "trie" form, i.e., each
     state has at most one transition, which is always guaranteed here and
     simplifies the algorithm. */

  void merge_state(state *st1, state *st2);
  void merge_rules(ruleml& r1, ruleml& r2);
  void merge_trans(transl& tr1, transl& tr2);
  void merge_ftrans(transl& tr, int32_t tag, state *st);
  void merge_vtrans(transl& tr, state *st) { merge_vtrans(tr, 0, st); }
  void merge_vtrans(transl& tr, int8_t ttag, state *st);
  void merge_ctrans(transl& tr, int32_t x, state *st);
  void merge_ctrans(transl& tr, const mpz_t& x, state *st);
  void merge_ctrans(transl& tr, double x, state *st);
  void merge_ctrans(transl& tr, const char *x, state *st);

  /* Finalize the automaton, given the desired start state. Assigns state
     numbers and builds the state table). */

  void build(state *st);
};

#endif // ! MATCHER_HH
