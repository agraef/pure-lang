/* stlbase.hpp -- Support for stl containers of pure_expr*
    
Copyright (c) 2011 by Peter Summerland <p.summerland@gmail.com>.

All rights reserved.

This software is is part of pure-stlvec, an addon to the Pure Programming
Language (http://code.google.com/p/pure-lang/).

This software is distributed under a BSD-style license in the hope that it
will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the COPYING file
included with the pure-stlvec distribution package for details.

*/

#ifndef STLBASE_H
#define STLBASE_H

#include <vector>
#include <map>
#include <iterator>
#include <functional>
#include <stdlib.h>
#include "pure/runtime.h"

/* Quick print macro for debugging */

#define PR(x,y) std::cerr << #x ", " #y ": " << y << std::endl;
#define PR2(x,y,z) std::cerr << #x ", " #y ": " << y << ", " #z ": " << z << std::endl;

/* Class to keep track of interpreter-local data */

template <class T>
struct ILS {
  pure_interp_key_t key;
  T val;
  /* This is safe to invoke at any time. */
  ILS() : key(pure_interp_key(free)), val(T()) {}
  ILS(T const& x) : key(pure_interp_key(free)), val(x) {}
  /* This must only be invoked after an interpreter instance is available. It
     will return a different reference to an object of type T (initialized to
     the default value, if given) for each interpreter. */
  T& operator()();
};

template <class T>
T& ILS<T>::operator()()
{
  T *ptr = (T*)pure_interp_get(key);
  if (!ptr) {
    ptr = (T*)malloc(sizeof(T));
    pure_interp_set(key, ptr);
    *ptr = val;
  }
  return *ptr;
}

/* px - shorthand for pure_expr */

typedef pure_expr px;

/* px_new and friends - wrappers around pure_new and friends to
   allow observation of pure_expr ref count changes if STL_DEBUG
   is defined. 
*/

#ifdef STL_DEBUG
px* px_new(px* x);
void px_free(px* x);
void px_freenew(px* x);
void px_ref(px* x);
void px_unref(px* x);
#else 
inline px* px_new(px* x){return x ? pure_new(x) : x;}
inline void px_free(px* x){if (x) pure_free(x);}
inline void px_freenew(px* x){if (x) pure_freenew(x);}
inline void px_ref(px* x){if (x) pure_ref(x);}
inline void px_unref(px* x){if (x) pure_unref(x);}
#endif

/* px_handle (pxh) - wrapper around pure_expr* to automate pure_expr ref
   counting. Please note that C++ will call a pxh's destructor when it goes
   out of scope. You can stop this from happening by calling the pxh's release
   function. Also, there are no virtual functions (to avoid vf table). This
   keeps sizeof(pxh) = sizeof(px*).

*/
class px_handle {
public:
  // default constructor
  px_handle();

  // constructor and type conversion from px*
  px_handle(px* p);

  // copy constructor
  px_handle(const px_handle& pxh);

  // assignment
  px_handle& operator=(const px_handle& pxh);

  // operator conversion to px*
  operator px*() const { return pxp_; }

  // destructor - not virtual - (avoid vf table)
  ~px_handle(); 

  // return the underlying px*
  px* pxp() const {return pxp_;}

  // release underlying px*
  void release(){pxp_ = 0;}

  // 
  friend std::ostream& operator<<(std::ostream& os, const px_handle& pxh);

private:
  px* pxp_;
};

typedef px_handle pxh;  // sizeof(pxh) == sizeof(px*) -- no virtual funs

/* pxhpairs and hash-rocket expressions (lhs=>rhs) - pxhpairs are (C++) pairs
   of pxh's which are useful for map<pxh,pxh> and multimap<pxh,pxh>. Functions
   are provided to convert hash-rockets to pxhpairs and to extract the hash
   rockets rhs and lhs, as well as functions that do the reverse. */

typedef std::pair<pxh,pxh> pxhpair;

typedef std::pair<const pxh,pxh> pxhkvpair;

bool pxrocket_to_pxlhs_pxrhs(px* rp, px** lhs, px** rhs);

px* pxlhs_pxrhs_to_pxrocket(const px* lhs, const px* rhs);

bool pxrocket_to_pxhpair(pxh rp, pxhpair& pair);

px* pxhpair_to_pxrocket(const pxhpair& pair);

px* pxhpair_to_pxlhs(const pxhpair& pair);

/* pxh_fun and subclasses - function objects to lift px* functions to pxh
   functions. When you need to pass a Pure callback to an algorithm that acts
   on pure expressions, wrap it in one of these. Note that if the Pure
   callback function calls throws an exception, the pxh_fun will catch it and
   then throw it as a C++ exception. Thus, C++ functions that take a pxh_fun
   as an argument should be called from within a try block, with a catch (px*
   e) block. Assuming that the call was made within a function called from
   Pure, the catch block could be {pure_throw(e)}. */

class pxh_fun {
public:
  pxh_fun(px* fun) : fun_(px_new(fun)){}
  pxh_fun(const pxh_fun& pxh_f) : fun_(px_new(pxh_f.fun_)) {}
  virtual ~pxh_fun(){px_free(fun_);}
  virtual pxh_fun& operator=(const pxh_fun& rhs);
  px* pxfun() {return fun_;}
protected:
  px* fun_;
};

struct pxh_fun1 : public pxh_fun,
                  public std::unary_function<const pxh&, pxh>
{
  pxh_fun1(px* f) : pxh_fun(f){}
  pxh operator()(const pxh&) const;
};

struct pxh_less : public pxh_fun, 
                  public std::binary_function<const pxh&, const pxh&, pxh>
{
  bool is_lt;
  bool is_gt;
  pxh_less(px* f);
  bool operator()(const pxh&, const pxh&) const;
  
};

struct pxh_fun2 : public pxh_fun, 
                  public std::binary_function<const pxh&, const pxh&, pxh>
{
  pxh_fun2(px* f) : pxh_fun(f){}
  pxh operator()(const pxh&, const pxh&) const;
};

struct pxh_pred1 : public pxh_fun,
                   public std::unary_function<const pxh&, bool>
{
  pxh_pred1(px* f) : pxh_fun(f){}
  bool operator()(const pxh&) const;
};

struct pxh_pred2 : public pxh_fun,
                   public std::binary_function<const pxh&, const pxh&, bool>
{
  pxh_pred2(px* f) : pxh_fun(f){}
  bool operator()(const pxh&, const pxh&) const;
};

struct pxh_gen : public pxh_fun
{
  pxh_gen(px* f) : pxh_fun(f){}
  pxh operator()();
};

struct pxhpair_less : 
  public std::binary_function<const pxhpair&, const pxhpair&, bool>
{
  pxhpair_less(px* f, px* s) : first_less(f), second_less(s) {}
  bool operator()(const pxhpair&, const pxhpair&) const;
protected:
  pxh_pred2 first_less;
  pxh_pred2 second_less;
};

struct pxhpair_equal : 
  public std::binary_function<const pxhpair&, const pxhpair&, bool>
{
  pxhpair_equal(px* f, px* s) : first_equal(f), second_equal(s) {}
  bool operator()(const pxhpair&, const pxhpair&) const;
protected:
  pxh_pred2 first_equal;
  pxh_pred2 second_equal;
};

struct pxhpair_first_equal : 
  public std::binary_function<const pxhpair&, const pxhpair&, bool>
{
  pxhpair_first_equal(px* f) : first_equal(f) {}
  bool operator()(const pxhpair&, const pxhpair&) const;
protected:
  pxh_pred2 first_equal;
};

struct pxhpair_first_equivalent : 
  public std::binary_function<const pxhpair&, const pxhpair&, bool>
{
  pxhpair_first_equivalent(px* f) : first_less(f) {}
  bool operator()(const pxhpair&, const pxhpair&) const;
protected:
  pxh_pred2 first_less;
};

struct pxhpair_equivalent : 
  public std::binary_function<const pxhpair&, const pxhpair&, bool>
{
  pxhpair_equivalent(px* f, px* s) : first_less(f), second_equal(s) {}
  bool operator()(const pxhpair&, const pxhpair&) const;
protected:
  pxh_pred2 first_less;
  pxh_pred2 second_equal;
};

/**** Helpers **********************************************************/

std::ostream& operator<<(std::ostream& os, px* pe);

px* px_cons_sym();
px* px_null_list_sym();
px* px_rocket_sym();
px* px_less_than_sym();
px* px_equal_sym();
px* px_greater_than_sym();

void bad_function();
void index_error();
void range_overflow();
void range_overlap();
void bad_argument();
void failed_cond();

px* pxh_to_pxp(pxh h); // used by std::transform


/*** C functions used by Pure scripts *************************************/

/* stl_refc and friends - functions to return a pure_expr's refc and to
   enable or disable pure_expr ref count tracing or the tracing of stlvec's
   (sv's). These functions can be called whether or not STl_DEBUG is
   defined. If it is not, however, the tracing facility does nothing.
 */

extern "C" {
  int  stl_refc(px *x);
  void stl_set_px_trace(bool enable);
  bool stl_px_trace_enabled();
}

/* Global symbols used by stlmap. Basic stlvec creation and deletion --
   available to stlmap modules. */

typedef std::vector<pxh> sv;

extern "C" {
  px*  stlbegin_sym();
  px*  stlend_sym();

  sv*  sv_make_empty();
  void sv_delete(sv* sv_p);
}

#endif // STLBASE_H
