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

#ifndef STL_H
#define STL_H

#include <vector>
#include <map>
#include <iterator>
#include <functional>
#include "pure/runtime.h"

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
inline px* px_new(px* x){return pure_new(x);}
inline void px_free(px* x){pure_free(x);}
inline void px_freenew(px* x){pure_freenew(x);}
inline void px_ref(px* x){pure_ref(x);}
inline void px_unref(px* x){pure_unref(x);}
#endif

/* px_handle (pxh) - wrapper around pure_expr* to automate pure_expr ref
   counting. We do not try to make these smart pointers (i.e., with -> and *
   operators) because we want client code to be clear about when it is using a
   pxh and when it is using a pure_expr*. Also, users should not generally be
   dereferencing pure_expr* (directly or through a pxh) because pure_expr is
   meant to be an obtuse data type. In spite of this, we do provide an
   automatic type conversion from a pure_expr* to a pxh, simply because
   the conversion occurs frequently. 

   Please remember that C++ will call a pxh's destructor when it goes out of
   scope. When you are returning the underlying pure_expr* help by a pxh, say,
   h, you should get the pure_expr* (with h.pxp()), release it (with
   h.release()), and then return it. Otherwise the pure_expr's ref count
   will be decremented after C++ sets up the return value, which is probably
   not what you want. 

   Note that there are no virtural functions (to avoid vf table). This keeps
   sizeof(pxh) = sizeof(px*).

*/
class px_handle {
public:
  // constructor
  px_handle() : pxp_(0) {};

  // constructor and type conversion from px*
  px_handle(px* p) : pxp_(px_new(p)) {}

  // copy constructor
  px_handle(const px_handle& pxh) : pxp_(px_new(pxh.pxp_)) {}

  // assignment
  px_handle& operator=(const px_handle& pxh);

  // destructor - not virtural
  ~px_handle(){if (pxp_) px_free(pxp_);} // not base class (avoid vf table)

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

typedef std::pair<pxh,pxh> pxh_pair;

/* pxh_fun and subclasses - function objects to lift px* functions to pxh
   functions. When you need to pass a Pure callback to an algorithm that acts
   on pxh'x "wrap" it in one of these. Note that if the Pure callback function
   calls throws an exception, the pxh_fun will catch it and then throw it as a
   C++ exception. Thus, C++ functions that take a pxh_fun as an argument
   should be called from within a try block, with a catch (px* e)
   block. Assuming that the call was made within a function called from Pure,
   the catch block could be {pure_throw(e)}. */

class pxh_fun {
public:
  pxh_fun(px* fun) : fun_(px_new(fun)){}
  pxh_fun(const pxh_fun& pxh_f) : fun_(px_new(pxh_f.fun_)) {}
  virtual pxh_fun& operator=(const pxh_fun& rhs);
  virtual ~pxh_fun(){px_free(fun_);}
protected:
  px* fun_;
};

struct pxh_fun1 : public pxh_fun,
                  public std::unary_function<const pxh&, pxh>
{
  pxh_fun1(px* f) : pxh_fun(f){}
  pxh operator()(const pxh&);
};

struct pxh_fun2 : public pxh_fun, 
                  public std::binary_function<const pxh&, const pxh&, pxh>
{
  pxh_fun2(px* f) : pxh_fun(f){}
  pxh operator()(const pxh&, const pxh&);
};

struct pxh_pred1 : public pxh_fun,
                   public std::unary_function<const pxh&, bool>
{
  pxh_pred1(px* f) : pxh_fun(f){}
  bool operator()(const pxh&);
};

struct pxh_pred2 : public pxh_fun,
                   public std::binary_function<const pxh&, const pxh&, bool>
{
  pxh_pred2(px* f) : pxh_fun(f){}
  bool operator()(const pxh&, const pxh&);
};

struct pxh_gen : public pxh_fun
{
  pxh_gen(px* f) : pxh_fun(f){}
  pxh operator()();
};


/**** Helpers **********************************************************/

std::ostream& operator<<(std::ostream& os, px* pe);

void stl_throw_sym(const char *name);

inline void bad_function() {stl_throw_sym("bad_function");}
inline void index_error() {stl_throw_sym("out_of_bounds");}
inline void range_overflow() {stl_throw_sym("range_overflow");}
inline void range_overlap() {stl_throw_sym("range_overlap");}
inline void bad_argument() {stl_throw_sym("bad_argument");}
inline void failed_cond() {stl_throw_sym("failed_cond");}

int cons_tag();
int null_list_tag();
int rocket_tag();

pxh_pair* rocket_to_pair(px* rp);
px* pair_to_rocket(pxh_pair* pp);

px* stl_begin();
px* stl_end();
px* stl_insert();
px* stl_back_insert();


/*** Interface ***********************************************************/

/* stl_refc and friends - functions to return a pure_expr's refc and to
   enable or disable pure_expr ref count tracing or the tracing of stlvec's
   (sv's). These functions can be called whether or not STl_DEBUG is
   defined. If it is not, however, the tracing facility does nothing.
 */

extern "C" {
  int stl_refc(px *x);
  void stl_set_px_trace(bool enable);
  void stl_set_sv_trace(bool enable);
  bool stl_px_trace_enabled();
  bool stl_sv_trace_enabled();
}


#endif // STL_H
