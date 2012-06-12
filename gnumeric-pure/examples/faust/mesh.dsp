
/* Please see the paper for details. */

/* Layout of a systolic array:

        x1        xm
        ↓         ↓
   y1 → □ → ... → □ → y1'
        ↓         ↓
       ...       ...
        ↓         ↓
   yn → □ → ... → □ → yn'
        ↓         ↓
        x1'       xm'

  g(m,f)   : y,x1,...,xm -> x1',...,xm',y'
  constructs a single row of size m.

  h(n,m,f) : y1,...,yn,x1,...,xm -> x1',...,xm',yn',...,y1'
  constructs an array of size nxm.

  f is the function computed by each cell, which must take
  exactly two inputs and yield exactly two outputs. */

g(1,f)		= f;
g(m,f)		= (f, r(m-1)) : (_, g(m-1,f));

h(1,m,f)	= g(m,f);
h(n,m,f)	= (r(n+m) <:
		   (!,r(n-1),s(m), (_,s(n-1),r(m) : g(m,f)))) :
                  (h(n-1,m,f), _);

/* An alternate entry point which calls h but also does some pre- and
   postprocessing to bring inputs and outputs into a more natural order; see
   the paper for details. */

k(n,m,f)        = u(n,m) : h(n,m,f) : v(n,m);

// route n inputs
r(1)		= _;
r(n)		= _,r(n-1);

// skip n inputs
s(1)		= !;
s(n)		= !,s(n-1);

// reverse n inputs
t(1)		= _;
t(n)		= r(n) <: (!,t(n-1),_,s(n-1));

// preprocessing: swap x and y inputs
u(n,m)          = r(m+n) <: (s(m),r(n+m),s(n));

// postprocessing: reverse n y outputs
v(n,m)          = r(m),t(n);

// sample cell function
f		= + <: _,_;

// Uncomment this to get just one row of the mesh.
//process		= g(3,f);
// Uncomment this to get the entire mesh (mesh-example.pd).
//process		= h(2,3,f);
// Uncomment this to get the mesh with inputs and outputs rearranged so that
// they appear in the "right" order (mesh-prepost.pd).
process		= k(2,3,f);
