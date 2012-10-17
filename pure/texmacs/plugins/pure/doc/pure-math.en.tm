<TeXmacs|1.0.7.16>

<style|<tuple|tmdoc|varsession>>

<\body>
  <section|Math Input>

  Start up a Pure session and load the <verbatim|math> and <verbatim|reduce>
  modules, as well as the <verbatim|texmacs> module which provides some
  convenience functions for use with Pure in <TeXmacs>.

  <\session|pure|math>
    <\output>
      \;

      \ __ \\ \ \| \ \ \| \ __\| _ \\ \ \ \ Pure 0.56
      (x86_64-unknown-linux-gnu)

      \ \| \ \ \| \| \ \ \| \| \ \ \ __/ \ \ \ Copyright (c) 2008-2012 by
      Albert Graef

      \ .__/ \\__,_\|_\| \ \\___\| \ \ \ (Type 'help' for help, 'help
      copying'

      _\| \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ for license
      information.)

      \;

      Loaded prelude from /usr/lib/pure/prelude.pure.

      \;
    </output>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      using math, reduce, texmacs;
    <|unfolded-io>
      Reduce (Free CSL version), 09-Oct-12 ...
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      simplify (df (sin (x^2)) x);
    <|unfolded-io>
      2*cos (x^2)*x
    </unfolded-io>
  </session>

  To enter an expression like the one above as a mathematical formula, we
  must switch the input line to math mode first. To do that, you can go
  search the toolbar for <samp|Input Options \| Mathematical Input> and check
  it, or type the key combination <key|Ctrl $> defined by the Pure plugin.
  Another useful convenience is the <verbatim|?> prefix operator (defined in
  <verbatim|texmacs.pure>) which does a Reduce <verbatim|simplify> of its
  expression argument which is quoted automagically. So here's how we can
  enter the expression <verbatim|? df (sin (x^2)) x)> in math mode:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?df <around*|(|sin <around*|(|x<rsup|2>|)>|)> x;
    <|unfolded-io-math>
      2*cos (x^2)*x
    </unfolded-io-math>
  </session>

  This has the same effect as:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|<rprime|'>df <around*|(|sin
      <around*|(|x<rsup|2>|)>|)> x|)>;
    <|unfolded-io-math>
      2*cos (x^2)*x
    </unfolded-io-math>
  </session>

  But it's a lot easier to type, and the <verbatim|?> operator also includes
  the necessary magic to make the <TeXmacs> syntax of sums, products, limits
  and differentials work in Pure (we'll see how to use these in a moment).
  The <verbatim|?:> operator does the same, but evaluates its argument; you
  want to use that if the expression includes some Pure functions which
  should be evaluated before submitting the result to Reduce. Note the
  difference:

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      foo x = x+1;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?intg<around*|(|foo x|)> x;
    <|unfolded-io-math>
      \;

      *** foo declared operator\ 

      intg (foo x) x
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?:intg<around*|(|foo x|)> x;
    <|unfolded-io-math>
      (x^2+2*x)/2
    </unfolded-io-math>
  </session>

  <subsection|Basic expressions>

  Most mathematical expressions are mapped to corresponding Pure expressions
  in a sensible way:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|\<backslash\>x\<rightarrow\>2*x+\<alpha\>|)>
      5;<with|mode|prog| // lambdas>
    <|unfolded-io-math>
      10+alpha
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      x<rsub|k>,x<rsup|k>,<lsub|k>x,<lsup|k>x,<below|x|k>,<above|x|k>;<text|<verbatim|
      // sub- and superscripts>>
    <|unfolded-io-math>
      x!k,x^k,x!k,x^k,below x k,above x k
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <frac|x|y>,<tfrac|x|y>,<dfrac|x|y>,<frac*|x|y>,x/y;<with|mode|prog| //
      fractions>
    <|unfolded-io-math>
      x/y,x/y,x/y,x/y,x/y
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <binom|n-1|k-1>+<binom|n-1|k>;<text|<verbatim| // binomials>>
    <|unfolded-io-math>
      binom (n-1) (k-1)+binom (n-1) k
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>;
      <tabular*|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>;
      <block*|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>;<text|<verbatim|
      // matrices and tables (various formats)>>
    <|unfolded-io-math>
      {a,b;b,-a}

      {a,b;b,-a}

      {a,b;b,-a}
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <det|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>;<text|<verbatim|
      // determinants>>
    <|unfolded-io-math>
      det {a,b;b,-a}
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|c>>>>>;
      <matrix|<tformat|<table|<row|<cell|a>>|<row|<cell|b>>|<row|<cell|c>>>>>;<text|<verbatim|
      // vectors>>
    <|unfolded-io-math>
      {a,b,c}

      {a;b;c}
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <tree|a|b|c|<tree|x|y|z>>;<text|<verbatim| // trees>>
    <|unfolded-io-math>
      tree [a,b,c,tree [x,y,z]]
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      e<rsup|x>*\<alpha\>+\<beta\>;<text|<verbatim| // arithmetic>>
    <|unfolded-io-math>
      2.71828182845905^x*alpha+beta
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <sqrt|x<rsup|2>+y<rsup|2>>; <rprime|'><around*|(|<sqrt|x|3>|)>;<text|<verbatim|
      // roots>>
    <|unfolded-io-math>
      sqrt (x^2+y^2)

      x^(1/3)
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<neg\>A\<wedge\><around*|(|B\<vee\>C|)>;<text|<verbatim| // logic>>
    <|unfolded-io-math>
      ~A&&(B\|\|C)
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      x\<gtr\>y, x\<less\>y, x\<geqslant\>y,x\<leqslant\>y,x\<longequal\>y,x\<neq\>y;<text|<verbatim|
      // comparisons>>
    <|unfolded-io-math>
      x\<gtr\>y,x\<less\>y,x\<gtr\>=y,x\<less\>=y,x==y,x~=y
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      x\<equiv\>y, x\<nequiv\>y;<text|<verbatim| // syntactic equality (=== /
      ~== in Pure)>>
    <|unfolded-io-math>
      0,1
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|1,2,3|]>;<text|<verbatim| // lists>>
    <|unfolded-io-math>
      [1,2,3]
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      1\<ldots\>10; 1:3\<ldots\>11;<text|<verbatim| // arithmetic sequences>>
    <|unfolded-io-math>
      [1,2,3,4,5,6,7,8,9,10]

      [1,3,5,7,9,11]
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|a+1 <mid|\|> a=1\<ldots\>10;a mod 2|]>;<text|<verbatim| //
      comprehensions>>
    <|unfolded-io-math>
      [2,4,6,8,10]
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|a,b+1|)>; <around*|[|a,b+1|]>;
      <around*|\<langle\>|a,b+1|\<rangle\>>;
      <around*|\<llbracket\>|a,b+1|\<rrbracket\>>;
      <around*|\<lfloor\>|n/2|\<rfloor\>>; <around*|\<lceil\>|n/2|\<rceil\>>;
      <around*|\||x|\|>; <around*|\<\|\|\>|x|\<\|\|\>>;<text|<verbatim| //
      various brackets>>
    <|unfolded-io-math>
      a,b+1

      [a,b+1]

      a,b+1

      [a,b+1]

      floor (n/2)

      ceil (n/2)

      abs x

      norm x
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <wide|x|^>;<wide|x|~>;<wide|x|\<bar\>>;<wide|x|\<vect\>>;<wide|x|\<check\>>;<wide|x|\<breve\>>;<wide|x|\<dot\>>;<wide|x|\<ddot\>>;<wide|x|\<acute\>>;<wide|x|\<grave\>>;<rprime|'>x;<rprime|''>x;
      <neg|x>;<text|<verbatim| // accents and primes/quotes>>
    <|unfolded-io-math>
      hat x

      tilde x

      bar x

      vect x

      check x

      breve x

      dot x

      ddot x

      acute x

      grave x

      x

      'x

      ~x
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      x\<oplus\>y;x\<ominus\>y;x\<otimes\>y;x\<oslash\>y;x\<pm\>y;x\<mp\>y;x\<div\>y;x\<cap\>y;x\<cup\>y;x\<uplus\>y;<text|<verbatim|
      // infix operators>>
    <|unfolded-io-math>
      x oplus y

      x ominus y

      x otimes y

      x oslash y

      x pm y

      x mp y

      x div y

      x cap y

      x cup y

      x uplus y
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<alpha\>;\<Gamma\>;\<b-up-Z\>;\<b-z\>;\<b-0\>;\<cal-C\>;\<frak-F\>;\<frak-u\>,\<frak-v\>,\<frak-w\>;\<bbb-Q\>;<text|<verbatim|
      // Greek symbols, special glyphs>>
    <|unfolded-io-math>
      alpha

      Gamma

      Z

      z

      0

      C

      F

      u,v,w

      Q
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <math-bf|foo>,<math-it|bar>,<math-sl|baz>,<math-ss|gnu>,<math-tt|gna>,<math-up|gnats>;<text|<verbatim|
      // special markup>>
    <|unfolded-io-math>
      foo,bar,baz,gnu,gna,gnats
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <really-tiny|A,B>, <tiny|A,B>, <very-small|A,B>, <small|A,B>,
      <normal-size|A,B>, <large|A,B>, <very-large|A,B>, <huge|A,B>,
      <really-huge|A,B>;<text|<verbatim| // various sizes>>
    <|unfolded-io-math>
      A,B,A,B,A,B,A,B,A,B,A,B,A,B,A,B,A,B
    </unfolded-io-math>
  </session>

  More examples using list comprehensions:<\footnote>
    Note that in order to get the single vertical bar <verbatim|\|> in math
    mode, you'll have to type either <key|Shift+F5 \|> or <key|Alt+M \|> (the
    latter gives you a ``big'' <verbatim|\|> symbol which automatically
    expands with the brackets surrounding it).
  </footnote>

  <\session|pure|math>
    <\input>
      \<gtr\>\ 
    <|input>
      <math|<around*|\<\|\|\>|X\<colons\>matrix|\<\|\|\>> = <sqrt|sum
      <around*|[|x<rsup|2><mid|\|> x=X|]>>;>
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<around*|\<\|\|\>|<matrix|<tformat|<table|<row|<cell|1>|<cell|2>|<cell|3>|<cell|4>>>>>|\<\|\|\>>;>
    <|unfolded-io>
      5.47722557505166
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<around*|[|2*x+1<mid|\|>x=1\<ldots\>5|]>;>
    <|unfolded-io>
      [3,5,7,9,11]
    </unfolded-io>

    <\input-math>
      \<gtr\>\ 
    <|input-math>
      eye n = <around*|{|i\<longequal\>j<mid|\|>i=1\<ldots\>n;j=1\<ldots\>n|}>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      eye 3;
    <|unfolded-io-math>
      {1,0,0;0,1,0;0,0,1}
    </unfolded-io-math>

    <\input>
      \<gtr\>\ 
    <|input>
      <math|<math-ss|primes> = <math-ss|sieve> (2\<ldots\>\<infty\>)
      <math-bf|with> <math-ss|sieve> (p:<math-it|qs>) = p : <math-ss|sieve>
      [q<mid|\|>q = <math-it|qs>; q <math-ss|mod> p] &; <math-bf|end>;>
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<math-ss|primes>!!<around*|(|0\<ldots\>20|)>;>
    <|unfolded-io>
      [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73]
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<math-ss|list> <around*|(|<math-ss|take> 20
      <around*|(|<math-ss|drop> 100 <math-ss|primes>|)>|)>;>
    <|unfolded-io>
      [547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659]
    </unfolded-io>
  </session>

  <subsection|Big operators (integrals, limits, sums, etc.)>

  Big operators (<samp|Insert \| Symbol \| Big operator> in math mode) are
  mapped to corresponding Pure expressions, generally using a
  Reduce-compatible form:

  <\session|pure|math>
    \;

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<big|int>sin x \<mathd\>x;<big|int><rsub|a><rsup|b>x<rsup|2>\<mathd\>x;<big|intlim><rsub|a><rsup|b>x<rsup|2>\<mathd\>x;lim<rsub|n\<rightarrow\>\<infty\>><frac|1|n>;>
    <|unfolded-io>
      intg (sin x) x

      intg (x^2) x a b

      intg (x^2) x a b

      limit (1/n) n inf
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<big|sum><rsub|k=1><rsup|n><around*|(|2*k-1|)>;<big|prod><rsub|k=1><rsup|n><around*|(|2*k-1|)>;>
    <|unfolded-io>
      sum (2*k-1) k 1 n

      prod (2*k-1) k 1 n
    </unfolded-io>
  </session>

  Sums and products without an upper bound are translated to the appropriate
  aggregates of Pure list comprehensions so that they can be computed
  directly in Pure:

  <\session|pure|math>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<big|sum><rsub|k=1\<ldots\>5><around*|(|2*k-1|)>;<big|prod><rsub|k=1\<ldots\>5><around*|(|2*k-1|)>;>
    <|unfolded-io>
      25

      945
    </unfolded-io>
  </session>

  Note that the above is equivalent to the following verbatim Pure code:

  <\session|pure|dummy>
    <\input>
      \<gtr\>\ 
    <|input>
      sum [(2*k-1)\|k=1..5]; prod [(2*k-1)\|k=1..5];
    </input>
  </session>

  The same holds for a number of other big operators, specifically set union
  and intersection (``bigcup'' and ``bigcap''). These aren't predefined in
  Pure, but we can implement some useful Pure operations with them, for
  instance:

  <\session|pure|math>
    <\input>
      \<gtr\>\ 
    <|input>
      X::list cap Y::list = [x \| x=X; any (==x) Y];

      X::list - Y::list = [x \| x=X; all (~=x) Y];

      X::list cup Y::list = X + (Y - X);
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<around*|[|9,1,3,7|]>\<cap\><around*|[|9,1,7,2|]>;
      <around*|[|9,1,3,7|]>\<cup\><around*|[|1,7,2|]>;
      <around*|[|9,1,3,7|]>-<around*|[|1,7,2|]>;>
    <|unfolded-io>
      [9,1,7]

      [9,1,3,7,2]

      [9,3]
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      bigcap = foldl1 (cap); bigcup = foldl1 (cup);
    </input>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\<nobracket\>|<big|cap><rsub|k=1><rsup|n>X<rsub|k-1>,<big|cup><rsub|k=1><rsup|n>X<rsub|k-1>,
      <big|cap><rsub|k=1><rsup|n><big|cup><rsub|l=1><rsup|k><around*|(|X<rsub|n>-X<rsub|l-1>|)>|\<nobracket\>><space|1spc><math-bf|when>
      X=<around*|[|1\<ldots\>n<mid|\|>n=1\<ldots\>\<infty\>|]>; n=10
      <math-bf|end>;
    <|unfolded-io-math>
      [1],[1,2,3,4,5,6,7,8,9,10],[2,3,4,5,6,7,8,9,10,11]
    </unfolded-io-math>

    <\input>
      \<gtr\>\ 
    <|input>
      bigwedge = foldl (&&) true; bigvee = foldl (\|\|) false;
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<around*|\<nobracket\>|<big|wedge><rsub|k=1><rsup|n><around*|(|x<rsub|k-1>\<geqslant\>0|)>,<big|vee><rsub|k=1><rsup|n><around*|(|x<rsub|k-1>\<less\>0|)>|\<nobracket\>>
      <math-bf|when> x=-3\<ldots\>3; n=#x <math-bf|end>;>
    <|unfolded-io>
      0,1
    </unfolded-io>
  </session>

  <subsection|Differentials and integrals>

  Both differentials and integrals can be specified using customary
  mathematical notation which gets translated to invocations of the Reduce
  <verbatim|df> and <verbatim|int> operators (the latter is actually named
  <verbatim|intg> in Pure, to avoid a name clash with the built-in Pure
  function <verbatim|int>):

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\><rsup|2>f/\<mathd\>x<rsup|2>;
    <|unfolded-io-math>
      df f x 2
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<partial\><rsup|2>f/\<partial\>x<rsup|2>;
    <|unfolded-io-math>
      df f x 2
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int>2*f<around*|(|x|)>\<mathd\>x;
    <|unfolded-io-math>
      intg (2*f x) x
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int>2*f<around*|(|x|)>\<mathd\> <around*|(|cos x|)>;
    <|unfolded-io-math>
      intg (2*f x) (cos x)
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\><around*|(|x<rsup|2>|)>/\<mathd\>x;
    <|unfolded-io-math>
      df (x^2) x
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\><rsup|2><around*|(|x<rsup|3>|)>/\<mathd\>x<rsup|2>;
    <|unfolded-io-math>
      df (x^3) x 2
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <frac|\<partial\><rsup|9><around*|(|x<rsup|2>*y<rsup|3>*z<rsup|4>|)>|\<partial\>x<rsup|2>*\<partial\>y<rsup|3>*\<partial\>z<rsup|4>>;
    <|unfolded-io-math>
      df (x^2*y^3*z^4) x 2 y 3 z 4
    </unfolded-io-math>
  </session>

  Ok, so let's see how we can actually calculate some differentials,
  integrals, limits, etc.<nbsp>with Reduce, using the <verbatim|?> operator
  we've introduced earlier:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ? df <around*|(|<around*|(|x+y|)><rsup|5>|)> x;
    <|unfolded-io-math>
      5*x^4+20*x^3*y+30*x^2*y^2+20*x*y^3+5*y^4
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?<frac|\<partial\><rsup|9><around*|(|x<rsup|2>*y<rsup|3>*z<rsup|4>|)>|\<partial\>x<rsup|2>*\<partial\>y<rsup|3>*\<partial\>z<rsup|4>>;
    <|unfolded-io-math>
      288
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?<big|int>sin<around*|(|2*x|)>\<mathd\>x;
    <|unfolded-io-math>
      (-cos (2*x))/2
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\<nobracket\>|?<big|int>x<rsup|2>*<around|(|a*x+b|)><rsup|n>\<mathd\>x|\<nobracket\>>
      <math-bf|when> b=0 <math-bf|end>;
    <|unfolded-io-math>
      x^n*a^n*x^3/(n+3)
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?lim<rsub|x\<rightarrow\>\<infty\>><around*|(|x*sin<around*|(|1/x|)>|)>;
    <|unfolded-io-math>
      1
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?lim<rsub|x\<rightarrow\>0><around*|(|1/x|)>;
    <|unfolded-io-math>
      inf
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?<big|sum><rsub|k=0><rsup|n-1><around*|(|a+k*r|)>;
    <|unfolded-io-math>
      (2*a*n+n^2*r-n*r)/2
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?<big|prod><rsub|k=1><rsup|n><frac|k|k+2>;
    <|unfolded-io-math>
      2/(n^2+3*n+2)
    </unfolded-io-math>
  </session>

  It is often convenient to wrap up calls to <verbatim|?> in Pure functions.
  A function definition in Pure takes the general form ``<math|f
  <space|0.2spc>x<rsub|1><space|0.2spc>\<cdots\><space|0.2spc>x<rsub|n>=<math-it|rhs>>''.
  (We'll learn about different ways to define Pure functions in math mode in
  a moment.) For instance:

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      I a b n = ?<big|int>x<rsup|2>*<around|(|a*x+b|)><rsup|n>\<mathd\>x;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      I a b n;
    <|unfolded-io-math>
      ((a*x+b)^n*a^3*n^2*x^3+3*(a*x+b)^n*a^3*n*x^3+2*(a*x+b)^n*a^3*x^3+

      (a*x+b)^n*a^2*b*n^2*x^2+(a*x+b)^n*a^2*b*n*x^2-2*(a*x+b)^n*a*b^2*n*x+2*(a*x+b)^n*b^3)/(a^3*n^3+6*a^3*n^2+11*a^3*n+6*a^3)
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      I a b 0;
    <|unfolded-io-math>
      x^3/3
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      I 0 b n;
    <|unfolded-io-math>
      b^n*x^3/3
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      I a 0 k;
    <|unfolded-io-math>
      x^k*a^k*x^3/(k+3)
    </unfolded-io-math>
  </session>

  <subsection|Programming>

  Note that all the supported math elements not only work in expression
  evaluation, but also when defining Pure functions, on both sides of the
  definition. For instance, let's define a prettier notation for the list
  slicing operator (<verbatim|!!> in Pure). We'd actually like to write an
  ordinary index in math mode, like this:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|<math-it|xs>|)><rsub|1\<ldots\>n>;
    <|unfolded-io-math>
      xs!(1..n)
    </unfolded-io-math>
  </session>

  This isn't usually defined if both <em|xs> and <em|ys> are lists, but we
  can make it so as follows:

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      <around*|(|<math-it|xs>\<colons\>list|)><rsub|<math-it|ys>\<colons\>list>=<math-it|xs>!!<math-it|ys>;
    </input-math>
  </session>

  That's it. We can now write:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|"a"\<ldots\>"z"|)><rsub|10\<ldots\>16>;
    <|unfolded-io-math>
      ["k","l","m","n","o","p","q"]
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <math-ss|primes><rsub|99\<ldots\>117><space|1spc><math-bf|with><space|1spc><math-ss|primes>
      = <math-ss|sieve> (2\<ldots\>\<infty\>); <math-ss|sieve>
      (p:<math-it|qs>) = p : <math-ss|sieve> [q\|q = <math-it|qs>; q
      <math-ss|mod> p] & <math-bf|end>;
    <|unfolded-io-math>
      [541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647]
    </unfolded-io-math>
  </session>

  For a slightly more substantial example, let's consider the binomials:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <binom|n|k>;
    <|unfolded-io-math>
      binom n k
    </unfolded-io-math>
  </session>

  This function isn't predefined in Pure, so let's do that now. To get nicely
  aligned equations, we'll use an equation array this time
  (<verbatim|\\eqnarray*> in math mode; similarly, the binomials can be
  entered with <verbatim|\\binom>):

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      <\eqnarray*>
        <tformat|<table|<row|<cell|<binom|n\<colons\>int|k\<colons\>int>>|<cell|=>|<cell|<binom|n-1|k-1>+<binom|n-1|k><space|1spc><math-bf|if><space|1spc>n\<gtr\>k\<wedge\>k\<gtr\>0;>>|<row|<cell|>|<cell|=>|<cell|1<space|1spc><math-bf|otherwise>;>>>>
      </eqnarray*>
    </input-math>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      show binom
    <|unfolded-io>
      binom n::int k::int = binom (n-1) (k-1)+binom (n-1) k if
      n\<gtr\>k&&k\<gtr\>0;

      binom n::int k::int = 1;
    </unfolded-io>
  </session>

  Let's calculate the first five rows of the Pascal triangle to see that it
  works:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|<around*|[|<binom|n|k><mid|\|>k=0\<ldots\>n|]><mid|\|>n=0\<ldots\>5|]>;
    <|unfolded-io-math>
      [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
    </unfolded-io-math>
  </session>

  Or how about the entire Pascal triangle? The following comprehension yields
  a Pure stream (a.k.a.<space|1spc>infinite list):<\footnote>
    This will become <em|very> slow if you try to access the binomials for
    larger values of <math|n>, since the algorithm needs
    <math|O<around*|(|n<rsup|2>|)>> time. A more efficient algorithm is left
    as an exercise to the reader.
  </footnote>

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      binomials =<around*|[|<around*|[|<binom|n|k><mid|\|>k=0\<ldots\>n|]><mid|\|>n=0\<ldots\>\<infty\>|]>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|binomials<rsub|5>|)><rsub|0\<ldots\>5>;
      <around*|(|binomials<rsub|20>|)><rsub|0\<ldots\>5>;
    <|unfolded-io-math>
      [1,5,10,10,5,1]

      [1,20,190,1140,4845,15504]
    </unfolded-io-math>
  </session>

  We can also align subterms in expressions, using the <TeXmacs> <samp|stack>
  construct. E.g., the \ <math|<stack|<tformat|<table|<row|<cell|x>|<cell|y>>|<row|<cell|z>|<cell|t>>>>>>
  stack will expand to just <verbatim|x y z t> in Pure (the converter
  traverses this construct in row-major order).

  So, for \ instance, the following two expressions are exactly the same in
  Pure syntax:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|<binom|n|k><mid|\|>n=0\<ldots\>5;<space|1spc>k=0\<ldots\>n|]>;
    <|unfolded-io-math>
      [1,1,1,1,2,1,1,3,3,1,1,4,6,4,1,1,5,10,10,5,1]
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|<binom|n|k><mid|\|><stack|<tformat|<table|<row|<cell|n=0\<ldots\>5;>>|<row|<cell|k=0\<ldots\>n>>>>>|]>;
    <|unfolded-io-math>
      [1,1,1,1,2,1,1,3,3,1,1,4,6,4,1,1,5,10,10,5,1]
    </unfolded-io-math>
  </session>

  And suddenly Pure starts looking a lot like real mathematical notation! But
  wait, there's more. The <TeXmacs> <samp|choice> construct can be used to
  write simple Pure function definitions doing case analysis in a compact and
  pretty way. For instance:<\footnote>
    Note that the <strong|if> keyword is mandatory here, as it is required by
    the Pure syntax (as are the semicolons). The <strong|otherwise> keyword,
    on the other hand, is just syntactic sugar for an empty guard and is
    completely optional, although it often improves readability.
  </footnote>

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      f<around*|(|n|)> = <choice|<tformat|<table|<row|<cell|1>|<cell|<math-bf|if
      >n\<leqslant\>0>>|<row|<cell|n\<times\>f<around*|(|n-1|)>>|<cell|<math-bf|otherwise>>>>>>;<text|<verbatim|
      // the factorial>>
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      map f <around*|(|0\<ldots\>12|)>;
    <|unfolded-io-math>
      [1,1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600]
    </unfolded-io-math>
  </session>

  The above definition is in fact equivalent to the following verbatim Pure
  code (which isn't all that unreadable either, as Pure's function definition
  syntax mimics mathematical notation very closely):

  <\session|pure|dummy>
    <\input>
      \<gtr\>\ 
    <|input>
      f(n) = 1 if n\<less\>=0; = n*f(n-1) otherwise;
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      show f
    <|unfolded-io>
      f n = 1 if n\<less\>=0;

      f n = n*f (n-1);
    </unfolded-io>
  </session>

  The <samp|choice> construct can also be used with Pure's pattern-matching
  <verbatim|case> expressions:

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      <around*|\||<math-it|xs>\<colons\>list|\|>=<math-bf|case>
      <math-it|xs><space|0.8spc><math-bf|of><space|0.8spc><choice|<tformat|<table|<row|<cell|<around*|[||]>>|<cell|=>|<cell|0>|<cell|>>|<row|<cell|x:<math-it|xs>>|<cell|=>|<cell|1+<around*|\||<math-it|xs>|\|>>|<cell|<math-bf|otherwise>>>>>>;
    </input-math>
  </session>

  As you probably noticed, we've just defined
  <math|<around*|\||<math-it|xs>|\|>> (a.k.a. <verbatim|abs xs> in Pure
  notation) as our own variant of the list size function. For instance, let's
  count the number of primes up to 5000 (this may take a little while; alas,
  our definition of the prime sieve isn't very efficient either!):

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\||takewhile <around*|(|\<leqslant\>5000|)>
      <math-ss|primes>|\|>;
    <|unfolded-io-math>
      669
    </unfolded-io-math>
  </session>

  <subsection|Caveats>

  <TeXmacs> doesn't know the Pure syntax; as far as it is concerned, Pure's
  functions, operators and keywords are just text. So there are situations in
  which you have to help the converter along by adding parentheses to
  disambiguate the parsing. This is true, in particular, for big operators
  (integrals, sums, etc., especially in conjunction with Pure <verbatim|with>
  and <verbatim|when> clauses) and differentials. Even an invisible bracket
  (shortcut: <key|( Space>) will do the trick. For instance:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\>x<rsup|2>/\<mathd\>x; <text|// ! missing bracket around
      <math|x<rsup|2> > !>
    <|unfolded-io-math>
      <\errput>
        \<less\>stdin\<gtr\>, line 83: unhandled exception 'bad_diff (d x 2/d
        x 1)' while evaluating 'd x 2/d x'
      </errput>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\><around*|\<nobracket\>|<around*|(|x<rsup|2>|)>|\<nobracket\>>/\<mathd\>x;<text|
      // parentheses around <math|x<rsup|2>> disambiguate the construct>
    <|unfolded-io-math>
      df (x^2) x
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\><around*|\<nobracket\>|x<rsup|2>|\<nobracket\>>/\<mathd\>x;<text|
      // invisible brackets around <math|x<rsup|2>> work, too>
    <|unfolded-io-math>
      df (x^2) x
    </unfolded-io-math>
  </session>

  Here's another example:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|sum><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>,<big|prod><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>
      <math-bf|when >n = 5<math-bf| end>;
    <|unfolded-io-math>
      <\errput>
        \<less\>stdin\<gtr\>, line 86: syntax error, unexpected '\|',
        expecting end
      </errput>
    </unfolded-io-math>
  </session>

  The tuple (comma operator) actually binds stronger than the <verbatim|when>
  clause, so this is valid Pure syntax. But <TeXmacs> doesn't know about the
  <verbatim|when> syntax; for it the ``<math|<around*|(|2*k-1|)>
  <math-bf|when >n>'' looks like an ordinary term belonging under the product
  on the right, which is followed by an equals sign and another term
  ``<math|5<math-bf| end>>''. This makes perfect sense for <TeXmacs>, but
  it's not valid Pure syntax. This wouldn't normally be a problem (Pure would
  be able to reparse the expression correctly anyway), if it wasn't for the
  <math|<big|prod>>operator which translates to a Pure list comprehension.
  The initial part of the <verbatim|when> clause now ends up in this list
  comprehension where it shouldn't be, hence the somewhat surprising syntax
  error.

  In such cases it is often helpful to have a look at the converted
  expression. A neat trick to do this is to just copy and paste the entire
  expression to another input line operated in verbatim mode. This shows us
  exactly what went wrong here:

  <\session|pure|dummy>
    <\input>
      \<gtr\>\ 
    <|input>
      sum [(2*k-1)\|k=1..n],prod [(2*k-1) when n \|k=1..n]= 5 end;
    </input>
  </session>

  Placing brackets around either the entire tuple or just the product on the
  right correctly resolves the ambiguity. In this case, we might actually
  prefer to insert visible parentheses, since they make the expression easier
  to parse for human readers, too:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|<big|sum><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>,<big|prod><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>|)>
      <math-bf|when >n = 5<math-bf| end>;
    <|unfolded-io-math>
      25,945
    </unfolded-io-math>
  </session>

  We can again copy/paste the expression to a verbatim input line, to confirm
  that it was converted correctly this time:

  <\session|pure|dummy>
    <\input>
      \<gtr\>\ 
    <|input>
      (sum [(2*k-1)\|k=1..n],prod [(2*k-1)\|k=1..n]) when n = 5 end;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
    <associate|prog-scripts|pure-script>
    <associate|sfactor|4>
    <associate|src-close|repeat>
  </collection>
</initial>