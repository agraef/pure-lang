<TeXmacs|1.0.7.16>

<style|<tuple|generic|varsession|tmdoc-keyboard>>

<\body>
  <doc-data|<doc-title|Math Input>>

  Start up a Pure session and load the <verbatim|math> and <verbatim|reduce>
  modules, as well as the <verbatim|texmacs> module which provides some
  convenience functions for use with Pure in <TeXmacs>.

  <\session|pure|default>
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

  <key|Ctrl+$><math| switches to math mode\<nocomma\>\<nocomma\>>, and the
  <verbatim|?> prefix operator does a Reduce <verbatim|simplify> of its
  expression argument which is quoted automagically.

  <\session|pure|default>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?df <around*|(|sin <around*|(|x<rsup|2>|)>|)> x;
    <|unfolded-io-math>
      2*cos (x^2)*x
    </unfolded-io-math>
  </session>

  This has the same effect as:

  <\session|pure|default>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify $ <rprime|'>df <around*|(|sin <around*|(|x<rsup|2>|)>|)> x;
    <|unfolded-io-math>
      2*cos (x^2)*x
    </unfolded-io-math>
  </session>

  The <verbatim|?:> operator does the same, but evaluates its argument. Note
  the difference:

  <\session|pure|default>
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

  Most mathematical expressions are mapped to corresponding Pure expressions
  in a sensible way:

  <\session|pure|default>
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
      <around*|(|\<backslash\>x\<rightarrow\>2*x+\<alpha\>|)>
      5;<with|mode|prog| // lambdas>
    <|unfolded-io-math>
      10+alpha
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>;<text|<verbatim|
      // matrices>>
    <|unfolded-io-math>
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
      1\<ldots\>10;<text|<verbatim| // arithmetic sequences>>
    <|unfolded-io-math>
      [1,2,3,4,5,6,7,8,9,10]
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
      <wide|x|^>;<wide|x|~>;<wide|x|\<bar\>>;<wide|x|\<vect\>>;<wide|x|\<check\>>;<wide|x|\<breve\>>;<wide|x|\<dot\>>;<wide|x|\<ddot\>>;<wide|x|\<acute\>>;<wide|x|\<grave\>>;<rprime|'>x;<rprime|''>x;<text|<verbatim|
      // accents and primes/quotes>>
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
  </session>

  More examples using list comprehensions:

  <\session|pure|default>
    <\input>
      \<gtr\>\ 
    <|input>
      <math|<around*|\<\|\|\>|x\<colons\>matrix|\<\|\|\>> = <sqrt|sum
      <around*|[|x<rsup|2><mid|\|> x=x|]>>;>
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

    <\input>
      \<gtr\>\ 
    <|input>
      <math|<math-ss|primes> = <math-ss|sieve> (2\<ldots\>\<infty\>)
      <math-bf|with> <math-ss|sieve> (p:qs) = p : <math-ss|sieve> [q<mid|\|>q
      = qs; q <math-ss|mod> p] &; <math-bf|end>;>
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

  Big operators (integrals, limits, sums, etc.):

  <\session|pure|default>
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

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<big|sum><rsub|k=1\<ldots\>5><around*|(|2*k-1|)>;<big|prod><rsub|k=1\<ldots\>5><around*|(|2*k-1|)>;>
    <|unfolded-io>
      25

      945
    </unfolded-io>

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

    <\input>
      \<gtr\>\ 
    <|input>
      let X = [1..n \| n = 1..inf]; let n = 10;
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      <math|<big|cap><rsub|k=1><rsup|n>X<rsub|k-1>;<big|cup><rsub|k=1><rsup|n>X<rsub|k-1>;
      <big|cap><rsub|k=1><rsup|n><big|cup><rsub|l=1><rsup|k><around*|(|X<rsub|n>-X<rsub|l-1>|)>;>
    <|unfolded-io>
      [1]

      [1,2,3,4,5,6,7,8,9,10]

      [2,3,4,5,6,7,8,9,10,11]
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      clear n X
    </input>

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

  Differentials and integrals:

  <\session|pure|default>
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

  Calculate differentials and integrals using Reduce (<verbatim|?> operator):

  <\session|pure|default>
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
      when b=0 end;
    <|unfolded-io-math>
      x^n*a^n*x^3/(n+3)
    </unfolded-io-math>
  </session>

  Calculating various instances of an integral using a Pure function:

  <\session|pure|default>
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

  <subsection|Caveat!>

  <TeXmacs> doesn't know the Pure syntax. As far as it is concerned, Pure's
  functions, operators and keywords are just mathematical text. So there are
  situations in which you have to help the converter by adding parentheses to
  disambiguate the parsing. This is true, in particular, for big operators
  (integrals, sums, etc., especially in conjunction with Pure <verbatim|with>
  and <verbatim|when> clauses) and differentials. Even an invisible bracket
  (shortcut: <key|( Space>) will do the trick. For instance:

  <\session|pure|default>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\>x<rsup|2>/\<mathd\>x; <text|// ! missing bracket around
      <math|x<rsup|2> > !>
    <|unfolded-io-math>
      <\errput>
        \<less\>stdin\<gtr\>, line 59: unhandled exception 'bad_diff (d x 2/d
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

  Here's another, more startling example:

  <\session|pure|default>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|sum><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>,<big|prod><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>
      when n = 5 end;
    <|unfolded-io-math>
      <\errput>
        \<less\>stdin\<gtr\>, line 62: syntax error, unexpected '\|',
        expecting end
      </errput>
    </unfolded-io-math>
  </session>

  The tuple (comma operator) actually binds stronger than the <verbatim|when>
  clause, so this is valid Pure syntax. But <TeXmacs> doesn't know about the
  <verbatim|when> syntax; for it the ``<math|<around*|(|2*k-1|)> when n>''
  looks like an ordinary term belonging under the product on the right, which
  is followed by an equals sign and another term ``<math|5 end>''. This makes
  perfect sense for <TeXmacs>, but it's not valid Pure syntax. This wouldn't
  normally be a problem (Pure would be able to reparse the expression
  correctly anyway), if it wasn't for the <math|<big|prod>>operator which
  translates to a Pure list comprehension. The initial part of the
  <verbatim|when> clause now ends up in this list comprehension where it
  shouldn't be, hence the somewhat surprising syntax error.

  In such cases it is often helpful to have a look at the converted
  expression. A neat trick to do this is to just copy and paste the entire
  expression to another input line operated in verbatim mode. This shows us
  exactly what went wrong here:

  <\session|pure|default>
    <\input>
      \<gtr\>\ 
    <|input>
      sum [(2*k-1)\|k=1..n],prod [(2*k-1) when n \|k=1..n]= 5 end;
    </input>
  </session>

  Placing brackets around either the entire tuple or just the product on the
  right correctly resolves the ambiguity. In this case, we might actually
  prefer to insert visible parentheses (angle brackets can also be used),
  since they make the expression easier to parse for human readers, too:

  <\session|pure|default>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\<langle\>|<big|sum><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>,<big|prod><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>|\<rangle\>>
      when n = 5 end;
    <|unfolded-io-math>
      25,945
    </unfolded-io-math>
  </session>

  We can again copy/paste the expression to a verbatim input line, to confirm
  that the expression was converted correctly this time:

  <\session|pure|default>
    <\input>
      \<gtr\>\ 
    <|input>
      (sum [(2*k-1)\|k=1..n],prod [(2*k-1)\|k=1..n]) when n = 5 end;
    </input>
  </session>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
    <associate|prog-scripts|pure-script>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
  </collection>
</references>