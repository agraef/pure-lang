<TeXmacs|1.0.7.16>

<style|<tuple|tmdoc|varsession>>

<\body>
  <section|Mathematical Input and Output>

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
  must switch first the input line to <em|math input mode>. To do that, you
  can go search the toolbar for <samp|Input Options \| Mathematical Input>
  and check it, or type the key combination <key|Ctrl $> defined by the Pure
  plugin. Another useful convenience is the <verbatim|?> prefix operator
  (defined in <verbatim|texmacs.pure>) which does a Reduce
  <verbatim|simplify> of its expression argument which is quoted
  automagically. So here's how we can enter the expression <verbatim|? df
  (sin (x^2)) x)> in math mode:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      ?df <around*|(|sin <around*|(|x<rsup|2>|)>|)> x;
    <|unfolded-io-math>
      2*cos (x^2)*x
    </unfolded-io-math>
  </session>

  This has pretty much the same effect as:

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

  Once we change to <em|math output mode>, simplifications are done
  automatically by the pretty-printer, so the <verbatim|simplify>,
  <verbatim|?> and <verbatim|?:> operations are not needed at the command
  line any more (you might still need them if they are used programmatically,
  though). Note that this only works with expressions which Reduce
  understands; there are a lot of expression types in Pure which have no
  Reduce counterparts and are thus printed verbatim. However, the
  pretty-printer will try to print as much of the results in math mode as it
  can. In the following, we always leave math output enabled.

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      math;
    <|unfolded-io-math>
      ()
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      df <around*|(|sin <around*|(|x<rsup|2>|)>|)> x;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|2*cos<around*|(|x<rsup|2>|)>*x>
    </unfolded-io-math>
  </session>

  <subsection|Basic expressions>

  Most mathematical expressions are mapped to corresponding Pure expressions
  in a sensible way. We start out with some Reduce declarations of operators
  to be used below. (This isn't strictly necessary, but gets rid of Reduce's
  noisy ``declared operator'' messages.)

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      declare<space|1spc>operator <around*|[|above,below,binom,tree|]>;
    <|unfolded-io-math>
      ()
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      declare<space|1spc>operator <around*|[|hat,tilde,bar,vect,check,breve,dot,ddot,acute,grave|]>;
    <|unfolded-io-math>
      ()
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|\<backslash\>x\<rightarrow\>2*x+\<alpha\>|)>
      5;<with|mode|prog| // lambdas>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|\<alpha\>+<with|math-font-family|rm|10>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      x<rsub|k>,x<rsup|k>,<lsub|k>x,<lsup|k>x,<below|x|k>,<above|x|k>;<text|<verbatim|
      // sub- and superscripts>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|x>!<with|color|black|mode|math|math-display|true|k>,<with|color|black|mode|math|math-display|true|x<rsup|k>>,<with|color|black|mode|math|math-display|true|x>!<with|color|black|mode|math|math-display|true|k>,<with|color|black|mode|math|math-display|true|x<rsup|k>>,<with|color|black|mode|math|math-display|true|<math-up|below><around*|(|x,k|)>>,<with|color|black|mode|math|math-display|true|<math-up|above><around*|(|x,k|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <frac|x|y>,<tfrac|x|y>,<dfrac|x|y>,<frac*|x|y>,x/y;<with|mode|prog| //
      fractions>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|x|y>>,<with|color|black|mode|math|math-display|true|<frac|x|y>>,<with|color|black|mode|math|math-display|true|<frac|x|y>>,<with|color|black|mode|math|math-display|true|<frac|x|y>>,<with|color|black|mode|math|math-display|true|<frac|x|y>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <binom|n-1|k-1>+<binom|n-1|k>;<text|<verbatim| // binomials>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<math-up|binom><around*|(|n-1,k-1|)>+<math-up|binom><around*|(|n-1,k|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>,
      <tabular*|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>,
      <block*|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>;<text|<verbatim|
      // matrices and tables (various formats)>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|(|<tabular*|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>|)>>,<with|color|black|mode|math|math-display|true|<around*|(|<tabular*|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>|)>>,<with|color|black|mode|math|math-display|true|<around*|(|<tabular*|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <det|<tformat|<table|<row|<cell|a>|<cell|b>>|<row|<cell|b>|<cell|-a>>>>>;<text|<verbatim|
      // determinants>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|-<around*|(|a<rsup|2>+b<rsup|2>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <matrix|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|c>>>>>,
      <matrix|<tformat|<table|<row|<cell|a>>|<row|<cell|b>>|<row|<cell|c>>>>>;<text|<verbatim|
      // vectors>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|(|<tabular*|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|c>>>>>|)>>,<with|color|black|mode|math|math-display|true|<around*|(|<tabular*|<tformat|<table|<row|<cell|a>>|<row|<cell|b>>|<row|<cell|c>>>>>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <tree|a|b|c|<tree|x|y|z>>;<text|<verbatim| // trees>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|<math-up|tree><around*|(|a|)><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><math-up|tree><around*|(|b|)><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><math-up|tree><around*|(|c|)><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|<math-up|tree><around*|(|<math-up|tree><around*|(|x|)>|)><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><math-up|tree><around*|(|<math-up|tree><around*|(|y|)>|)><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><math-up|tree><around*|(|<math-up|tree><around*|(|z|)>|)>|]>|]>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      exp<around*|(|x|)>*\<alpha\>+\<beta\>;<text|<verbatim| // arithmetic>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|e<rsup|x>*\<alpha\>+\<beta\>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <sqrt|x<rsup|2>+y<rsup|2>>, <sqrt|x|3>;<text|<verbatim| // roots>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<sqrt|x<rsup|2>+y<rsup|2>>>,<with|color|black|mode|math|math-display|true|<sqrt|x|<space|0.25spc>3>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<neg\>A\<wedge\><around*|(|B\<vee\>C|)>;<text|<verbatim| // logic>>
    <|unfolded-io-math>
      ~<with|color|black|mode|math|math-display|true|A>&&(<with|color|black|mode|math|math-display|true|B>\|\|<with|color|black|mode|math|math-display|true|C>)
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      x\<gtr\>y, x\<less\>y, x\<geqslant\>y,x\<leqslant\>y,x\<longequal\>y,x\<neq\>y;<text|<verbatim|
      // comparisons>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|x>\<gtr\><with|color|black|mode|math|math-display|true|y>,<with|color|black|mode|math|math-display|true|x>\<less\><with|color|black|mode|math|math-display|true|y>,<with|color|black|mode|math|math-display|true|x>\<gtr\>=<with|color|black|mode|math|math-display|true|y>,<with|color|black|mode|math|math-display|true|x>\<less\>=<with|color|black|mode|math|math-display|true|y>,<with|color|black|mode|math|math-display|true|x\<longequal\>y>,<with|color|black|mode|math|math-display|true|x>~=<with|color|black|mode|math|math-display|true|y>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      x\<equiv\>y, x\<nequiv\>y;<text|<verbatim| // syntactic equality (=== /
      ~== in Pure)>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|0>,<with|color|black|mode|math|math-display|true|1>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|1,2,3|]>;<text|<verbatim| // lists>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3|]>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      1\<ldots\>10; 1:3\<ldots\>11;<text|<verbatim| // arithmetic sequences>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>6<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>7<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>8<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>9<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10>|]>>

      <with|color|black|mode|math|math-display|true|<around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>7<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>9<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|11>|]>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|a+1 <mid|\|> a=1\<ldots\>10;a mod 2|]>;<text|<verbatim| //
      comprehensions>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>6<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>8<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10>|]>>
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
      <with|color|black|mode|math|math-display|true|a>,<with|color|black|mode|math|math-display|true|b+1>

      <with|color|black|mode|math|math-display|true|<around*|[|a<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>b+1|]>>

      <with|color|black|mode|math|math-display|true|a>,<with|color|black|mode|math|math-display|true|b+1>

      <with|color|black|mode|math|math-display|true|<around*|[|a<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>b+1|]>>

      <with|color|black|mode|math|math-display|true|<math-up|floor><around*|(|<frac|n|2>|)>>

      <with|color|black|mode|math|math-display|true|<math-up|ceiling><around*|(|<frac|n|2>|)>>

      <with|color|black|mode|math|math-display|true|<math-up|abs><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|norm><around*|(|x|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <wide|x|^>;<wide|x|~>;<wide|x|\<bar\>>;<wide|x|\<vect\>>;<wide|x|\<check\>>;<wide|x|\<breve\>>;<wide|x|\<dot\>>;<wide|x|\<ddot\>>;<wide|x|\<acute\>>;<wide|x|\<grave\>>;<rprime|'>x;<rprime|''>x;
      <neg|x>;<text|<verbatim| // accents and primes/quotes>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<math-up|hat><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|tilde><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|bar><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|vect><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|check><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|breve><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|dot><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|ddot><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|acute><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<math-up|grave><around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|x>

      '<with|color|black|mode|math|math-display|true|x>

      ~<with|color|black|mode|math|math-display|true|x>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      x\<oplus\>y;x\<ominus\>y;x\<otimes\>y;x\<oslash\>y;x\<pm\>y;x\<mp\>y;x\<div\>y;x\<cap\>y;x\<cup\>y;x\<uplus\>y;<text|<verbatim|
      // infix operators>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|x> oplus
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> ominus
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> otimes
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> oslash
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> pm
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> mp
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> div
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> cap
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> cup
      <with|color|black|mode|math|math-display|true|y>

      <with|color|black|mode|math|math-display|true|x> uplus
      <with|color|black|mode|math|math-display|true|y>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<alpha\>;\<Gamma\>;\<b-up-Z\>;\<b-z\>;\<b-0\>;\<cal-C\>;\<frak-F\>;\<frak-u\>,\<frak-v\>,\<frak-w\>;\<bbb-Q\>;<text|<verbatim|
      // Greek symbols, special glyphs>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|\<alpha\>>

      <with|color|black|mode|math|math-display|true|\<Gamma\>>

      <with|color|black|mode|math|math-display|true|Z>

      <with|color|black|mode|math|math-display|true|z>

      <with|color|black|mode|math|math-display|true|0>

      <with|color|black|mode|math|math-display|true|C>

      <with|color|black|mode|math|math-display|true|F>

      <with|color|black|mode|math|math-display|true|u>,<with|color|black|mode|math|math-display|true|v>,<with|color|black|mode|math|math-display|true|w>

      <with|color|black|mode|math|math-display|true|math-font-family|rm|QQ>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <math-bf|foo>,<math-it|bar>,<math-sl|baz>,<math-ss|gnu>,<math-tt|gna>,<math-up|gnats>;<text|<verbatim|
      // special markup>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|math-font-family|rm|foo>,<with|color|black|mode|math|math-display|true|math-font-family|rm|bar>,<with|color|black|mode|math|math-display|true|math-font-family|rm|baz>,<with|color|black|mode|math|math-display|true|math-font-family|rm|gnu>,<with|color|black|mode|math|math-display|true|math-font-family|rm|gna>,<with|color|black|mode|math|math-display|true|math-font-family|rm|gnats>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <really-tiny|A,B>, <tiny|A,B>, <very-small|A,B>, <small|A,B>,
      <normal-size|A,B>, <large|A,B>, <very-large|A,B>, <huge|A,B>,
      <really-huge|A,B>;<text|<verbatim| // various sizes>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>,<with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>,<with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>,<with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>,<with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>,<with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>,<with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>,<with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>,<with|color|black|mode|math|math-display|true|A>,<with|color|black|mode|math|math-display|true|B>
    </unfolded-io-math>
  </session>

  More examples using list and matrix comprehensions:<\footnote>
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
      <with|color|black|mode|math|math-display|true|<around*|[|3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>7<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>9<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|11>|]>>
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
      <with|color|black|mode|math|math-display|true|<around*|(|<tabular*|<tformat|<table|<row|<cell|1>|<cell|0>|<cell|0>>|<row|<cell|0>|<cell|1>|<cell|0>>|<row|<cell|0>|<cell|0>|<cell|1>>>>>|)>>
    </unfolded-io-math>

    <\input-math>
      \<gtr\>\ 
    <|input-math>
      <math-bf|let> P=sieve (2\<ldots\>\<infty\>)
      <math-bf|with><space|1spc>sieve(p:<math-it|qs>) = p :sieve[q<mid|\|>q =
      <math-it|qs>; q <math-ss|mod> p] & <math-bf|end>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      P!!<around*|(|0\<ldots\>20|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>7<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|11><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|13><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|17><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|19><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|23><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|29><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|31><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|37><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|41><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|43><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|47><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|53><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|59><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|61><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|67><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|71><space|0.25spc><space|0.25spc>|\<nobracket\>>>

      <with|color|black|mode|math|math-display|true|<around*|\<nobracket\>|<with|math-font-family|rm|,<space|0.25spc>>73|]>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      list <around*|(|take 20 <around*|(|drop 100 P|)>|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|<with|math-font-family|rm|547><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|557><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|563><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|569><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|571><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|577><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|587><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|593><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|599><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|601><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|607><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|613><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|617><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|619><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|631><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>|\<nobracket\>>>

      <with|color|black|mode|math|math-display|true|<around*|\<nobracket\>|641<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|643><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|647><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|653><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|659>|]>>
    </unfolded-io-math>
  </session>

  List and vector/matrix data can be exchanged between Pure and Reduce in a
  seamless fashion. This makes it easy to inspect and manipulate compound
  results returned by Reduce, such as lists of partial fractions:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      let p=2/<around*|(|<around*|(|x+1|)><rsup|2>*<around*|(|x+2|)>|)>; p;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|2|x<rsup|3>+4*x<rsup|2>+5*x+2>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      let pfs = ?:pf p x; pfs;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|<frac|2|x+2><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><frac|-2|x+1><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><frac|2|x<rsup|2>+2*x+1>|]>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      map <around*|(|\\y\<rightarrow\>df y<space|1spc>x|)> pfs;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|<frac|-2|x<rsup|2>+4*x+4><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><frac|2|x<rsup|2>+2*x+1><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><frac|-4|x<rsup|3>+3*x<rsup|2>+3*x+1>|]>>
    </unfolded-io-math>
  </session>

  Another example: equation solving.

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      let<space|1spc>eqn=log<around*|(|sin<around*|(|x+3|)>|)><rsup|5>\<longequal\>8;
      eqn;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|log<around*|(|sin<around*|(|x+3|)>|)><rsup|5>\<longequal\>8>
    </unfolded-io-math>

    <\input-math>
      \<gtr\>\ 
    <|input-math>
      let solns=?:solve eqn<space|1spc>x;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      #solns; head solns;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|10>

      <with|color|black|mode|math|math-display|true|x\<longequal\>2*<math-up|arbint><around*|(|5|)>*\<pi\>+arcsin<around*|(|<frac|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>|)>|exp<around*|(|2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>>|)>-3>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      tail solns;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|x\<longequal\>2*<math-up|arbint><around*|(|5|)>*\<pi\>-arcsin<around*|(|<frac|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>|)>|exp<around*|(|2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>>|)>+\<pi\>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>2*<math-up|arbint><around*|(|4|)>*\<pi\>+arcsin<around*|(|<frac|1|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>*i|)>>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>|\<nobracket\>>>

      <with|color|black|mode|math|math-display|true|x\<longequal\>2*<math-up|arbint><around*|(|4|)>*\<pi\>-arcsin<around*|(|<frac|1|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>*i|)>>|)>+\<pi\>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>2*<math-up|arbint><around*|(|3|)>*\<pi\>+arcsin<around*|(|<frac|e<rsup|2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>*i>|e<rsup|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>>>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>>

      <with|color|black|mode|math|math-display|true|x\<longequal\>2*<math-up|arbint><around*|(|3|)>*\<pi\>-arcsin<around*|(|<frac|e<rsup|2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>*i>|e<rsup|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>>>|)>+\<pi\>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>2*<math-up|arbint><around*|(|2|)>*\<pi\>+arcsin<around*|(|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>>

      <with|color|black|mode|math|math-display|true|x\<longequal\>2*<math-up|arbint><around*|(|2|)>*\<pi\>-arcsin<around*|(|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>|)>+\<pi\>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>2*<math-up|arbint><around*|(|1|)>*\<pi\>+arcsin<around*|(|e<rsup|2<rsup|3/5>>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>>

      <with|color|black|mode|math|math-display|true|<around*|\<nobracket\>|x\<longequal\>2*<math-up|arbint><around*|(|1|)>*\<pi\>-arcsin<around*|(|e<rsup|2<rsup|3/5>>|)>+\<pi\>-3|]>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      let solns=?:reduce_with <around*|[|arbint\<Rightarrow\>cst
      0|]><space|1spc>solns; solns;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|x\<longequal\>arcsin<around*|(|<frac|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>|)>|exp<around*|(|2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>-arcsin<around*|(|<frac|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>|)>|exp<around*|(|2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>>|)>+\<pi\>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>arcsin<around*|(|<frac|1|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>*i|)>>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>|\<nobracket\>>>

      <with|color|black|mode|math|math-display|true|x\<longequal\>-arcsin<around*|(|<frac|1|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>*i|)>>|)>+\<pi\>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>arcsin<around*|(|<frac|e<rsup|2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>*i>|e<rsup|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>>>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>-arcsin<around*|(|<frac|e<rsup|2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>*i>|e<rsup|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>>>|)>+\<pi\>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>>

      <with|color|black|mode|math|math-display|true|x\<longequal\>arcsin<around*|(|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>-arcsin<around*|(|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>|)>+\<pi\>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>x\<longequal\>arcsin<around*|(|e<rsup|2<rsup|3/5>>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>>

      <with|color|black|mode|math|math-display|true|<around*|\<nobracket\>|x\<longequal\>-arcsin<around*|(|e<rsup|2<rsup|3/5>>|)>+\<pi\>-3|]>>
    </unfolded-io-math>

    <\input-math>
      \<gtr\>\ 
    <|input-math>
      check \ <around*|(|u\<longequal\>v|)>
      <around*|(|x\<longequal\>y|)>=eval<around*|(|?:reduce_with
      <around*|[|x\<Rightarrow\>y|]> <around*|\||u-v|\|>|)>;
    </input-math>

    <\input-math>
      \<gtr\>\ 
    <|input-math>
      \<Delta\> s=check eqn<space|1spc>s;let \<varepsilon\>=10<rsup|-8>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|y<mid|\|>x\<longequal\>y=solns;\<Delta\><around*|(|x\<longequal\>y|)>\<longequal\>0|]>;<text|<verbatim|
      // exact solutions>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|arcsin<around*|(|e<rsup|2<rsup|3/5>>|)>-3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>-arcsin<around*|(|e<rsup|2<rsup|3/5>>|)>+\<pi\>-3|]>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|y<mid|\|> s@<around*|(|x\<longequal\>y|)>=solns;\<Delta\>
      s\<neq\>0\<wedge\>\<Delta\> s\<leqslant\>\<varepsilon\>|]>;<text|<verbatim|
      // inexact solutions>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|arcsin<around*|(|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>+2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>*i|)>|)>-3|]>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|y\<Rightarrow\>\<Delta\> s<mid|\|>
      s@<around*|(|x\<longequal\>y|)>=solns;\<Delta\>
      s\<gtr\>\<varepsilon\>|]>;<text|<verbatim| // what's up with these??>>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|arcsin<around*|(|<frac|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>|)>|exp<around*|(|<around*|(|2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>|)>*i|)>>|)>-3\<Rightarrow\><with|math-font-family|rm|7.8764><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|(|-arcsin<around*|(|<frac|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>|)>|exp<around*|(|<around*|(|2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>|)>*i|)>>|)>+\<pi\>|)>-3\<Rightarrow\><with|math-font-family|rm|7.8764><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>|\<nobracket\>>>

      <with|color|black|mode|math|math-display|true|arcsin<around*|(|<frac|1|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>+<around*|(|2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>|)>*i|)>>|)>-3\<Rightarrow\><with|math-font-family|rm|7.8764><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|(|-arcsin<around*|(|<frac|1|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>+<around*|(|2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>|)>*i|)>>|)>+\<pi\>|)>-3\<Rightarrow\><with|math-font-family|rm|7.8764><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>>

      <with|color|black|mode|math|math-display|true|arcsin<around*|(|<frac|exp<around*|(|<around*|(|2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>|)>*i|)>|e<rsup|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>>>|)>-3\<Rightarrow\><with|math-font-family|rm|7.8764><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|(|-arcsin<around*|(|<frac|exp<around*|(|<around*|(|2<rsup|3/5>*sin<around*|(|<frac|\<pi\>|5>|)>|)>*i|)>|e<rsup|2<rsup|3/5>*cos<around*|(|<frac|\<pi\>|5>|)>>>|)>+\<pi\>|)>-3\<Rightarrow\><with|math-font-family|rm|7.8764><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>>

      <with|color|black|mode|math|math-display|true|<around*|\<nobracket\>|<around*|(|-arcsin<around*|(|exp<around*|(|2<rsup|3/5>*cos<around*|(|<frac|2*\<pi\>|5>|)>+<around*|(|2<rsup|3/5>*sin<around*|(|<frac|2*\<pi\>|5>|)>|)>*i|)>|)>+\<pi\>|)>-3\<Rightarrow\><with|math-font-family|rm|7.8764>|]>>
    </unfolded-io-math>
  </session>

  <subsection|Big operators (integrals, limits, sums, etc.)>

  Big operators (<samp|Insert \| Symbol \| Big operator> in math mode) are
  mapped to corresponding Pure expressions, generally using a
  Reduce-compatible form:

  <\session|pure|math>
    \;

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int>sin x \<mathd\>x;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|-cos<around*|(|x|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int><rsub|a><rsup|b>x<rsup|2>\<mathd\>x;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|-a<rsup|3>+b<rsup|3>|3>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|intlim><rsub|a><rsup|b>x<rsup|2>\<mathd\>x;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|-a<rsup|3>+b<rsup|3>|3>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      lim<rsub|x\<rightarrow\>0><around*|(|1/x|)>;
      lim<rsub|x\<rightarrow\>\<infty\>><around*|(|x*sin<around*|(|1/x|)>|)>;lim<rsub|x\<rightarrow\>\<infty\>><frac|1|x>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|\<infty\>>

      <with|color|black|mode|math|math-display|true|1>

      <with|color|black|mode|math|math-display|true|0>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int>sin x \<mathd\>x;<big|int><rsub|a><rsup|b>x<rsup|2>\<mathd\>x;<big|intlim><rsub|a><rsup|b>x<rsup|2>\<mathd\>x;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|-cos<around*|(|x|)>>

      <with|color|black|mode|math|math-display|true|<frac|-a<rsup|3>+b<rsup|3>|3>>

      <with|color|black|mode|math|math-display|true|<frac|-a<rsup|3>+b<rsup|3>|3>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|sum><rsub|k=1><rsup|n><around*|(|2*k-1|)>;<big|prod><rsub|k=1><rsup|n><around*|(|2*k-1|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|n<rsup|2>>

      <with|color|black|mode|math|math-display|true|<frac|2*\<gamma\>*<around*|(|2*n|)>|2<rsup|n>*\<gamma\><around*|(|n|)>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|sum><rsub|k=0><rsup|n-1><around*|(|a+k*r|)>;
      <big|prod><rsub|k=1><rsup|n><frac|k|k+2>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|n*<around*|(|2*a+n*r-r|)>|2>>

      <with|color|black|mode|math|math-display|true|<frac|2|n<rsup|2>+3*n+2>>
    </unfolded-io-math>
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
      <with|color|black|mode|math|math-display|true|25>

      <with|color|black|mode|math|math-display|true|945>
    </unfolded-io>
  </session>

  Note that the above is equivalent to the following verbatim Pure code:

  <\verbatim-code>
    sum [(2*k-1)\|k=1..5]; prod [(2*k-1)\|k=1..5];
  </verbatim-code>

  The same holds for a number of other big operators, such as the big wedge
  and wee, which have no counterpart in Reduce. These aren't predefined in
  Pure either, but we can implement some useful Pure operations with them,
  for instance:

  <\session|pure|math>
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
      <with|color|black|mode|math|math-display|true|0>,<with|color|black|mode|math|math-display|true|1>
    </unfolded-io>
  </session>

  <subsection|Differentials and integrals>

  Both differentials and integrals can be specified using customary
  mathematical notation which gets translated to invocations of the Reduce
  <verbatim|df> and <verbatim|int> operators (the latter is actually named
  <verbatim|intg> in Pure, to avoid a name clash with the built-in Pure
  function <verbatim|int>).

  An integral is written with the big integral symbol (<verbatim|\\big>
  <verbatim|int> or <key|Shift+F5 I>), followed by the integrand, followed by
  the upright <math|\<mathd\>> symbol (<verbatim|\\mathd> or <key|d Tab
  Tab>), followed by the integration variable. There may be spaces around the
  <math|\<mathd\>> symbol, but nothing else. This will be translated to a
  Pure/Reduce call of the form <verbatim|intg f x>.

  Differentials use a somewhat more elaborate syntax and may be denoted in a
  number of ways, each involving the <math|\<mathd\>> symbol (or the partial
  symbol <math|\<partial\>>) and a fraction (any kind of fraction will do, as
  will the <verbatim|/> operator, but in the latter case you may have to
  parenthesize numerator and denominator accordingly). If you're lucky and
  entered everything correctly, the result is a corresponding Pure/Reduce
  call of the form <verbatim|df f x>. The following variations are supported:

  <\itemize>
    <item>A first-order differential may be written as
    <math|\<mathd\>f/\<mathd\>x>, with or without spaces around the
    <math|\<mathd\>> symbol. The Pure syntax requires that the function is
    put in parentheses if it is a compound expression (likewise the
    differentiation variable); e.g., <math|\<mathd\><around*|(|f
    x|)>/\<mathd\>x> works, as does <math|\<mathd\><around*|(|f
    <around*|(|x|)>|)>/\<mathd\>x>, but <em|not>
    <math|\<mathd\>f<around*|(|x|)>/\<mathd\>x> (invisible brackets will
    work, too, cf. Section <reference|Caveats>).

    <item>Higher orders may be denoted simply as <math|\<mathd\> 2
    f/\<mathd\> x 2> which is easy to type, and is also the way the Pure
    plugin represents these constructs internally.

    <item>Another quick way to type higher-order differentials, which has the
    advantage that it resembles customary notation, is to just tack on the
    order as a superscript instead: <math|\<mathd\><rsup|2>f/\<mathd\>x<rsup|2>>
    (i.e.: <verbatim|d^2 f/d x^2>). Note that according to Pure syntax, this
    would normally be parsed as <verbatim|d^(2 f)/(d x)^2>, which is bogus,
    but we can get away with it because the <verbatim|texmacs> module has
    some magic built into it which translates this to <verbatim|d 2 f/d x 2>.
    Hence this shortcut only works with math output or when using the
    <verbatim|?> operator, otherwise the fully multiplicative form below
    should be used.

    <item>The above form with the superscript can also be written with a
    multiplication sign between the d operator and the function or variable
    argument, so that it looks like this:
    <math|<dfrac|\<mathd\><rsup|2>\<ast\>f|\<mathd\>\<ast\>x<rsup|2>>>. (The
    multiplication signs are indicated explicitly here for clarity, but of
    course you will normally just type a literal <verbatim|*> character which
    is rendered as an invisible multiplication sign in math mode.) This
    representation is more effort to type, but it has the advantage that it
    parses correctly in Pure and will thus work even without the <verbatim|?>
    operator or math output mode.<\footnote>
      Note, however, that if you write this with the <verbatim|/> operator
      instead of an explicit fraction, you have to be careful to parenthesize
      the denominator like this: <math|\<mathd\><rsup|2>f/<around*|(|\<mathd\>x<rsup|2>|)>>.
      That's because the multiplication operators (which includes
      <verbatim|/>) are left-associative in Pure.
    </footnote> It is also the way Reduce itself prints unexpanded
    differentials in math output mode (albeit using the partial symbol).

    <item>No matter which variant of the notation you use, multiple
    differentiation variables can be given as a product of the corresponding
    differential terms in the denominator, such as
    <math|\<partial\><rsup|4>f/<around*|(|\<partial\>x<rsup|2>*\<partial\>y<rsup|2>|)>>,
    and the notation <math|\<mathd\>/\<mathd\>x*f> (with a multiplication
    sign between <math|\<mathd\>/\<mathd\>x*> and <math|f>) is provided as a
    alternative to <math|\<mathd\>f/\<mathd\>x>, which is often convenient.
  </itemize>

  Of course, in either case you may also just write the corresponding
  Pure/Reduce call, which is often easier to type, but doesn't nearly look as
  nice and mathematical. It is also instructive to take a look at how Reduce
  itself renders calls to <verbatim|intg> and <verbatim|df>; you can always
  copy such output to the input line again and it should just work. Here are
  some examples.

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      declare<space|1spc>depend <around*|[|f,x|]>;
    <|unfolded-io-math>
      ()
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      df f x 2, intg f x;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|\<partial\><rsup|2>*f|\<partial\>*x<rsup|2>>>,<with|color|black|mode|math|math-display|true|<big|int>f*<space|0.25spc>d*<space|0.25spc>x>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<partial\><rsup|2>*f/\<partial\>x<rsup|2>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|\<partial\><rsup|2>*f|\<partial\>*x<rsup|2>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int>2*f<around*|(|x|)>\<mathd\>x;
    <|unfolded-io-math>
      \;

      *** f declared operator\ 

      <with|color|black|mode|math|math-display|true|2*<big|int>f<around*|(|x|)>*<space|0.25spc>d*<space|0.25spc>x>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int>2*f<around*|(|x|)>\<mathd\> <around*|(|cos x|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|2*cos<around*|(|x|)>*f<around*|(|x|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\><rsup|2><around*|(|x<rsup|3>|)>/\<mathd\>x<rsup|2>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|6*x>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <frac|\<partial\><rsup|9><around*|(|x<rsup|2>*y<rsup|3>*z<rsup|4>|)>|\<partial\>x<rsup|2>*\<partial\>y<rsup|3>*\<partial\>z<rsup|4>>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|288>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\>/\<mathd\>x*<around*|(|x+y|)><rsup|5>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|5*<around*|(|x<rsup|4>+4*x<rsup|3>*y+6*x<rsup|2>*y<rsup|2>+4*x*y<rsup|3>+y<rsup|4>|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\>/\<mathd\>x*<around*|(|sin<around*|(|x|)>*cos<around*|(|x|)>|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|cos<around*|(|x|)><rsup|2>-sin<around*|(|x|)><rsup|2>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|int>sin<around*|(|2*x|)>\<mathd\>x;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|-cos<around*|(|2*x|)>|2>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\<nobracket\>|<big|int>x<rsup|2>*<around|(|a*x+b|)><rsup|n>\<mathd\>x|\<nobracket\>>
      <math-bf|when> b=0 <math-bf|end>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|x<rsup|n>*a<rsup|n>*x<rsup|3>|n+3>>
    </unfolded-io-math>
  </session>

  It is often convenient to abbreviate parametric Reduce expressions using
  Pure functions. We'll learn about different ways to enter Pure functions in
  math mode in a moment, but for the moment it suffices to know that the
  simplest form is just <math|f <space|0.2spc>x<rsub|1><space|0.2spc>\<cdots\><space|0.2spc>x<rsub|n>=<math-it|rhs>>
  where <math|f> is the name of the function,
  <math|x<rsub|1>,\<ldots\>,x<rsub|n>> are the parameters and <em|rhs> is the
  right-hand side (the body) of the definition. Note the missing parentheses
  around the parameters. Pure uses the curried notation for function
  applications where the parameters simply follow the function, similar to
  shell command syntax. For compatibility with Reduce, function calls in
  <em|Reduce> expressions can also be specified in the usual uncurried form
  <math|f<around*|(|x<rsub|1>,\<ldots\>,x<rsub|n>|)>>, but Pure definitions
  and expressions generally use the curried form. For instance:

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      I a b n = <big|int>x<rsup|2>*<around|(|a*x+b|)><rsup|n>\<mathd\>x;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      I a b n;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|<around*|(|a*x+b|)><rsup|n>*<around*|(|a<rsup|3>*n<rsup|2>*x<rsup|3>+3*a<rsup|3>*n*x<rsup|3>+2*a<rsup|3>*x<rsup|3>+a<rsup|2>*b*n<rsup|2>*x<rsup|2>+a<rsup|2>*b*n*x<rsup|2>-2*a*b<rsup|2>*n*x+2*b<rsup|3>|)>|a<rsup|3>*<around*|(|n<rsup|3>+6*n<rsup|2>+<with|math-font-family|rm|11>*n+6|)>>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      I a b 0;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|x<rsup|3>|3>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      I 0 b n;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|b<rsup|n>*x<rsup|3>|3>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      I a 0 k;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|x<rsup|k>*a<rsup|k>*x<rsup|3>|k+3>>
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
      <with|color|black|mode|math|math-display|true|math-font-family|rm|xs>!(<with|color|black|mode|math|math-display|true|1>..<with|color|black|mode|math|math-display|true|n>)
    </unfolded-io-math>
  </session>

  As you can see, this kind of expression isn't readily defined in Pure, so
  we can do it ourselves:

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      <around*|(|<math-it|xs>\<colons\>list|)><rsub|<math-it|ys>\<colons\>list>=<math-it|xs>!!<math-it|ys>;
    </input-math>
  </session>

  That's it. Now we can write:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|"a"\<ldots\>"z"|)><rsub|10\<ldots\>16>;
    <|unfolded-io-math>
      ["k","l","m","n","o","p","q"]
    </unfolded-io-math>

    <\input-math>
      \<gtr\>\ 
    <|input-math>
      <math-bf|let> P=sieve (2\<ldots\>\<infty\>)
      <math-bf|with><space|1spc>sieve(p:<math-it|qs>) = p :sieve[q<mid|\|>q =
      <math-it|qs>; q <math-ss|mod> p] & <math-bf|end>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      P<rsub|99\<ldots\>117>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|<with|math-font-family|rm|541><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|547><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|557><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|563><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|569><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|571><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|577><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|587><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|593><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|599><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|601><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|607><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|613><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|617><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|619><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>|\<nobracket\>>>

      <with|color|black|mode|math|math-display|true|<around*|\<nobracket\>|631<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|641><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|643><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|647>|]>>
    </unfolded-io-math>
  </session>

  For another example, let's consider the binomials:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <binom|n|k>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<math-up|binom><around*|(|n,k|)>>
    </unfolded-io-math>
  </session>

  This function isn't predefined in Pure either, so let's do that now. To get
  nicely aligned equations, we'll use an equation array this time. This is
  available as <verbatim|\\eqnarray*> in math mode; similarly, the binomials
  can be entered with <verbatim|\\binom>:

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
      <with|color|black|mode|math|math-display|true|<around*|[|<around*|[|1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>6<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]>|]>>
    </unfolded-io-math>
  </session>

  Or how about the entire Pascal triangle? As we already saw in the prime
  sieve example, Pure can deal with ``lazy'' lists (called <em|streams> in
  functional programming parlance) just fine. The following comprehension
  yields a stream containing all rows of the Pascal triangle. (Note that the
  algorithm embodied by the binomials function isn't all that efficient, so
  it will become <em|very> slow even for moderate values of <math|n>.)

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      binomials =<around*|[|<around*|[|<binom|n|k><mid|\|>k=0\<ldots\>n|]><mid|\|>n=0\<ldots\>\<infty\>|]>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      binomials; binomials<rsub|0\<ldots\>5>; binomials<rsub|16>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|1|]>>:#\<less\>thunk
      0x7ff3bd682c98\<gtr\>

      <with|color|black|mode|math|math-display|true|<around*|[|<around*|[|1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>6<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]>|]>>

      <with|color|black|mode|math|math-display|true|<around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|16><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|120><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|560><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|1820><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|4368><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|8008><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|11440><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|12870><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|11440><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|8008><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|4368><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|1820><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>|\<nobracket\>>>

      <with|color|black|mode|math|math-display|true|<around*|\<nobracket\>|560<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|120><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|16><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]>>
    </unfolded-io-math>
  </session>

  We can also align subterms in expressions, using the <TeXmacs> <samp|stack>
  construct. E.g., the \ <math|<stack|<tformat|<table|<row|<cell|x>|<cell|y>>|<row|<cell|z>|<cell|t>>>>>>
  stack will expand to just <verbatim|x y z t> in Pure (the converter
  traverses this construct in row-major order).

  So, for instance, the following two expressions are exactly the same in
  Pure syntax:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|[|<binom|n|k><mid|\|>n=0\<ldots\>5;<space|1spc>k=0\<ldots\>n|]>;<around*|[|<binom|n|k><mid|\|><stack|<tformat|<table|<row|<cell|n=0\<ldots\>5;>>|<row|<cell|k=0\<ldots\>n>>>>>|]>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>6<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]>>

      <with|color|black|mode|math|math-display|true|<around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>6<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>4<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|10><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>1|]>>
    </unfolded-io-math>
  </session>

  Moreover, the <TeXmacs> <samp|choice> construct can be used to write Pure
  function definitions involving guards in a compact and pretty way. For
  instance, here's another definition of the factorial, this time entered in
  math mode:<\footnote>
    Note that the <strong|if> keyword is mandatory here, as it is required by
    the Pure syntax (as are the semicolons). The <strong|otherwise> keyword
    is just syntactic sugar, however, although it often improves readability.
  </footnote>

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      f<around*|(|n|)> = <choice|<tformat|<table|<row|<cell|1>|<cell|<math-bf|if
      >n\<leqslant\>0>>|<row|<cell|n\<times\>f<around*|(|n-1|)>>|<cell|<math-bf|otherwise>>>>>>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      map f <around*|(|0\<ldots\>12|)>;
    <|unfolded-io-math>
      [<with|color|black|mode|math|math-display|true|1>,<with|color|black|mode|math|math-display|true|1>,<with|color|black|mode|math|math-display|true|2>,<with|color|black|mode|math|math-display|true|6>,<with|color|black|mode|math|math-display|true|24>,<with|color|black|mode|math|math-display|true|120>,<with|color|black|mode|math|math-display|true|720>,<with|color|black|mode|math|math-display|true|5040>,<with|color|black|mode|math|math-display|true|40320>,<with|color|black|mode|math|math-display|true|362880>,<with|color|black|mode|math|math-display|true|3628800>,<with|color|black|mode|math|math-display|true|39916800>,479001600]
    </unfolded-io-math>
  </session>

  The above definition is in fact equivalent to the following verbatim Pure
  code (which isn't all that unreadable either, as Pure's function definition
  syntax already mimics mathematical notation very closely):

  <\verbatim-code>
    f(n) = 1 if n\<less\>=0; = n*f(n-1) otherwise;
  </verbatim-code>

  Of course, the same construct can also be used to define local functions:

  <\session|pure|math>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      map f <around*|(|0\<ldots\>12|)><space|1spc><math-bf|with><space|1spc>f<around*|(|n|)>
      = <choice|<tformat|<table|<row|<cell|1>|<cell|<math-bf|if
      >n\<leqslant\>0>>|<row|<cell|n\<times\>f<around*|(|n-1|)>>|<cell|<math-bf|otherwise>>>>>><math-bf|end>;
    <|unfolded-io-math>
      [<with|color|black|mode|math|math-display|true|1>,<with|color|black|mode|math|math-display|true|1>,<with|color|black|mode|math|math-display|true|2>,<with|color|black|mode|math|math-display|true|6>,<with|color|black|mode|math|math-display|true|24>,<with|color|black|mode|math|math-display|true|120>,<with|color|black|mode|math|math-display|true|720>,<with|color|black|mode|math|math-display|true|5040>,<with|color|black|mode|math|math-display|true|40320>,<with|color|black|mode|math|math-display|true|362880>,<with|color|black|mode|math|math-display|true|3628800>,<with|color|black|mode|math|math-display|true|39916800>,479001600]
    </unfolded-io-math>
  </session>

  Last but not least, the <samp|choice> construct can also be used with
  Pure's pattern-matching <verbatim|case> expressions. Note that the closing
  <verbatim|end> of the <verbatim|case> expression is omitted, the
  <samp|choice> construct generates it automatically.

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      <around*|\||<math-it|xs>\<colons\>list|\|>=<math-bf|case>
      <math-it|xs><space|0.8spc><math-bf|of><space|0.8spc><choice|<tformat|<table|<row|<cell|<around*|[|
      |]>>|<cell|=>|<cell|0>|<cell|>>|<row|<cell|x:<math-it|xs>>|<cell|=>|<cell|1+<around*|\||<math-it|xs>|\|>>|<cell|<math-bf|otherwise>>>>>>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\||1:3\<ldots\>100|\|>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|50>
    </unfolded-io-math>
  </session>

  This defines <math|<around*|\||<math-it|xs>|\|>> (a.k.a. <verbatim|abs xs>
  in Pure notation) to compute the size of a list <em|xs> (similar to what
  Pure's <verbatim|#> operator does). For instance, let's count the number of
  primes up to 5000 (this may take a little while; alas, our definition of
  the prime sieve isn't very efficient either!):

  <\session|pure|math>
    <\input-math>
      \<gtr\>\ 
    <|input-math>
      <math-bf|let> P=sieve (2\<ldots\>\<infty\>)
      <math-bf|with><space|1spc>sieve(p:<math-it|qs>) = p :sieve[q<mid|\|>q =
      <math-it|qs>; q <math-ss|mod> p] & <math-bf|end>;
    </input-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|\||takewhile <around*|(|\<leqslant\>5000|)> P|\|>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|669>
    </unfolded-io-math>
  </session>

  <subsection|Caveats>

  <label|Caveats><TeXmacs> doesn't know anything about Pure syntax; as far as
  it is concerned, Pure's functions, operators and keywords are just text. So
  there are situations in which you may have to help the converter along by
  adding parentheses to disambiguate the parsing. This is true, in
  particular, for big operators (integrals, sums, etc., especially in
  conjunction with Pure <verbatim|with> and <verbatim|when> clauses) and
  differentials. Even an invisible bracket (shortcut: <key|( Space>) will do
  the trick. For instance:

  <\session|pure|caveats>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      using texmacs; math;
    <|unfolded-io>
      Reduce (Free CSL version), 09-Oct-12 ...

      ()
    </unfolded-io>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\>x<rsup|2>/\<mathd\>x; <text|// !! missing bracket around
      <math|x<rsup|2> >, yields <verbatim|(d x)^2> rather than <verbatim|d
      (x^2)> !!>
    <|unfolded-io-math>
      \;

      *** d declared operator\ 

      <with|color|black|mode|math|math-display|true|d<around*|(|x|)>>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\><around*|\<nobracket\>|<around*|(|x<rsup|2>|)>|\<nobracket\>>/\<mathd\>x;<text|
      // use parentheses around <math|x<rsup|2>> to disambiguate>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|2*x>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      \<mathd\><around*|\<nobracket\>|x<rsup|2>|\<nobracket\>>/\<mathd\>x;<text|
      // invisible brackets around <math|x<rsup|2>> work, too>
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|2*x>
    </unfolded-io-math>
  </session>

  Here's another snippet which produces a strange error:

  <\session|pure|caveats>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <big|sum><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>,<big|prod><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>
      <math-bf|when >n = 5<math-bf| end>;
    <|unfolded-io-math>
      <\errput>
        \<less\>stdin\<gtr\>, line 5: syntax error, unexpected '\|',
        expecting end
      </errput>
    </unfolded-io-math>
  </session>

  The tuple (comma operator) binds stronger than the <verbatim|when> clause,
  so this is valid Pure syntax. But for <TeXmacs> the
  ``<math|<around*|(|2*k-1|)> <math-bf|when >n>'' part looks like an ordinary
  term belonging under the product on the right, which is followed by an
  equals sign and another term ``<math|5<math-bf| end>>''. This makes perfect
  sense for <TeXmacs>, but it's not valid Pure syntax. This wouldn't normally
  be a problem (Pure would be able to reparse the expression correctly
  anyway), if it wasn't for the <math|<big|prod>>operator which translates to
  a Pure list comprehension. This means that the ``<math|<around*|(|2*k-1|)>
  <math-bf|when >n>'' part ends up in this list comprehension where it
  doesn't belong, hence the somewhat surprising syntax error.

  If you run into such mishaps, it is often helpful to have a look at the
  converted expression. A neat trick to do this is to just copy and paste the
  entire expression to another input line operated in verbatim mode. This
  shows us exactly what went wrong here:

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

  <\session|pure|caveats>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      <around*|(|<big|sum><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>,<big|prod><rsub|k=1\<ldots\>n><around*|(|2*k-1|)>|)>
      <math-bf|when >n = 5<math-bf| end>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|25>,<with|color|black|mode|math|math-display|true|945>
    </unfolded-io-math>
  </session>

  We can copy/paste the modified expression to a verbatim input line again,
  to confirm that it was converted correctly this time:

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