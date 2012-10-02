<TeXmacs|1.0.7.16>

<style|generic>

<\body>
  This illustrates the use of the latex.pure module contributed by Kurt
  Pagani in a <TeXmacs> Pure session. (See the <hlink|Pure installation
  guide|http://docs.pure-lang.googlecode.com/hg/install.html#texmacs-mode>
  for information on how to set up the Pure session feature in <TeXmacs>.)

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

    <\input>
      \<gtr\>\ 
    <|input>
      using latex;
    </input>

    <\input>
      \<gtr\>\ 
    <|input>
      let s = list (keys _symbols_);
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      #s;
    <|unfolded-io>
      <math|216>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      let ms = redim (11,20) (matrix (s+[1,2,3,4]));
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      ms;
    <|unfolded-io>
      <math|<around*|[|<tabular*|<tformat|<table|<row|<cell|\<alpha\>>|<cell|\<beta\>>|<cell|\<chi\>>|<cell|\<delta\>>|<cell|\<epsilon\>>|<cell|\<eta\>>|<cell|\<gamma\>>|<cell|\<iota\>>|<cell|\<kappa\>>|<cell|\<lambda\>>|<cell|\<mu\>>|<cell|\<nu\>>|<cell|\<omega\>>|<cell|\<phi\>>|<cell|\<pi\>>|<cell|\<psi\>>|<cell|\<rho\>>|<cell|\<sigma\>>|<cell|\<tau\>>|<cell|\<theta\>>>|<row|<cell|\<upsilon\>>|<cell|\<xi\>>|<cell|\<zeta\>>|<cell|\<digamma\>>|<cell|\<varepsilon\>>|<cell|\<varkappa\>>|<cell|\<varphi\>>|<cell|\<varpi\>>|<cell|\<varrho\>>|<cell|\<varsigma\>>|<cell|\<vartheta\>>|<cell|\<Delta\>>|<cell|\<Gamma\>>|<cell|\<Lambda\>>|<cell|\<Omega\>>|<cell|\<Phi\>>|<cell|\<Pi\>>|<cell|\<Psi\>>|<cell|\<Sigma\>>|<cell|\<Theta\>>>|<row|<cell|\<Upsilon\>>|<cell|\<Xi\>>|<cell|\<aleph\>>|<cell|\<beth\>>|<cell|\<daleth\>>|<cell|\<gimel\>>|<cell|\<pm\>>|<cell|\<mp\>>|<cell|\<times\>>|<cell|\<div\>>|<cell|\<cdot\>>|<cell|\<ast\>>|<cell|\<star\>>|<cell|\<dagger\>>|<cell|\<ddagger\>>|<cell|\<amalg\>>|<cell|\<cap\>>|<cell|\<cup\>>|<cell|\<uplus\>>|<cell|\<sqcap\>>>|<row|<cell|\<sqcup\>>|<cell|\<vee\>>|<cell|\<wedge\>>|<cell|\<oplus\>>|<cell|\<ominus\>>|<cell|\<otimes\>>|<cell|\<circ\>>|<cell|\<bullet\>>|<cell|\<diamond\>>|<cell|\<lhd\>>|<cell|\<rhd\>>|<cell|\<unlhd\>>|<cell|\<unrhd\>>|<cell|\<oslash\>>|<cell|\<odot\>>|<cell|\<bigcirc\>>|<cell|\<triangleleft\>>|<cell|\<Diamond\>>|<cell|<big|triangleup>>|<cell|<big|triangledown>>>|<row|<cell|\<Box\>>|<cell|\<triangleright\>>|<cell|\<setminus\>>|<cell|\<wr\>>|<cell|\<le\>>|<cell|\<ge\>>|<cell|\<neq\>>|<cell|\<sim\>>|<cell|\<ll\>>|<cell|\<gg\>>|<cell|\<doteq\>>|<cell|\<simeq\>>|<cell|\<subset\>>|<cell|\<supset\>>|<cell|\<approx\>>|<cell|\<asymp\>>|<cell|\<subseteq\>>|<cell|\<supseteq\>>|<cell|\<cong\>>|<cell|\<smile\>>>|<row|<cell|\<sqsubset\>>|<cell|\<sqsupset\>>|<cell|\<equiv\>>|<cell|\<frown\>>|<cell|\<sqsubseteq\>>|<cell|\<sqsupseteq\>>|<cell|\<propto\>>|<cell|\<in\>>|<cell|\<ni\>>|<cell|\<prec\>>|<cell|\<succ\>>|<cell|\<vdash\>>|<cell|\<dashv\>>|<cell|\<preceq\>>|<cell|\<succeq\>>|<cell|\<models\>>|<cell|\<perp\>>|<cell|\<parallel\>>|<cell|\<mid\>>|<cell|\<nmid\>>>|<row|<cell|\<nleq\>>|<cell|\<ngeq\>>|<cell|\<nsim\>>|<cell|\<ncong\>>|<cell|\<nparallel\>>|<cell|\<nless\>>|<cell|\<ngtr\>>|<cell|\<lneq\>>|<cell|\<gneq\>>|<cell|\<lnsim\>>|<cell|\<lneqq\>>|<cell|\<gneqq\>>|<cell|\<leftarrow\>>|<cell|\<to\>>|<cell|\<leftarrow\>>|<cell|\<Leftarrow\>>|<cell|\<rightarrow\>>|<cell|\<Rightarrow\>>|<cell|\<leftrightarrow\>>|<cell|\<Leftrightarrow\>>>|<row|<cell|\<mapsto\>>|<cell|\<hookleftarrow\>>|<cell|\<leftharpoonup\>>|<cell|\<leftharpoondown\>>|<cell|\<rightleftharpoons\>>|<cell|\<longleftarrow\>>|<cell|\<Longleftarrow\>>|<cell|\<longrightarrow\>>|<cell|\<Longrightarrow\>>|<cell|\<longleftrightarrow\>>|<cell|\<Longleftrightarrow\>>|<cell|\<longmapsto\>>|<cell|\<hookrightarrow\>>|<cell|\<rightharpoonup\>>|<cell|\<rightharpoondown\>>|<cell|\<leadsto\>>|<cell|\<uparrow\>>|<cell|\<Uparrow\>>|<cell|\<downarrow\>>|<cell|\<Downarrow\>>>|<row|<cell|\<updownarrow\>>|<cell|\<Updownarrow\>>|<cell|\<nearrow\>>|<cell|\<searrow\>>|<cell|\<swarrow\>>|<cell|\<nwarrow\>>|<cell|\<ldots\>>|<cell|\<vdots\>>|<cell|\<cdots\>>|<cell|\<ddots\>>|<cell|\<infty\>>|<cell|\<triangle\>>|<cell|\<angle\>>|<cell|\<aleph\>>|<cell|\<hbar\>>|<cell|\<imath\>>|<cell|\<jmath\>>|<cell|\<ell\>>|<cell|\<wp\>>|<cell|\<Re\>>>|<row|<cell|\<Im\>>|<cell|\<mho\>>|<cell|\<prime\>>|<cell|\<emptyset\>>|<cell|\<nabla\>>|<cell|\<partial\>>|<cell|\<top\>>|<cell|\<bot\>>|<cell|\<vdash\>>|<cell|\<dashv\>>|<cell|\<forall\>>|<cell|\<exists\>>|<cell|\<neg\>>|<cell|\<flat\>>|<cell|\<natural\>>|<cell|\<sharp\>>|<cell|>|<cell|\<clubsuit\>>|<cell|\<diamondsuit\>>|<cell|\<heartsuit\>>>|<row|<cell|\<spadesuit\>>|<cell|\<blacksquare\>>|<cell|¿>|<cell|<big|sum>>|<cell|<big|int>>|<cell|<big|oint>>|<cell|<big|prod>>|<cell|<big|coprod>>|<cell|<big|cap>>|<cell|<big|cup>>|<cell|<big|sqcup>>|<cell|<big|vee>>|<cell|<big|wedge>>|<cell|<big|odot>>|<cell|<big|otimes>>|<cell|<big|oplus>>|<cell|1>|<cell|2>|<cell|3>|<cell|4>>>>>|]>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      alpha + beta^gamma - Delta + aleph^rho + hbar^N;
    <|unfolded-io>
      <math|\<alpha\>+\<beta\><rsup|\<gamma\>>-\<Delta\>+\<aleph\><rsup|\<rho\>>+\<hbar\><rsup|N>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      sum/prod - nabla + forall * exists;
    <|unfolded-io>
      <math|<frac|<big|sum>|<big|prod>>-\<nabla\>+\<forall\>\<cdot\>\<exists\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      {a,b,c ; mu, nu, rho ; Phi, Psi, Sigma};
    <|unfolded-io>
      <math|<around*|[|<tabular*|<tformat|<table|<row|<cell|a>|<cell|b>|<cell|c>>|<row|<cell|\<mu\>>|<cell|\<nu\>>|<cell|\<rho\>>>|<row|<cell|\<Phi\>>|<cell|\<Psi\>>|<cell|\<Sigma\>>>>>>|]>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      {i,0,-1 ; 1,0,i ; 0,0,i} * {u,v,w};
    <|unfolded-io>
      <math|<around*|[|<tabular*|<tformat|<table|<row|<cell|i>|<cell|0>|<cell|-1>>|<row|<cell|1>|<cell|0>|<cell|i>>|<row|<cell|0>|<cell|0>|<cell|i>>>>>|]>\<cdot\><around*|[|<tabular*|<tformat|<table|<row|<cell|u>|<cell|v>|<cell|w>>>>>|]>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      e^(i*z+alpha);
    <|unfolded-io>
      <math|e<rsup|i\<cdot\>z+\<alpha\>>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      (x^n - z)/(x-z);
    <|unfolded-io>
      <math|<frac|x<rsup|n>-z|x-z>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      Integral Omega (ExtD omega);
    <|unfolded-io>
      <math|<big|int><rsub|\<Omega\>><space|0.25spc><text|<with|font-family|tt|d>>\<omega\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      Integral (Bdry Omega) omega;
    <|unfolded-io>
      <math|<big|int><rsub|\<partial\>*\<Omega\>><space|0.25spc>\<omega\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      Diff f x;
    <|unfolded-io>
      <math|<frac|\<partial\>*f|\<partial\>*x>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      Lambda!i;
    <|unfolded-io>
      <math|\<Lambda\><rsub|i>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      F!!(1..10);
    <|unfolded-io>
      <math|F<rsub|1>:F<rsub|2>:F<rsub|3>:F<rsub|4>:F<rsub|5>:F<rsub|6>:F<rsub|7>:F<rsub|8>:F<rsub|9>:F<rsub|10>:<around|[||]>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      q ~= -q;
    <|unfolded-io>
      <math|q\<neq\>-q>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      a && b \|\| c;
    <|unfolded-io>
      <math|a\<wedge\>b\<vee\>c>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      ~T;
    <|unfolded-io>
      <math|\<urcorner\>*T>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      neg T;
    <|unfolded-io>
      <math|-T>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      hbar^N/(2*m*c);
    <|unfolded-io>
      <math|<frac|\<hbar\><rsup|N>|<around|(|2\<cdot\>m|)>\<cdot\>c>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      forall:x:in:T:exists:y:in:S;
    <|unfolded-io>
      <math|\<forall\>:x:\<in\>:T:\<exists\>:y:\<in\>:S>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      Re ~= Im;
    <|unfolded-io>
      <math|\<Re\>\<neq\>\<Im\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      A:cap:cup:B;
    <|unfolded-io>
      <math|A:\<cap\>:\<cup\>:B>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      Integral D (Diff F u);
    <|unfolded-io>
      <math|<big|int><rsub|D><space|0.25spc><frac|\<partial\>*F|\<partial\>*u>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      infty == inf;
    <|unfolded-io>
      <math|\<infty\>=i*n*f>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      F x y z;
    <|unfolded-io>
      <with|font-family|tt|F x y z>
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      // function form =
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      cdots;
    <|unfolded-io>
      <math|\<cdots\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      ldots;
    <|unfolded-io>
      <math|\<ldots\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      longleftarrow;
    <|unfolded-io>
      <math|\<longleftarrow\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      longrightarrow;
    <|unfolded-io>
      <math|\<longrightarrow\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      Box;
    <|unfolded-io>
      <math|\<Box\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      sharp;
    <|unfolded-io>
      <math|\<sharp\>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      X!sharp;
    <|unfolded-io>
      <math|X<rsub|\<sharp\>>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      Q^sharp;
    <|unfolded-io>
      <math|Q<rsup|\<sharp\>>>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      quit
    <|unfolded-io>
      <script-dead>
    </unfolded-io>
  </session>
</body>