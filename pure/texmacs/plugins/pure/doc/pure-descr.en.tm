<TeXmacs|1.0.7.16>

<style|<tuple|tmdoc|varsession>>

<\body>
  <section|Pure Sessions>

  You can insert a Pure session as usual with <samp|Insert \| Session \|
  Pure>.

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
      fact n = if n\<gtr\>0 then n*fact (n-1) else 1;
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      map fact (0..10);
    <|unfolded-io>
      [1,1,2,6,24,120,720,5040,40320,362880,3628800]
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      show fact
    <|unfolded-io>
      fact n = if n\<gtr\>0 then n*fact (n-1) else 1;
    </unfolded-io>
  </session>

  As with other <TeXmacs> session inserts, these snippets aren't just
  verbatim Pure code, they are <em|real interactions> with the Pure
  interpreter, so you can rerun the calculations or enter your own code. By
  these means you can use <TeXmacs> as a frontend for the Pure interpreter;
  please check the <TeXmacs> documentation, section ``<TeXmacs> \ as an
  interface'', for details. To make this work, you'll have to install the
  plugin first so that <TeXmacs> knows about it; instructions for that can be
  found in the <hlink|Pure installation instructions|http://docs.pure-lang.googlecode.com/hg/install.html#texmacs-mode>.
  The distributed configuration actually defines various different types of
  Pure sessions, each with their own options for the Pure interpreter, and
  it's easy to add your own if needed.

  Sessions can be formatted in different ways. Here we use the <TeXmacs>
  <samp|varsession> style package for a somewhat fancier formatting.

  <\bothlined>
    <strong|Note: >This will not actually be visible if you're reading this
    in the <TeXmacs> help browser which uses its own style options. In this
    case you'll have to choose the <samp|varsession> style manually, see
    <samp|Document \| Add package \| Program \| varsession> in the menu.
  </bothlined>

  It's also possible to globally override formatting options such as the
  color of prompts, input and output fields, by defining the
  <verbatim|pure-input> and <verbatim|pure-output> macros accordingly; see
  the <TeXmacs> manual, section ``Writing <TeXmacs> style files'', for
  details. An example can be found in the accompanying
  <verbatim|pure-session-styles.ts> file; install this in your
  <verbatim|~/.TeXmacs/packages> directory if you want to give it a try.

  Here's another session showing plain text formatting and subsessions.

  <\session|pure|subsession-example>
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
      fact n = if n\<gtr\>0 then n*fact (n-1) else 1;
    </input>

    <\unfolded>
      This is a subsession.
    <|unfolded>
      <\unfolded-io>
        \<gtr\>\ 
      <|unfolded-io>
        map fact (1..10); // <text|This is a <em|plain text> comment with
        math: <math|n! = 1\<times\>\<cdots\>\<times\><around*|(|n-1|)>\<times\>n>.>
      <|unfolded-io>
        [1,2,6,24,120,720,5040,40320,362880,3628800]
      </unfolded-io>

      <\unfolded-io>
        \<gtr\>\ 
      <|unfolded-io>
        show fact
      <|unfolded-io>
        fact n = if n\<gtr\>0 then n*fact (n-1) else 1;
      </unfolded-io>

      <\input>
        \<gtr\>\ 
      <|input>
        \;
      </input>
    </unfolded>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      fact fact;
    <|unfolded-io>
      <\errput>
        \<less\>stdin\<gtr\>, line 4: unhandled exception 'failed_cond' while
        evaluating 'fact fact'
      </errput>
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      fact 30L;
    <|unfolded-io>
      265252859812191058636308480000000L
    </unfolded-io>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      quit
    <|unfolded-io>
      <script-dead>
    </unfolded-io>
  </session>

  Another session type (<verbatim|pure-debug>, this runs the interpreter in
  debugging mode):

  <\session|pure-debug|default>
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
      fact n = if n\<gtr\>0 then n*fact (n-1) else 1;
    </input>

    <\input>
      \<gtr\>\ 
    <|input>
      break fact
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      fact 5;
    <|unfolded-io>
      ** [1] fact: fact n = if n\<gtr\>0 then n*fact (n-1) else 1;

      \ \ \ \ \ n = 5

      (Type 'h' for help.)
    </unfolded-io>

    <\unfolded-io>
      :\ 
    <|unfolded-io>
      h
    <|unfolded-io>
      Debugger commands:

      a \ \ \ \ \ \ auto: step through the entire program, run unattended

      c [f] \ \ continue until next breakpoint, or given function f

      h \ \ \ \ \ \ help: print this list

      n \ \ \ \ \ \ next step: step over reduction

      p [n] \ \ print rule stack (n = number of frames)

      r \ \ \ \ \ \ run: finish evaluation without debugger

      s \ \ \ \ \ \ single step: step into reduction

      t, b \ \ \ move to the top or bottom of the rule stack

      u, d \ \ \ move up or down one level in the rule stack

      x \ \ \ \ \ \ exit the interpreter (after confirmation)

      . \ \ \ \ \ \ reprint current rule

      ! cmd \ \ execute interpreter command

      ? expr \ evaluate expression

      \<less\>cr\<gtr\> \ \ \ single step (same as 's')

      \<less\>eof\<gtr\> \ \ step through program, run unattended (same as
      'a')
    </unfolded-io>

    <\unfolded-io>
      :\ 
    <|unfolded-io>
      s
    <|unfolded-io>
      ** [2] (\<gtr\>): x::int\<gtr\>y::int = x\<gtr\>y;

      \ \ \ \ \ x = 5; y = 0
    </unfolded-io>

    <\unfolded-io>
      :\ 
    <|unfolded-io>
      s
    <|unfolded-io>
      ++ [2] (\<gtr\>): x::int\<gtr\>y::int = x\<gtr\>y;

      \ \ \ \ \ x = 5; y = 0

      \ \ \ \ \ --\<gtr\> 1

      ** [2] (-): x::int-y::int = x-y;

      \ \ \ \ \ x = 5; y = 1
    </unfolded-io>

    <\unfolded-io>
      :\ 
    <|unfolded-io>
      s
    <|unfolded-io>
      ++ [2] (-): x::int-y::int = x-y;

      \ \ \ \ \ x = 5; y = 1

      \ \ \ \ \ --\<gtr\> 4

      ** [2] fact: fact n = if n\<gtr\>0 then n*fact (n-1) else 1;

      \ \ \ \ \ n = 4
    </unfolded-io>

    <\unfolded-io>
      :\ 
    <|unfolded-io>
      r
    <|unfolded-io>
      120
    </unfolded-io>
  </session>

  <section|Completion and Help>

  Completion of Pure keywords and functions is fully supported in <TeXmacs>.
  Just type <key|Tab> as usual and <TeXmacs> displays a list of possible
  completions in its status line. Pressing <key|Tab> again you can cycle
  through the completions and pick the one that you want. For instance, you
  can try this yourself on the following input line by placing the cursor
  behind the <verbatim|f> and hitting the <key|Tab> key:

  <\session|pure|default>
    <\input>
      \<gtr\>\ 
    <|input>
      f
    </input>
  </session>

  The Pure <verbatim|help> command also works in <TeXmacs>. By default, this
  will pop up a new <TeXmacs> window with the help file in it. Search terms
  also work as usual; you might want to try the following to find out how the
  Pure help system works (this may take a while to load, so be patient):

  <\session|pure|default>
    <\input>
      \<gtr\>\ 
    <|input>
      help online-help
    </input>
  </session>

  Note that Pure's online help is in html format. While <TeXmacs> can load
  html files, it has to convert them to its own format first, which at least
  in the current version of <TeXmacs> is quite slow and the rendering isn't
  perfect. Hopefully this will improve in future <TeXmacs> versions, but for
  the time being you may prefer to use an external graphical html browser
  instead. If the <verbatim|PURE_HELP> shell environment variable is set, the
  interpreter will use that command to display the online help.<\footnote>
    Note that the <verbatim|PURE_HELP> environment variable also works
    outside of <TeXmacs>, i.e., when the interpreter is run from Emacs or the
    shell. The <verbatim|BROWSER> environment variable, however, does
    <em|not> change the way that the <verbatim|help> command works in
    <TeXmacs>, so you can use this variable instead to specify a browser
    program for use outside of <TeXmacs> only.
  </footnote>

  <section|Pure and Reduce>

  The following example shows how to run the Reduce computer algebra system
  in Pure (to make this work, you also need to have the
  <verbatim|pure-reduce> module installed; this is available as an addon from
  the <hlink|Pure website|http://pure-lang.googlecode.com>).

  <\session|pure|reduce>
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
      using reduce;
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

  Up to now we've only been running Pure in verbatim
  a.k.a.<space|1spc>program mode. But the Pure plugin also fully supports
  math input and output. These are enabled as follows:

  <\itemize>
    <item>To use math <em|input>, you can toggle the input line between math
    and program (verbatim) mode using the <key|Ctrl+$> key combination. This
    isn't a standard <TeXmacs> keybinding, but is defined at the beginning of
    the <verbatim|pure-init.scm> script for your convenience; you can edit
    the script to change this according to your preferences. Of course, you
    can also use the corresponding <samp|Focus \| Input options \|
    Mathematical input> menu option or the equivalent toolbar item; these
    become visible when the cursor is located at the input line. Or you can
    make math input the default when the Pure <TeXmacs> plugin is loaded by
    putting the following Scheme command into your
    <verbatim|my-init-texmacs.scm> file:

    <\scm-code>
      (if (not (session-math-input?)) (toggle-session-math-input))
    </scm-code>

    <item>To enable math <em|output>, you'll need to import the Pure
    <verbatim|texmacs> module. The <verbatim|math> function in this module
    switches output to <LaTeX> math, as provided by the Reduce
    <verbatim|tmprint> package (thus this feature really needs the
    <verbatim|pure-reduce> module). The <verbatim|verbatim> function switches
    back to verbatim Pure output. Verbatim output is also used as a fallback
    in math mode for all Pure expressions which cannot be printed through the
    Reduce interface (typically because they aren't valid Reduce
    expressions).

    You can make math output the default by enabling the <verbatim|tmmath>
    conditional compilation option on the Pure side. This can be set
    interactively by entering the following pragma at the input line
    <em|before> loading the <verbatim|texmacs> module:

    <\verbatim-code>
      #! --enable tmmath
    </verbatim-code>

    Alternatively, you can also set the <verbatim|PURE_OPTION_TMMATH>
    variable in your process environment or you shell's startup files to
    enable this option.
  </itemize>

  At present this is still a bit experimental and work in progress, but it
  seems to work pretty well already, as shown below. (If you notice any bugs
  or missing features in math input and output, please submit a bug report on
  the <hlink|Pure website|http://code.google.com/p/pure-lang/issues/list>.)

  <\session|pure|reduce>
    <\input>
      \<gtr\>\ 
    <|input>
      #! --enable tmmath
    </input>

    <\input>
      \<gtr\>\ 
    <|input>
      using texmacs;
    </input>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|df <around*|(|sin<around*|(|x<rsup|2>|)>|)> x|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|2*cos<around*|(|x<rsup|2>|)>*x>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|intg <around*|(|cos <around*|(|x+y|)><rsup|2>|)>
      x|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|cos<around*|(|x+y|)>*sin<around*|(|x+y|)>+x|2>>
    </unfolded-io-math>
  </session>

  <section|Pure Scripting>

  Last but not least, Pure can also be used as a <em|scripting language> in
  <TeXmacs>, as described at the end of the ``<TeXmacs> \ as an interface''
  section. This is done by enabling the <samp|Document \| Scripts \| Pure>
  option (or <samp|Document \| Scripts \| Pure-math> if you prefer math
  output; this is what we use here).

  <\bothlined>
    <strong|Note: >If you're reading this in the <TeXmacs> help browser, then
    Pure scripting will most likely not be enabled yet, so you should do this
    now if you'd like to try the examples below.
  </bothlined>

  Scripting uses its own instance of the Pure interpreter which is separate
  from all Pure sessions that might be included in the same document. It
  gives you both executable input fields and spreadsheets whose cells can be
  evaluated in the Pure interpreter. For instance:

  Enter <math|x> here: <with|font-base-size|14|<calc-inert|x|a+b>>. This is
  the value of <math|x> squared: <with|font-base-size|14|<calc-output|x
  squared|<calc-ref|x>^2|<with|color|black|mode|math|math-display|true|a<rsup|2>+2*a*b+b<rsup|2>>>>.

  Position the cursor in the right field above and hit <key|Return> twice to
  toggle between the computed Pure expression and its value. You can also
  change the value in the left field and hit <key|Return> to have the right
  field recomputed.

  An even more convenient way to have Pure compute values in a <TeXmacs>
  document are spreadsheets. Here is an example of a textual spreadsheet
  (<samp|Insert \| Table \| Textual spreadsheet>) showing some Pure and
  Reduce calculations. As before, you can type <key|Return> in the cells of
  the last column to reveal the underlying Pure formulas; also try changing
  some of the values in the <verbatim|b> and <verbatim|c> columns and hitting
  <key|Return> to recompute the corresponding values in the last column.

  <with|font-base-size|12|<\calc-table|table1>
    <textual-table|<tformat|<cwith|1|1|1|-1|cell-background|pastel
    yellow>|<cwith|2|-1|1|-1|cell-hyphen|n>|<cwith|2|2|1|-1|cell-bborder|0>|<cwith|1|-1|1|1|cell-width|>|<cwith|1|-1|1|1|cell-hmode|auto>|<cwith|4|4|1|-1|cell-bborder|0>|<cwith|1|-1|1|1|cell-background|pastel
    yellow>|<cwith|2|-1|4|4|cell-background|pastel
    green>|<table|<row|<cell|<cell-inert|a1|a1>>|<cell|<cell-inert|b1|b>>|<cell|<cell-inert|c1|c>>|<cell|<cell-inert|d1|d>>>|<row|<cell|<cell-inert|a2|2>>|<cell|<cell-inert|b2|1>>|<cell|<cell-inert|c2|12>>|<cell|<cell-output|d2|=
    fact (b2+c2) with fact n = if n\<gtr\>0 then n*fact(n-1) else 1
    end|1932053504>>>|<row|<cell|<cell-inert|a3|3>>|<cell|<cell-inert|b3|17>>|<cell|<cell-inert|c3|33>>|<cell|<cell-output|d3|='(b3+c3)|<with|color|black|mode|math|math-display|true|50>>>>|<row|<cell|<cell-inert|a4|4>>|<cell|<cell-inert|b4|<math|sin
    <around*|(|x<rsup|2>|)>>>>|<cell|<cell-inert|c4|<math|x>>>|<cell|<cell-output|d4|=?df
    b4 c4|<with|color|black|mode|math|math-display|true|2*cos<around*|(|x<rsup|2>|)>*x>>>>|<row|<cell|<cell-inert|a5|5>>|<cell|<cell-inert|b5|<math|<around*|(|x+y|)><rsup|2>>>>|<cell|<cell-inert|c5|<math|x>>>|<cell|<cell-output|d5|=?df
    b5 c5|<with|color|black|mode|math|math-display|true|2*<around*|(|x+y|)>>>>>>>>

    \;
  </calc-table>>

  You can also refer to a table field in text like this:
  <with|font-base-size|12|<calc-output|field3|<calc-ref|table1-d4>|<with|color|black|mode|math|math-display|true|2*cos<around*|(|x<rsup|2>|)>*x>>>.
  Here we set the Ref field of the table to <verbatim|table1>; the reference
  to cell <verbatim|d4> can then be entered with the following series of key
  strokes: <key|\\ ! \\ ?> <verbatim|table1-d4> <key|Return>.
</body>

<\initial>
  <\collection>
    <associate|font-base-size|10>
    <associate|language|american>
    <associate|page-type|a4>
    <associate|par-hyphen|normal>
    <associate|preamble|false>
    <associate|prog-scripts|pure-script-math>
    <associate|sfactor|4>
  </collection>
</initial>