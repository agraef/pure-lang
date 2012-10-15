<TeXmacs|1.0.7.16>

<style|<tuple|generic|varsession>>

<\body>
  <doc-data|<doc-title|Pure <TeXmacs> Examples>>

  <section|Pure Sessions>

  Some (inactive) program code (<verbatim|verbatim>):

  <\verbatim-code>
    fact n = if n\<gtr\>0 then n*fact (n-1) else 1;
  </verbatim-code>

  A Pure session:

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

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  You can insert a Pure session with <samp|Insert \| Session \| Pure>. These
  snippets are <em|real interactions> with the Pure interpreter, so you can
  rerun the calculations or enter your own code. By these means you can use
  <TeXmacs> as a frontend for the Pure interpreter; please check the
  <TeXmacs> documentation, section ``<TeXmacs> \ as an interface'', for
  details. To make this work, you'll have to install the accompanying
  <verbatim|init-pure.scm> and <verbatim|pure-script-input.scm> files in your
  <verbatim|~/.TeXmacs/plugins/pure/progs> folder, so that <TeXmacs> knows
  about the Pure session. (The distributed file actually defines various
  different types of Pure sessions, each with their own options for the Pure
  interpreter, and it's easy to add your own if needed.)

  Sessions can be formatted in different ways. Here we use the <TeXmacs>
  <samp|varsession> style package for a somewhat fancier formatting. It's
  also possible to globally override formatting options such as the color of
  prompts, input and output fields, by defining the <verbatim|pure-input> and
  <verbatim|pure-output> macros accordingly; see the <TeXmacs> manual,
  section ``Writing <TeXmacs> style files'', for details. An example can be
  found in the accompanying <verbatim|pure-session-styles.ts> file; install
  this in your <verbatim|~/.TeXmacs/packages> directory if you want to give
  it a try.

  Here's another session showing plain text formatting and subsessions.

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
        \<less\>stdin\<gtr\>, line 2: unhandled exception 'failed_cond' while
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

    <\input>
      \<gtr\>\ 
    <|input>
      \;
    </input>
  </session>

  <section|Pure and Reduce>

  The following example shows how to run the Reduce computer algebra system
  in Pure (to make this work, you also need to have the
  <verbatim|pure-reduce> module installed; this is available as an addon from
  the <hlink|Pure website|http://pure-lang.googlecode.com>).

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

  Math input also works, as shown below. For convenience, you can toggle the
  input line between math and program (verbatim) mode using the
  <verbatim|Ctrl+$> key combination. At present this is still a bit
  experimental, and output from Pure is in text format only. Making both math
  input and output work transparently and flawlessly is being worked on,
  however, so stay tuned.

  <\session|pure|default>
    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|df <around*|(|sin<around*|(|x<rsup|2>|)>|)> x|)>;
    <|unfolded-io-math>
      2*cos (x^2)*x
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|intg <around*|(|cos <around*|(|x+y|)><rsup|2>|)>
      x|)>;
    <|unfolded-io-math>
      (cos (x+y)*sin (x+y)+x)/2
    </unfolded-io-math>

    <\input-math>
      \<gtr\>\ 
    <|input-math>
      \;
    </input-math>
  </session>

  <section|Pure Scripting>

  Last but not least, Pure can also be used as a <em|scripting language> in
  <TeXmacs>, as described at the end of the ``<TeXmacs> \ as an interface''
  section. This is done by enabling the <samp|Document \| Scripts \| Pure>
  option. Scripting uses its own instance of the Pure interpreter which is
  separate from all Pure sessions that might be included in the same
  document. It gives you both executable input fields and spreadsheets whose
  cells can be evaluated in the Pure interpreter. For instance:

  <\bothlined>
    Enter <math|x> here: <em|<calc-inert|x|99>>. This is the value of
    <math|x> squared: <em|<calc-output|x squared|<calc-ref|x>^2|9801.0>>.
  </bothlined>

  Position the cursor in the right field above and hit Return to reveal the
  underlying Pure expression, and Return again to recompute it. You can also
  change the value in the left field and hit Return to have the right field
  recomputed.

  An even more convenient way to have Pure compute values in a <TeXmacs>
  document are spreadsheets. Here is an example of a textual spreadsheet
  (<samp|Insert \| Table \| Textual spreadsheet>) showing some Pure and
  Reduce calculations (hit Return in the cells of the last column to reveal
  the underlying Pure formulas, and Return once again to recompute them).

  <\calc-table|table1>
    <textual-table|<tformat|<cwith|1|1|1|-1|cell-background|pastel
    yellow>|<cwith|2|-1|1|-1|cell-hyphen|n>|<cwith|2|2|1|-1|cell-bborder|0>|<cwith|1|-1|1|1|cell-width|>|<cwith|1|-1|1|1|cell-hmode|auto>|<cwith|4|4|1|-1|cell-bborder|0>|<cwith|1|-1|1|1|cell-background|pastel
    yellow>|<table|<row|<cell|<cell-inert|a1|a1>>|<cell|<cell-inert|b1|b>>|<cell|<cell-inert|c1|c>>|<cell|<cell-inert|d1|d>>>|<row|<cell|<cell-inert|a2|2>>|<cell|<cell-inert|b2|1>>|<cell|<cell-inert|c2|12>>|<cell|<cell-output|d2|=
    fact (b2+c2) with fact n = if n\<gtr\>0 then n*fact(n-1) else 1
    end|1932053504>>>|<row|<cell|<cell-inert|a3|3>>|<cell|<cell-inert|b3|17>>|<cell|<cell-inert|c3|33>>|<cell|<cell-output|d3|='(b3+c3)|17+33>>>|<row|<cell|<cell-inert|a4|4>>|<cell|<cell-inert|b4|<math|sin
    <around*|(|x<rsup|2>|)>>>>|<cell|<cell-inert|c4|<math|x>>>|<cell|<cell-output|d4|=?df
    b4 c4|2*cos (x^2)*x>>>|<row|<cell|<cell-inert|a5|5>>|<cell|<cell-inert|b5|<math|<around*|(|x+y|)><rsup|3>>>>|<cell|<cell-inert|c5|<math|x>>>|<cell|<cell-output|d5|=?df
    b5 c5|3*x^2+6*x*y+3*y^2>>>>>>

    \;
  </calc-table>

  You can also refer to a table field in text like this:
  <calc-output|field3|<calc-ref|table1-d4>|2*cos (x^2)*x>. To these ends, we
  set the Ref field of the table to <verbatim|table1>. The reference to cell
  <verbatim|d4> can then be entered with the following key sequence:
  <verbatim|\\! \\? table1-d4 Return>.
</body>

<\initial>
  <\collection>
    <associate|font-base-size|10>
    <associate|language|american>
    <associate|page-type|a4>
    <associate|par-hyphen|normal>
    <associate|preamble|false>
    <associate|prog-scripts|pure-script>
    <associate|sfactor|4>
    <associate|src-style|angular>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-2|<tuple|2|3>>
    <associate|auto-3|<tuple|3|4>>
    <associate|footnote-1|<tuple|1|1>>
    <associate|footnr-1|<tuple|1|1>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Pure
      Sessions> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Pure
      and Reduce> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Pure
      Scripting> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>