<TeXmacs|1.0.7.16>

<style|<tuple|tmdoc|varsession>>

<\body>
  <screens|<\shown>
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
    accompanying <verbatim|init-pure.scm>, <verbatim|pure-script-input.scm>
    and <verbatim|texmacs.pure> files in your
    <verbatim|~/.TeXmacs/plugins/pure/progs> folder (create this directory if
    needed). The distributed configuration actually defines various different
    types of Pure sessions, each with their own options for the Pure
    interpreter, and it's easy to add your own if needed.

    Sessions can be formatted in different ways. Here we use the <TeXmacs>
    <samp|varsession> style package for a somewhat fancier formatting.

    <\bothlined>
      <strong|Note: >This will not actually be visible if you're reading this
      in the <TeXmacs> help browser which uses its own style options. In this
      case you'll have to choose the <samp|varsession> style manually, see
      <samp|Document \| Add package \| Program \| varsession> in the menu.)
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
          \<less\>stdin\<gtr\>, line 4: unhandled exception 'failed_cond'
          while evaluating 'fact fact'
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

    <section|Pure and Reduce>

    The following example shows how to run the Reduce computer algebra system
    in Pure (to make this work, you also need to have the
    <verbatim|pure-reduce> module installed; this is available as an addon
    from the <hlink|Pure website|http://pure-lang.googlecode.com>).

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

    Math input also works, as shown below. For convenience, you can toggle
    the input line between math and program (verbatim) mode using the
    <key|Ctrl+$> key combination. At present this is still a bit
    experimental, and output from Pure is in text format only. Making both
    math input and output work transparently and flawlessly is being worked
    on, however, so stay tuned.

    <\session|pure|reduce>
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
    option.

    <\bothlined>
      <strong|Note: >If you're reading this in the <TeXmacs> help browser,
      then Pure scripting will most likely not be enabled yet, so you should
      do this now if you'd like to try the examples below.
    </bothlined>

    Scripting uses its own instance of the Pure interpreter which is separate
    from all Pure sessions that might be included in the same document. It
    gives you both executable input fields and spreadsheets whose cells can
    be evaluated in the Pure interpreter. For instance:

    Enter <math|x> here: <em|<calc-inert|x|99>>. This is the value of
    <math|x> squared: <em|<calc-output|x squared|<calc-ref|x>^2|9801.0>>.

    Position the cursor in the right field above and hit <key|Return> twice
    to toggle between the computed Pure expression and its value. You can
    also change the value in the left field and hit <key|Return> to have the
    right field recomputed.

    An even more convenient way to have Pure compute values in a <TeXmacs>
    document are spreadsheets. Here is an example of a textual spreadsheet
    (<samp|Insert \| Table \| Textual spreadsheet>) showing some Pure and
    Reduce calculations. As before, you can type <key|Return> in the cells of
    the last column to reveal the underlying Pure formulas; also try changing
    some of the values in the <verbatim|b> and <verbatim|c> columns and
    hitting <key|Return> to recompute the corresponding values in the last
    column.

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
    <calc-output|field3|<calc-ref|table1-d4>|2*cos (x^2)*x>. Here we set the
    Ref field of the table to <verbatim|table1>; the reference to cell
    <verbatim|d4> can then be entered with the following series of key
    strokes: <key|\\ ! \\ ?> <verbatim|table1-d4> <key|Return>.
  </shown>>
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
  </collection>
</initial>