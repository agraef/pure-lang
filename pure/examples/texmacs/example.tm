<TeXmacs|1.0.7.16>

<style|<tuple|generic|varsession>>

<\body>
  <doc-data|<doc-title|Pure <TeXmacs> Examples>>

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
  <verbatim|init-pure.scm> file in your <verbatim|~/.TeXmacs/plugins/pure/progs>
  folder, so that <TeXmacs> knows about the Pure session. (The distributed
  file actually defines various different types of Pure sessions, each with
  their own options for the Pure interpreter, and it's easy to add your own
  if needed.)

  Also note that sessions can be formatted in different ways. Here we use the
  <TeXmacs> <samp|varsession> style package for a somewhat fancier
  formatting. It's also possible to globally override formatting options such
  as the color of prompts, input and output fields, by defining the
  <verbatim|pure-input> and <verbatim|pure-output> macros accordingly; see
  the <TeXmacs> manual, section ``Writing <TeXmacs> style files'', for
  details. An example can be found in the accompanying
  <verbatim|pure-session-styles.ts> file; install this in your
  <verbatim|~/.TeXmacs/packages> directory if you want to give it a try.

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
      <\text>
        This is normal text.
      </text>
    </input>

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
        [fact 1,fact 2,fact 3,fact 4,fact 5,fact 6,fact 7,fact 8,fact 9,fact
        10]
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

  \;
</body>

<\initial>
  <\collection>
    <associate|font-base-size|10>
    <associate|language|american>
    <associate|page-type|a4>
    <associate|par-hyphen|normal>
    <associate|preamble|false>
    <associate|sfactor|4>
    <associate|src-style|angular>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|footnote-1|<tuple|1|1>>
    <associate|footnr-1|<tuple|1|1>>
  </collection>
</references>