<TeXmacs|1.0.7.17>

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
  found in the <hlink|Pure installation instructions|http://purelang.bitbucket.org/docs/install.html#texmacs-mode>.
  The distributed configuration actually defines various different types of
  Pure sessions, each with their own options for the Pure interpreter, and
  it's easy to add your own if needed.

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

  Note that <TeXmacs> always runs plugins in the current working directory in
  which it was started, which is also the directory from which the Pure
  interpreter will read its startup files. If you start <TeXmacs> from the
  GUI, this will most likely be your home directory. It often makes more
  sense to run the interpreter in the directory of the <TeXmacs> document
  hosting the Pure sessions, so that you can keep scripts and other files
  needed by your sessions in the same directory. Unfortunately, <TeXmacs>
  offers no help with this; there's simply no way to specify a working
  directory when running a session. Thus you'll either have to run <TeXmacs>
  in the right directory (by invoking it from the command line), or change
  the working directory manually inside the Pure session. To help with the
  latter, the <TeXmacs>-hosted interpreter offers a special <verbatim|cdd>
  (``current document directory'') command:

  <\session|pure|default>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      pwd
    <|unfolded-io>
      /home/ag
    </unfolded-io>

    <\input>
      \<gtr\>\ 
    <|input>
      cdd
    </input>

    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      pwd
    <|unfolded-io>
      /home/ag/.TeXmacs/plugins/pure/doc
    </unfolded-io>
  </session>

  <section|Completion and Help>

  Completion of Pure keywords and functions is fully supported in <TeXmacs>.
  Just type <key|Tab> as usual and <TeXmacs> displays a list of possible
  completions in its status line. Pressing <key|Tab> again you can cycle
  through the completions and pick the one that you want. For instance, you
  can try this yourself on the following input line by placing the cursor
  behind the <verbatim|f> and hitting the <key|Tab> key repeatedly:

  <\session|pure|default>
    <\input>
      \<gtr\>\ 
    <|input>
      f
    </input>
  </session>

  The Pure <verbatim|help> command also works in <TeXmacs>. This pops up a
  new <TeXmacs> window with the help file in it. Search terms also work as
  usual; you might want to try the following to find out how the Pure help
  system works (this may take a while to load, so be patient):

  <\session|pure|default>
    <\input>
      \<gtr\>\ 
    <|input>
      help online-help
    </input>
  </session>

  Note that Pure's online help is in html format by default. While <TeXmacs>
  can load html files, it has to convert them to its own format first, which
  at least in the current version of <TeXmacs> is quite slow and the
  rendering isn't perfect. As a remedy, there's <TeXmacs>-formatted online
  documentation available on the Pure website, see the Pure installation
  instructions for details. If these files are installed then the Pure help
  can also be accessed using the corresponding options in the <samp|Pure>
  plugin menu which automatically appears when the cursor is inside a Pure
  session. If you still prefer the html documentation, then it's also
  possible to use an external graphical html browser instead. Just set the
  <verbatim|PURE_HELP> shell environment variable accordingly, the
  interpreter will then use that command to display the online
  help.<\footnote>
    Note that the <verbatim|PURE_HELP> environment variable also works
    outside of <TeXmacs>, i.e., when the interpreter is run from Emacs or the
    shell. The <verbatim|BROWSER> environment variable, however, does
    <em|not> change the way that the <verbatim|help> command works in
    <TeXmacs>, so you can use this variable instead to specify a browser
    program for use outside of <TeXmacs> only.
  </footnote>

  <section|Math Input and Output>

  The Pure plugin fully supports math input and output. These are enabled as
  follows:

  <\itemize>
    <item>To use math <em|input>, you can toggle the input line between math
    and program (verbatim) mode using the <key|Ctrl+$> key combination. This
    isn't a standard <TeXmacs> keybinding, but is defined at the beginning of
    the <verbatim|pure-init.scm> script for your convenience; you can edit
    the script to change this according to your preferences. Of course, you
    can also use the corresponding <samp|Focus \| Input options \|
    Mathematical input> menu option or the equivalent toolbar item; these
    become visible when the cursor is located at the input line. Or you can
    make math input the default by putting the following Scheme command into
    your <verbatim|my-init-texmacs.scm> file:

    <\scm-code>
      (if (not (session-math-input?)) (toggle-session-math-input))
    </scm-code>

    <item>To enable math <em|output>, you'll have to invoke the
    <verbatim|math> function from the Pure <verbatim|texmacs> module. This
    module is always loaded when running Pure inside <TeXmacs>. The
    <verbatim|math> function uses the Reduce <verbatim|tmprint> package, so
    the <verbatim|pure-reduce> module <em|must> be installed to make this
    work.

    The <verbatim|verbatim> function switches back to verbatim Pure output.
    Verbatim output is also used as a fallback in math mode for all Pure
    expressions which cannot be printed through the Reduce interface
    (typically because they aren't valid Reduce expressions).

    To make math output the default, you can also run a <verbatim|pure-math>
    session (<samp|Insert \| Session \| Pure-math>) which works like a
    regular Pure session but enables math output and loads the
    <verbatim|reduce> module at startup.
  </itemize>

  Here's a little example, running the Reduce computer algebra system in
  Pure:

  <\session|pure|reduce>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      math;
    <|unfolded-io>
      Reduce (Free CSL version), 03-Nov-12 ...

      ()
    </unfolded-io>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|df <around*|(|sin<around*|(|x<rsup|2>|)>|)> x|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|2*cos
      <around*|(|x<rsup|2>|)>*x>
    </unfolded-io-math>

    <\unfolded-io-math>
      \<gtr\>\ 
    <|unfolded-io-math>
      simplify <around*|(|intg <around*|(|cos <around*|(|x+y|)><rsup|2>|)>
      x|)>;
    <|unfolded-io-math>
      <with|color|black|mode|math|math-display|true|<frac|cos
      <around*|(|x+y|)>*sin <around*|(|x+y|)>+x|2>>
    </unfolded-io-math>
  </session>

  It's useful to remember the following key bindings and special constructs
  in math input mode:

  <\itemize>
    <item>To enter a Pure lambda you need to type the backslash. The <key|\\>
    key will enter a special <TeXmacs> command, but you can escape the
    backslash character as follows: <key|Shift+F5 \\>. For instance (note
    that to type the right arrow, you just enter <key|- \<gtr\>>; likewise,
    the <key|..> operator shows as a ``<math|\<ldots\>>'' symbol in math
    input):

    <\session|pure|reduce>
      <\unfolded-io-math>
        \<gtr\>\ 
      <|unfolded-io-math>
        \\x\<rightarrow\>2*x+1;
      <|unfolded-io-math>
        #\<less\>closure 0x7f13e5b3f9f0\<gtr\>
      </unfolded-io-math>

      <\unfolded-io-math>
        \<gtr\>\ 
      <|unfolded-io-math>
        map ans <around*|(|1\<ldots\>10|)>;
      <|unfolded-io-math>
        <with|color|black|mode|math|math-display|true|<around*|[|3<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>5<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>7<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>9<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|11><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|13><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|15><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|17><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|19><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|21>|]>>
      </unfolded-io-math>
    </session>

    <item>The key combination <key|, Tab Tab> produces an ``invisible
    comma''. This is useful, in particular, to enter matrix subscripts in
    Pure, such as:

    <\session|pure|reduce>
      <\unfolded-io-math>
        \<gtr\>\ 
      <|unfolded-io-math>
        x<rsub|i\<nocomma\>j>;
      <|unfolded-io-math>
        <with|color|black|mode|math|math-display|true|x>!(<with|color|black|mode|math|math-display|true|i>,<with|color|black|mode|math|math-display|true|j>)
      </unfolded-io-math>

      <\unfolded-io-math>
        \<gtr\>\ 
      <|unfolded-io-math>
        let x=<matrix|<tformat|<table|<row|<cell|1>|<cell|2>>|<row|<cell|3>|<cell|4>>>>>;
        <around*|{|x<rsub|j\<nocomma\>i><mid|\|>i=0\<ldots\>1;j=0\<ldots\>1|}>;
      <|unfolded-io-math>
        <with|color|black|mode|math|math-display|true|<around*|(|<tabular*|<tformat|<table|<row|<cell|1>|<cell|3>>|<row|<cell|2>|<cell|4>>>>>|)>>
      </unfolded-io-math>
    </session>

    <item>Matrices, sub- and superscripts (<key|_> and <key|^>) and the usual
    arithmetic and logical operators will work in Pure. In particular, note
    that <key|*> will by default produce an ``invisible multiplication'' in
    <TeXmacs> which will be interpreted correctly in Pure, but you can also
    enter a visible multiplication instead (type <key|*> and then repeat
    <key|Tab> to cycle through the alternatives). For instance:

    <\session|pure|reduce>
      <\unfolded-io-math>
        \<gtr\>\ 
      <|unfolded-io-math>
        <around*|(|a+b*c|)><rsup|2>;
      <|unfolded-io-math>
        <with|color|black|mode|math|math-display|true|a<rsup|2>+2*a*b*c+b<rsup|2>*c<rsup|2>>
      </unfolded-io-math>
    </session>

    <item>Typing <key|\\eqnarray> gives you an equation array, i.e., a table
    of aligned display equations. This is useful, in particular, to enter
    Pure equations defining a function in a nicely formatted way:

    <\session|pure|reduce>
      <\input-math>
        \<gtr\>\ 
      <|input-math>
        <\eqnarray>
          <tformat|<table|<row|<cell|fact<around*|(|n|)>>|<cell|=>|<cell|n\<times\>fact<around*|(|n-1|)>
          if n\<gtr\>0;>>|<row|<cell|>|<cell|=>|<cell|1 otherwise;>>>>
        </eqnarray>
      </input-math>

      <\unfolded-io-math>
        \<gtr\>\ 
      <|unfolded-io-math>
        map fact <around*|(|1\<ldots\>10|)>;
      <|unfolded-io-math>
        <with|color|black|mode|math|math-display|true|<around*|[|1<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>2<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc>6<space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|24><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|120><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|720><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|5040><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|40320><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|362880><space|0.25spc><with|math-font-family|rm|,<space|0.25spc>><space|0.25spc><with|math-font-family|rm|3628800>|]>>
      </unfolded-io-math>

      <item>The <TeXmacs> <samp|choice> construct (<samp|Insert \| Table \|
      Choice>) can also be used to give nicely formatted definitions. For
      instance, the above definition of the factorial can also be written as
      follows:

      <\session|pure|reduce>
        <\input-math>
          \<gtr\>\ 
        <|input-math>
          <\equation*>
            fact<around*|(|n|)>=<choice|<tformat|<table|<row|<cell|n\<times\>fact<around*|(|n-1|)>>|<cell|if
            n\<gtr\>0>>|<row|<cell|1>|<cell|otherwise>>>>>;
          </equation*>
        </input-math>
      </session>
    </session>
  </itemize>

  This is only the tip of the iceberg; generally you can expect most
  <TeXmacs> math constructs to work as Pure code, as long as you keep to Pure
  syntax (using the right keywords and delimiters in the right places). The
  full version of this article gives a much more thorough overview of the
  available math constructs and how they are interpreted in Pure.

  <section|PostScript Output>

  Like the Python plugin, Pure can pipe PostScript graphics directly into
  <TeXmacs>. This is done with the <verbatim|ps> function from the
  <verbatim|texmacs> module which takes either verbatim PostScript code
  (indicated by a <verbatim|%!> header) or a PostScript filename as its
  string argument. In the latter case, if the PostScript filename doesn't
  have a slash in it, it is searched for in all directories on the
  <verbatim|TEXMACS_DOC_PATH>, and a <verbatim|.eps> or <verbatim|.ps> suffix
  is added automatically when needed. Otherwise the filename is interpreted
  relative to the current directory of the interpreter (which is normally the
  directory in which <TeXmacs> was started).

  Here is a simple example illustrating verbatim PostScript code:

  <\session|pure|postscript>
    <\unfolded-io>
      \<gtr\>\ 
    <|unfolded-io>
      ps "%!\\n%%BoundingBox: 195 295 405 405\\n/Times-Roman findfont 20
      scalefont setfont newpath 200 300 moveto 200 0 rlineto 0 100 rlineto
      -200 0 rlineto 0 -100 rlineto 235 345 moveto (Hello, world!) show
      closepath stroke showpage";
    <|unfolded-io>
      <image|<tuple|<#25210A2525426F756E64696E67426F783A203139352032393520343035203430350A2F54696D65732D526F6D616E2066696E64666F6E74203230207363616C65666F6E7420736574666F6E74206E6577706174682032303020333030206D6F7665746F20323030203020726C696E65746F20302031303020726C696E65746F202D323030203020726C696E65746F2030202D31303020726C696E65746F2032333520333435206D6F7665746F202848656C6C6F2C20776F726C6421292073686F7720636C6F736570617468207374726F6B652073686F7770616765>|ps>||||>

      ()
    </unfolded-io>
  </session>

  <section|Pure Scripting>

  Last but not least, Pure can also be used as a <em|scripting language> in
  <TeXmacs>. This is done by enabling the <samp|Document \| Scripts \| Pure>
  option (or <samp|Document \| Scripts \| Pure-math> if you prefer math
  output; this is what we use here). This enables a few additional options in
  the <samp|Pure> plugin menu, as well as the <samp|Insert \| Link> menu and
  the corresponding toolbar button. These facilities are described in more
  detail at the end of the ``<TeXmacs> \ as an interface'' section in the
  <TeXmacs> online help, so we only give a few basic examples here.

  <\bothlined>
    <strong|Note: >If you're reading this in the <TeXmacs> help browser, then
    Pure scripting will most likely not be enabled yet, so you should do this
    now if you'd like to try the examples below.
  </bothlined>

  For instance, you can just select any valid Pure expression in the text and
  evaluate it with the <samp|Evaluate> option of the <samp|Pure> menu or
  <key|Ctrl+Return> like this: <math|<around*|(|a+b|)><rsup|2>>
  <math|\<rightarrow\>> <with|color|black|mode|math|math-display|true|a<rsup|2>+2*a*b+b<rsup|2>>.
  It's also possible to create an <em|executable switch> such as
  <script-input|pure-script-math|default|<math|df<around*|(|<around*|(|x+y|)><rsup|3>,x|)>>|<with|color|black|mode|math|math-display|true|3*<around*|(|x<rsup|2>+2*x*y+y<rsup|2>|)>>>
  which can be toggled between input and computed result by pressing
  <key|Return> inside the field (try it!).

  Note that as an additional convenience, the scripting plugins accept a
  simple expression without the trailing semicolon as input. This is in
  contrast to the regular Pure plugins which allow you to enter definitions
  and expressions spanning multiple input lines, but also require you to
  terminate each input item with a semicolon. With the scripting plugins the
  terminating semicolon is optional and will be added automatically when
  needed, but it also doesn't hurt if you type it anyway:
  <script-input|pure-script-math|default|<math|df<around*|(|<around*|(|x+y|)><rsup|3>,x|)>;>|<with|color|black|mode|math|math-display|true|3*<around*|(|x<rsup|2>+2*x*y+y<rsup|2>|)>>>

  There's also the possibility to work with <em|executable fields> and
  <em|spreadsheets>. These offer the advantage that fields may depend on
  other fields in the same document.<\footnote>
    Note that the spreadsheet functionality requires <TeXmacs> 1.0.7.15 or
    later to work. Also be warned that, at least at the time of this writing,
    this might become <em|very> slow in large documents; however, it's
    possible to work around this limitation by breaking your document into
    smaller include files.
  </footnote> For instance, here is an example of a textual spreadsheet
  (<samp|Insert \| Table \| Textual spreadsheet>) showing some Pure and
  Reduce calculations. Type <key|Return> in the cells of the last column to
  reveal the underlying Pure formulas; also try changing some of the values
  in the <verbatim|b> and <verbatim|c> columns and hitting <key|Return> to
  recompute the corresponding values in the last column.

  <with|font-base-size|12|<\calc-table|table1>
    <\with|par-mode|center>
      <textual-table|<tformat|<cwith|1|1|1|-1|cell-background|pastel
      yellow>|<cwith|2|-1|1|-1|cell-hyphen|n>|<cwith|2|2|1|-1|cell-bborder|0>|<cwith|1|-1|1|1|cell-width|>|<cwith|1|-1|1|1|cell-hmode|auto>|<cwith|4|4|1|-1|cell-bborder|0>|<cwith|1|-1|1|1|cell-background|pastel
      yellow>|<cwith|2|-1|4|4|cell-background|pastel
      green>|<table|<row|<cell|<cell-inert|a1|a1>>|<cell|<cell-inert|b1|b>>|<cell|<cell-inert|c1|c>>|<cell|<cell-inert|d1|d>>>|<row|<cell|<cell-inert|a2|2>>|<cell|<cell-inert|b2|1>>|<cell|<cell-inert|c2|12>>|<cell|<cell-output|d2|=
      fact (b2+c2) with fact n = if n\<gtr\>0 then n*fact(n-1) else 1
      end|1932053504>>>|<row|<cell|<cell-inert|a3|3>>|<cell|<cell-inert|b3|17>>|<cell|<cell-inert|c3|33>>|<cell|<cell-output|d3|='(b3+c3)|<with|color|black|mode|math|math-display|true|50>>>>|<row|<cell|<cell-inert|a4|4>>|<cell|<cell-inert|b4|<math|sin
      <around*|(|x<rsup|2>|)>>>>|<cell|<cell-inert|c4|<math|x>>>|<cell|<cell-output|d4|=?df
      b4 c4|<with|color|black|mode|math|math-display|true|2*cos
      <around*|(|x<rsup|2>|)>*x>>>>|<row|<cell|<cell-inert|a5|5>>|<cell|<cell-inert|b5|<math|<around*|(|x+y|)><rsup|3>>>>|<cell|<cell-inert|c5|<math|x>>>|<cell|<cell-output|d5|=?df
      b5 c5|<with|color|black|mode|math|math-display|true|3*<around*|(|x<rsup|2>+2*x*y+y<rsup|2>|)>>>>>>>>
    </with>
  </calc-table>>

  You can also refer to a table field in text like this:
  <with|font-base-size|12|<calc-output|field3|<calc-ref|table1-d4>|<with|color|black|mode|math|math-display|true|2*cos
  <around*|(|x<rsup|2>|)>*x>>>. Here we set the Ref field of the table to
  <verbatim|table1>; the reference to cell <verbatim|d4> can then be entered
  with the following series of key strokes: <key|\\ ! \\ ?>
  <verbatim|table1-d4> <key|Return>.

  <section|More Information>

  This is just a very brief overview of the facilities provided by this
  plugin. Using Pure's Octave and Reduce interfaces, Pure can be employed as
  a full-featured scientific computing environment in <TeXmacs>, and with the
  other addon modules available for Pure you can extend <TeXmacs> in various
  useful ways. More information can be found in the full version of this
  manual, which is available in printable format in the
  <hlink|pure-texmacs.en.tm|pure-texmacs.en.tm> file. (This file won't be
  rendered correctly inside the <TeXmacs> help system, so you should locate
  it in the plugins/pure/doc directory and open it as a regular <TeXmacs>
  document in order to print it.)
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