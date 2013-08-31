<TeXmacs|1.0.7.16>

<style|<tuple|tmdoc|varsession>>

<\body>
  <section|About Pure>

  <hlink|Pure|http://purelang.bitbucket.org/> is a modern-style functional
  programming language based on term rewriting. It offers equational
  definitions with pattern matching, full symbolic rewriting capabilities,
  dynamic typing, eager and lazy evaluation, lexical closures, built-in list
  and matrix support and an easy-to-use C interface. The interpreter uses
  <hlink|LLVM|http://llvm.org/> as a backend to JIT-compile Pure programs to
  fast native code.

  Pure is by itself a very advanced language for symbolic computations. In
  addition, both <hlink|Octave|http://www.octave.org/> and
  <hlink|Reduce|http://reduce-algebra.com/> can be run as embedded components
  in Pure, which creates a nicely integrated and powerful environment for
  scientific computing. But Pure gives you much more than that; it provides
  you with a full-featured functional programming environment with a fairly
  comprehensive collection of add-on modules for all major areas of
  computing, and the ability to interface to other 3rd party software quite
  easily if needed.

  The integration with <TeXmacs> adds another dimension by letting you write
  Pure programs in a style which looks just like mathematical definitions.
  And these formulas don't just sit there looking nice, they can be executed,
  too! The plugin supports all major features of the <TeXmacs> interface,
  including Pure sessions and scripting, completion of Pure keywords and
  function names, accessing the Pure online help facility, as well as
  mathematical input and output (the latter is implemented using the Reduce
  <samp|<verbatim|tmprint>> package and thus requires Reduce). The examples
  in this document show off some of Pure's symbolic computing capabilities in
  <TeXmacs>, using Pure's onboard facilities as well as the Reduce interface
  which nicely integrates with Pure and <TeXmacs>.<\footnote>
    Note that the examples in each section are to be executed in the given
    order, as some calculations rely on earlier definitions.
  </footnote>

  <\acknowledgments*>
    Thanks are due to Kurt Pagani who provided much help, mission-critical
    Reduce/Lisp code and documentation for the Reduce and <TeXmacs>
    interfaces. Without his perseverance, insight and encouragment this
    plugin wouldn't exist.
  </acknowledgments*>
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