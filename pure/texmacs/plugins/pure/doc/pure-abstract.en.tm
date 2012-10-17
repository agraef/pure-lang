<TeXmacs|1.0.7.16>

<style|<tuple|tmdoc|varsession>>

<\body>
  <section|About Pure>

  <hlink|Pure|http://pure-lang.googlecode.com/> is a modern-style functional
  programming language based on term rewriting. It offers equational
  definitions with pattern matching, full symbolic rewriting capabilities,
  dynamic typing, eager and lazy evaluation, lexical closures, built-in list
  and matrix support and an easy-to-use C interface. The interpreter uses
  <hlink|LLVM|http://llvm.org/> as a backend to JIT-compile Pure programs to
  fast native code.

  Pure is by itself a very advanced language for the symbolic manipulation of
  expressions which also offers interfaces to
  <hlink|Octave|http://www.octave.org/> and
  <hlink|Reduce|http://reduce-algebra.com/> in order support scientific
  computing. Running Pure along with these modules gives you much more than
  just a powerful calculator, however; it provides you with a full-blown
  functional programming environment with a fairly comprehensive collection
  of add-on modules for all major areas of computing.

  The integration with <TeXmacs> adds another dimension by letting you write
  Pure programs in a style which looks very much like mathematical
  descriptions of algorithms, but those descriptions don't just sit there
  looking nice, they can be executed, too! The examples in this document show
  off some of Pure's symbolic computing capabilities in <TeXmacs>, using
  Pure's onboard facilities as well as the Reduce interface which nicely
  integrates with Pure and <TeXmacs>.<\footnote>
    Note that the examples in each section are to be executed in the given
    order, as some calculations rely on earlier definitions.
  </footnote>
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