<TeXmacs|1.0.7.16>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|dejavu|0.1>

    <\src-purpose>
      Format program code using DejaVu Sans Mono.
    </src-purpose>

    <src-copyright|2012|Albert Gräf>

    <\src-license>
      Copying and distribution of this file, with or without modification,
      are permitted in any medium without royalty provided the copyright
      notice and this notice are preserved. This file is offered as-is,
      without any warranty.
    </src-license>
  </src-title>>

  <active*|<\src-comment>
    A little macro to enable DejaVu Sans Mono in program code, which looks
    nicer than Courier (IMHO). <with|font-series|bold|Note:> We set both
    <active*|font> and prog-font here, since some environments use one and
    some the other. Also note that this font needs to be scaled down a bit so
    that it works with standard text fonts.
  </src-comment>>

  <assign|dejavu|<macro|body|<with|font|dejavu|prog-font|dejavu|font-size|<times|<value|font-size>|0.86>|<arg|body>>>>

  <active*|<\src-comment>
    Remap verbatim code to DejaVu.
  </src-comment>>

  <assign|orig-verbatim|<value|verbatim>>

  <assign|verbatim|<macro|body|<compound|orig-verbatim|<compound|dejavu|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Remap session input and output to DejaVu.
    </src-comment>
  </active*>

  <assign|orig-generic-output|<value|generic-output>>

  <assign|generic-output|<macro|body|<style-with|src-compact|none|<orig-generic-output|<compound|dejavu|<arg|body>>>>>>

  <assign|orig-generic-input|<value|generic-input>>

  <assign|generic-input|<macro|prompt|body|<style-with|src-compact|none|<orig-generic-input|<dejavu|<arg|prompt>>|<dejavu|<arg|body>>>>>>

  \;
</body>