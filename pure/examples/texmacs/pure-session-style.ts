<TeXmacs|1.0.7.16>

<style|source>

<\body>
  <\active*>
    <\src-comment>
      Here's an example showing how you can change the formatting of input
      and output fields in a Pure session. Adapt this as needed. Note that
      this needs to go into your ~/.TeXmacs/packages directory to become
      available in <compound|TeXmacs><active*|>.
    </src-comment>
  </active*>

  <assign|pure-output|<macro|body|<style-with|src-compact|none|<generic-output|<with|color|black|<arg|body>>>>>>

  <assign|pure-input|<macro|prompt|body|<style-with|src-compact|none|<generic-input|<with|color|dark
  green|<arg|prompt>>|<with|color|dark blue|<arg|body>>>>>>

  <assign|pure-plain-output|<value|pure-output>>

  <assign|pure-plain-input|<value|pure-input>>

  <assign|pure-quiet-output|<value|pure-output>>

  <assign|pure-quiet-input|<value|pure-input>>
</body>