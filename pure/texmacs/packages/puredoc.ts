<TeXmacs|1.0.7.16>

<style|source>

<\body>
  <active*|<\src-comment>
    puredoc style option. This makes up for some of the glitches in the
    converted html docs: verbatim has par-par-sep set to zero, and images are
    moved relative to the baseline. We also add article-style section
    numbering and adjust the description environments.
  </src-comment>>

  <assign|orig-verbatim|<value|verbatim>>

  <assign|verbatim|<macro|body|<with|par-par-sep|0fn|<orig-verbatim|<arg|body>>>>>

  <assign|puredoc-image|<macro|img|w|h|x|y|<move|<image|<arg|img>|<arg|w>|<arg|h>|<arg|x>|<arg|y>>|0fn|-0.3fn>>>

  <use-package|section-article>

  <use-package|std-list>

  <new-list|description-compact|<value|compact-strong-space-item>|<macro|name|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-aligned|<value|aligned-strong-space-item>|<macro|name|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-dash|<value|compact-strong-dash-item>|<macro|name|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description-long|<value|long-compact-strong-space-item>|<macro|name|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  <new-list|description|<value|compact-strong-space-item>|<macro|name|<active*|<with|mode|math|<with|math-font-series|bold|<rigid|\<ast\>>>>>>>

  \;
</body>