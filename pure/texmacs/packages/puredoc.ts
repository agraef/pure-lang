<TeXmacs|1.0.7.16>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|puredoc|0.1>

      <\src-purpose>
        Formatting of the Pure online documentation. To be used with
        generic.ts or any other basic document style of your choice.
      </src-purpose>

      <\src-copyright|2012>
        Albert Gräf
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <active*|<\src-comment>
    Some convenient formatting options for the <TeXmacs>-formatted Pure
    documentation. Specifically, the verbatim environments have par-par-sep
    set to zero, and images are moved relative to the baseline. We also add
    article-style section numbering and adjust the rendering of the
    description environments to get rid of the trailing dot in the
    description labels. This gives a pretty nice rendering of the documents,
    but of course your preferences may vary, so you might want to adjust the
    following definitions as needed.
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