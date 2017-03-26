<a name="doc-pure-doc"></a>

pure-doc
========

Version 0.7, March 06, 2017

Albert Graef &lt;<aggraef@gmail.com>&gt;

pure-doc is a simple utility for literate programming and documenting source
code written in the Pure programming language. It is designed to be used with
the excellent [docutils](http://docutils.sourceforge.net) tools and the gentle
markup format supported by these, called [RST](#rst) a.k.a.
"reStructuredText", usually pronounced "rest".

The basic idea is that you just comment your code as usual, but using RST
markup instead of plain text. In addition, you can also designate literate
programming fragments in your code, which will be translated to RST literal
blocks automatically. You then run pure-doc on your source files to extract
all marked up comments and the literate code blocks. The resulting RST source
can then be processed with the docutils utilities like rst2html.py and
rst2latex.py to create the documentation in a variety of formats.

Copying
-------

Copyright (c) 2009-2010 by Albert Graef.

pure-doc is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

pure-doc is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see &lt;<http://www.gnu.org/licenses/>&gt;.

Installation
------------

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-doc-0.7.tar.gz>.

Unpack and do the customary `make && sudo make install`. This only needs flex
and a standards-compliant C++ compiler.

Usage
-----

First, see the description of the [RST](#rst) format. RST is a very simple
markup format, almost like plain text (in fact, you're looking at RST right
now, this document is written in it!). You can learn enough of it to start
marking up your source in about five minutes.

Second, you'll have to mark up your source comments. pure-doc recognizes
comments in RST format by looking at the first non-empty line of the comment.
A comment (either `/* ... */` or a contiguous sequence of `//` line comments)
is assumed to contain RST format if the first non-empty line starts with `:`,
`..` or `__`. Other comments are taken to be plain text and are ignored by
pure-doc.

Notes:

-   pure-doc makes no other assumption about the contents of marked up
    comments, so you can include whatever you want: titles, section headers,
    fields, admonitions, plain text, whatever. Just make sure that the comment
    starts with one of the special tokens listed above. (You can always put
    just `..` at the beginning of the comment to force it to be recognized,
    this will be treated as a comment by the docutils tools.)
-   Also, pure-doc makes very few assumptions about the source; in fact, any
    source files with a C/C++-like comment and string syntax should work. So
    you could also use it to document your C/C++ programs, or even plain text
    files like this one, as long as they adhere to these standards.
-   Indentation in extracted comments is preserved (assuming tabs = 8 spaces
    by default, you can change this with the `-t` option). This is important
    because indentation conveys document structure in RST.

For instance, here is a sample RST-formatted comment:

    /* :Name: ``rand`` - compute random numbers
       :Synopsis: ``rand``
       :Description: Computes a (pseudo) random number. Takes no parameters.
       :Example: Here is how you can call ``rand`` in Pure:
         ::

           > extern int rand();
           > rand;
           1804289383

       :See Also: rand(3) */

This will be rendered as follows:

> Name
>
> :   `rand` - compute random numbers
>
> Synopsis
>
> :   `rand`
>
> Description
>
> :   Computes a (pseudo) random number. Takes no parameters.
>
> Example
>
> :   Here is how you can call `rand` in Pure:
>
> >     > extern int rand();
> >     > rand;
> >     1804289383
>
> See Also
>
> :   rand(3)
>
> Finally, to extract the documentation you run pure-doc on your source files
> as follows:

    pure-doc source-files ...

If no input files are specfied then the source is read from standard input.
Otherwise all input files are read and processed in the indicated order. The
output is written to stdout, so that you can directly pipe it into one of the
docutils programs:

    pure-doc source-files ... | rst2html.py

If you prefer to write the output to a file, you can do that as follows:

    pure-doc source-files ... > rst-file

pure-doc also understands the following options. These must come before any
file arguments.

`-h`
:   Print a short help message.

`-i`
:   Automatic index creation (see below).

`-s`
:   Generate Sphinx-compatible output (see below).

`-twidth`
:   Set the tab width to the given number of spaces.

There are no other options. By its design pure-doc is just a plain simple
"docstring scraping" utility with no formatting knowledge of its own. All
actual formatting is handled by the docutils programs which offer plenty of
options to change the appearance of the generated output; please refer to the
[docutils](http://docutils.sourceforge.net) documentation for details.

Note that since Pure 0.46, all Pure documentation is usually formatted using
[Sphinx](http://sphinx.pocoo.org), the RST formatter used by the Python
project which provides cross-document indexing and referencing, and even more
elaborate formatting options and prettier output than docutils. pure-doc
versions since 0.6 support this by adding the `-s` option which makes its
output compatible with Sphinx. (At present this option actually has any effect
only when combined with the `-i` index generation option, see [Hyperlink
Targets and Index Generation](#hyperlink-targets-and-index-generation) below.)

Literate Programming
--------------------

pure-doc also recognizes literate code delimited by comments which, besides
the comment delimiters and whitespace, contain nothing but the special start
and end "tags" `>>>` and `<<<`. Code between these delimiters (including all
comments) is extracted from the source and output as a RST literal code block.

For instance:

    /* ..

       pure-doc supports literate programming, too. */

    // >>>

    // This is a literate comment.
    /* .. This too! */

    extern int rand();
    rand;

    // <<<

This will be rendered as follows:

> pure-doc supports literate programming, too.
>
>     // This is a literate comment.
>     /* .. This too! */
>
>     extern int rand();
>     rand;

Try it now! You can scrape all the sample "documentation" from this file and
format it as html, as follows:

    pure-doc README | rst2html.py --no-doc-title --no-doc-info > test.html

Hyperlink Targets and Index Generation
--------------------------------------

------------------------------------------------------------------------------

> **Note:** This feature is now largely obsolete as Pure uses Sphinx for
> formatting its documentation these days. Thus, as of version 0.6, the
> indexing feature must be enabled explicitly with the `-i` option.

------------------------------------------------------------------------------

When run with the `-i` option, pure-doc supplements the normal hyperlink
target processing by the docutils tools, by recognizing explicit hyperlink
targets of the form `.. _target:` and automatically creating raw html targets
(`<a name=...>`) for them. This works around the docutils name mangling (which
is undesirable if you're indexing, say, function names). It also resolves a
quirk with some w3m versions which don't pick up all `id` attributes in the
docutils-generated html source.

In addition, you can also have pure-doc generate an index from all explicit
targets. To these ends, just add the following special directive at the place
where you want the index to appear:

    .. makeindex::

The directive will be replaced with a list of references to all targets
collected *up to that point*, sorted alphabetically. This also resets the list
of collected targets, so that you can have multiple smaller indices in your
document instead of one big one.

It goes without saying that this facility is rather simplistic, but it may be
useful when you are working with plain docutils which does not provide its own
indexing facility. Note, however, that docutils doesn't allow multiple
explicit targets with the same name, so you should take that into
consideration when devising your index terms.

Also note that in Sphinx compatibility mode (`-s`), pure-doc will generate the
appropriate Sphinx markup for index entries (`index::`) instead, and the
`makeindex::` directive will be ignored. You should then use Sphinx to
generate the index.

Finally, if the `-i` option isn't specified, then all this special processing
is disabled and the `makeindex::` directive won't be recognized at all. This
is the recommended way to process Pure documentation files which have been
fully converted to Sphinx.

Generating and Installing Local Documentation
---------------------------------------------

------------------------------------------------------------------------------

> **Note:** This section only applies to 3rd party packages with their own
> bundled documentation which isn't part of the "official" Pure documentation.
> In this case it is possible to use docutils or some other RST formatting
> software to generate additional documentation files for use with the Pure
> interpreter. Please note that the method sketched out in this section
> doesn't provide full integration with the rest of Pure's documentation, but
> at least it makes it possible to read the local documentation in the
> interpreter.

------------------------------------------------------------------------------

If you're generating some library documentation for which you have to process
a bigger collection of source files, then it is often convenient to have a few
Makefile rules to automatize the process. To these ends, simply add rules
similar to the following to your Makefile (the following assumes GNU make and
that you're using docutils to format the documentation):

``` {.sourceCode .make}
# The sources. Order matters here. The generated documentation will have the
# comments from each source file in the indicated order.
sources = foo.pure bar.pure

# The basename of the documentation files to be generated.
target = foo

.PHONY: html tex pdf

html: $(target).html
tex: $(target).tex
pdf: $(target).pdf

$(target).txt: $(sources)
        pure-doc $(sources) > $@

# This requires that you have docutils installed.

%.html: %.txt
        rst2html.py $< $@

%.tex: %.txt
        rst2latex.py $< $@

# This also requires that you have TeX installed.

%.pdf: %.tex
        pdflatex $<
        rm -f *.aux *.log *.out

clean:
        rm -f *.html *.tex *.pdf
```

You might want to add `-i` to the pure-doc command line if you want to enable
the indexing feature described in the previous section. If you want to use
some other RST formatting software, please check the corresponding
documentation for information on how to format your documents and adjust the
above rules for the html, tex and pdf targets accordingly.

Now you can just type `make html` to generate the documentation in html
format, and `make tex` or `make pdf` to generate the other formats. The
`clean` target removes the generated files.

Having generated the documentation files in html format, you can install them
in the docs subdirectory of the Pure library directory to make it known to the
Pure interpreter, so that you can read your documentation with the `help`
command of the interpreter. (When doing this, name your documentation files in
such a manner that you don't overwrite any of the Pure documentation files
there.) The following Makefile rule automatizes this process. Add this to the
Makefile in the previous section:

``` {.sourceCode .make}
# Try to guess the installation prefix (this needs GNU make):
prefix = $(patsubst %/bin/pure,%,$(shell which pure 2>/dev/null))
ifeq ($(strip $(prefix)),)
# Fall back to /usr/local.
prefix = /usr/local
endif

libdir = $(prefix)/lib
docsdir = $(libdir)/pure/docs

install:
        test -d "$(DESTDIR)$(docsdir)" || mkdir -p "$(DESTDIR)$(docsdir)"
        cp $(target).html "$(DESTDIR)$(docsdir)"
```

After a `make install` your documentation should now end up in the appropriate
place in the Pure library directory and you can read it in the Pure
interpreter using a command like the following:

    > help foo#

Note the hash character. This tells the `help` command that this is an
auxiliary documentation file, rather than a search term to be looked up in the
Pure documentation. You can also look up a specific section in your manual as
follows:

    > help foo#section-name

Please also refer to [The Pure Manual](pure.html) for more information on how
to use the interpreter's online help.

Formatting Tips
---------------

If you're generating documentation in pdf format using plain docutils, you
might have to fiddle with the formatting to get results suitable for
publication purposes. Newer versions of the rts2latex.py program provide some
options which let you adjust the formatting of various document elements. Here
are the options that the author found particularly helpful:

-   The table of contents that RST produces isn't all that useful in printed
    documentation, since it lacks page numbers. As a remedy, you can invoke
    rst2latex with `--use-latex-toc` to have LaTeX handle the formatting of
    the table of contents, which looks much nicer.
-   Similarly, `--use-latex-docinfo` can be used to tell rst2latex that you
    want the title information (author and date) to be formatted the LaTeX
    way.
-   If you need specific LaTeX document options, these can be specified with
    `--documentoptions`, e.g.: `--documentoptions="11pt"`.
-   For more comprehensive formatting changes which require special LaTeX code
    and/or packages, you can use the `--stylesheet` option. E.g.,
    `--stylesheet=preamble.tex` will cause a preamble.tex file with your own
    definitions to be included in the preamble of the generated document.
-   To format literal code blocks using an alternative environment instead of
    the default verbatim environment, use the `--literal-block-env` option.
    E.g., `--literal-block-env=lstlisting` will use the highlighted code
    environment from the listings package. (Note that in this case you'll also
    need a preamble which loads the corresponding package.).

To learn more about this, please consult the rts2latex.py documentation at the
docutils website.

In addition, the pure-doc package contains a little GNU awk script called
fixdoc, which attempts to improve the LaTeX output produced by older svn
versions of rst2latex in various ways. (This isn't necessary for the latest
rst2latex versions, or if you use Sphinx.)
