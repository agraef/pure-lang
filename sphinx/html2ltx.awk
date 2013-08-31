
# html2ltx.awk by Albert Graef <aggraef@gmail.com> 2012-11-04

# This is a little awk script to fix up various glitches when converting
# Sphinx html docs to LaTeX input for TeXmacs using pandoc. The script
# requires GNU awk.

##############################################################################

# There are some glitches left which aren't easy (or downright impossible) to
# fix in the LaTeX source, but can be dealt with by preprocessing the html
# input and/or using the appropriate TeXmacs style options. A suitable
# puredoc.ts style file is provided. The following fixes and workarounds are
# currently implemented:

# - Excessive line spacing in verbatim environments. The TeXmacs verbatim
#   environment uses the default par-par-sep value; this is fixed by setting
#   par-par-sep to 0fn in the verbatim macro.

# - Trailing period in description tags. This comes from the TeXmacs
#   description environment, and is fixed by redefining the corresponding
#   macros accordingly.

# - Block-formatted text in the html source uses special markup which pandoc
#   doesn't recognize. We try to fix this by inserting paragraph breaks in the
#   html source, but this is experimental and might not look good in some
#   cases.

# - Pandoc doesn't recognize the Sphinx-generated link targets. Although we
#   make a Herculean effort to recover most of these, this isn't perfect yet
#   (some link targets may get lost or just won't work in TeXmacs).

# - Sphinx uses an awful lot of special markup in the generated indices
#   (genindex and pure-modindex). We try to reformat this as good as we can,
#   but this part of the code might still need adjusting for special kinds of
#   index entries.

# - Similar remarks apply to tables. Pandoc doesn't handle these at all, so we
#   recover them from some simplistic homegrown markup added in the
#   preprocessing phase. This seems to be good enough for the simple kinds of
#   rst tables used in the Pure documentation, but we don't handle more
#   sophisticated table markup in the html yet.

##############################################################################

# Add the bare minimum necessary so that TeXmacs recognizes the encoding.
BEGIN {
    print "\\usepackage[utf8x]{inputenc}";
    # parse modes
    mode = ta = 0; lastlabels = lastbreak = "";
    # scaling of figures
    scale = 66;
}

# Trim down the navigation bar.
/\\subsubsection{Navigation}/ {
    if (mode==9) print "";
    mode = 1;
    print sprintf("\\href{#%s-toc}{toc}", name);
    next;
}
mode==0 { next; }
mode==1 && (/\\begin{itemize}/) { mode = 2; next; }
mode==1 { next; }
mode==2 && /\\item/ { next; }
mode==2 && /\\end{itemize}/ { mode = 3; print ""; next; }
mode==2 {
    gsub(/\\textbar{}/, " |");
    gsub(/»/, "");
    $0 = gensub(/\\href{([^:/]+)[.]html((#[^}]*)?)}{([^}]*)}/, "\\\\href{\\1.tm\\2}{\\4}", "g");
    print $0;
    next;
}

# Get rid of auto-generated floats for figures and their captions.
/\\begin{figure}\[htb\]/ {
    mode = 4;
    print "\\begin{center}";
    next;
}
mode==4 && /\\includegraphics/ {
    gsub(/\\includegraphics/, sprintf("\\includegraphics[width=%d\\%,height=%d\\%]", scale, scale));
    print $0 "\n";
    next;
}
mode==4 && /\\end{figure}/ {
    mode = 3;
    print "\\end{center}";
    next;
}
mode==4 { next; }

# Get rid of special sidebar elements which are useless in texmacs.
/\\subsubsection{(This Page|Quick [Ss]earch)}/ {
    if (mode==9) print "";
    mode = 5;
    next;
}
# Reformat the backmatter that we want to keep (table of contents).
/\\subsubsection{.*Table Of Contents.*}/ {
    if (mode==9) print "";
    mode = 3;
    gsub(/\\subsubsection/, "\n\\subsubsection*");
    gsub(/{Table Of Contents}/, sprintf("&\\label{%s-toc}", name));
    $0 = gensub(/\\href{([^:/]+)[.]html((#[^}]*)?)}{([^}]*)}/, "\\\\href{\\1.tm\\2}{\\4}", "g");
    print $0;
    next;
}
mode==5 { next; }

# Do a bit of formatting for Note callouts.
/^Note$/ {
    print "\\textbf{Note:}";
    if (match(getline, /^$/)) getline;
    next;
}

# Start formatting index entries.
/^\\section{(Module )?Index}$/ {
    mode = 9;
    gsub(/\\section/, "\\section*");
    print;
    next;
}
# Get rid of included graphics in the index.
(mode==9 && /^\\includegraphics.*{.*}$/) {
    next;
}

# All other content.
{
    # Some special TeX commands and characters causing trouble with TeXmacs.
    gsub(/\\textbar{}/, "|");
    gsub(/\\textless{}/, "<");
    gsub(/\\textgreater{}/, ">");
    gsub(/«/, "<<");
    gsub(/»/, ">>");
    gsub(/≡/, "==");
    gsub(/−/, "-");

    # Other UTF-8 characters that we might want to remap to ASCII facsimiles.
    # gsub(/—/, "--");
    # gsub(/–/, "--");
    gsub(/‘/, "`");
    gsub(/’/, "'");
    gsub(/“/, "``");
    gsub(/”/, "''");
    # gsub(/©/, "(c)");

    # Enforce smart quotes in simple cases where Sphinx doesn't get it right.
    #$0 = gensub(/`(\\?[^`]|\\verb.[^`].)`/, "`\\1'", "g");

    # Enforce line breaks in option lists.
    gsub(/^-[a-zA-Z-]+$/, "\n&");

    # Greek and other math symbols in plain text. TODO: Add others as needed.
    gsub(/α/, "\\ensuremath{\\alpha}");
    gsub(/β/, "\\ensuremath{\\beta}");
    gsub(/ε/, "\\ensuremath{\\varepsilon}");
    gsub(/ζ/, "\\ensuremath{\\zeta}");
    gsub(/π/, "\\ensuremath{\\pi}");
    gsub(/φ/, "\\ensuremath{\\phi}");
    gsub(/×/, "\\ensuremath{\\times}");
    gsub(/±/, "\\ensuremath{\\pm}");
    gsub(/•/, "\\ensuremath{\\bullet}");
    gsub(/∈/, "\\ensuremath{\\in}");
    gsub(/⊃/, "\\ensuremath{\\supset}");
    gsub(/≤/, "\\ensuremath{\\leq}");
    gsub(/≠/, "\\ensuremath{\\neq}");
    gsub(/≈/, "\\ensuremath{\\approx}");
    gsub(/∀/, "\\ensuremath{\\forall}");
    gsub(/√/, "\\ensuremath{\\sqrt{}}");
    gsub(/···/, "\\ensuremath{\\cdots}");
    gsub(/ℜ/, "\\ensuremath{\\frak R}");
    gsub(/ℑ/, "\\ensuremath{\\frak I}");
    gsub(/‣/, "\\ensuremath{\\blacktriangleright}");

    # The sections are actually entire documents, turn off numbering of these.
    gsub(/\\section/, "\\section*");

    # For some reason, the keyword prefixes of Pure "constant", "variable",
    # etc. items lack a whitespace afterwards. Add it back.
    gsub(/(\\item\[|\\\\)\\emph{[^}]*}/, "& ");

    # Scaling of image files.
    if (/\\includegraphics/) {
	gsub(/\\includegraphics/, sprintf("\\includegraphics[width=%d\\%,height=%d\\%]", scale, scale));
	$0 = $0 "\n";
    }

    # Get rid of back links (the corresponding link targets don't exist anyway).
    $0 = gensub(/\\href{#id[0-9]+}{([^}]*)}/, "\\1", "g");

    # Change links to local html documents so that they point to the
    # corresponding .tm files instead.
    $0 = gensub(/\\href{([^:/}]+)[.]html((#[^}]*)?)}{([^}]*|\\textbf{[^}]*})}/, "\\\\href{\\1.tm\\2}{\\4}", "g");

    # Index entries. We need to break the main entries into paragraphs.
    if (mode==9 && /^(\\href{.*}|-.*)$/) {
	# check for subentries
	if (!/{\([^)]+\)}/ && !/command line option/) {
	    $0 = lastbreak $0;
	    lastbreak = "\n";
	}
    }

    # TeXmacs parses [...] inside description tags incorrectly. Make sure that
    # the brackets get through. Also, multiple declarations are separated with
    # a newline in the description tag which TeXmacs just ignores. Create
    # multiple items instead.
    if (match($0, /\\item\[.*\]/)) {
	x = $0;
	y = substr(x, RSTART+6, RLENGTH-7);
	gsub(/\[/, "{[}", y);
	gsub(/\]/, "{]}", y);
	gsub(/\\\\/, "]\n\\item[", y);
	$0 = substr(x, 1, RSTART+5) y substr(x, RSTART+RLENGTH-1);
    }

    # Pandoc won't recognize the special link targets that Sphinx produces.
    # Fortunately, most link targets also have a special reference link which
    # we don't need, we can turn that into an appropriate LaTeX label instead.
    $0 = gensub(/\\href{#([^}]*)}{¶?}/, "\\\\label{\\1}", "g");
    gsub(/¶/, "");

    # See whether we have something which looks like a (level>=4) heading (a
    # line which ends in a label) and format it as a heading.
    if (/^(\\label{[^}]*}| |\t)*$/) {
	# A line consisting of nothing but labels. To be inserted later.
	lastlabels = lastlabels $0;
	$0 = "";
    } else if (/^.+\\label{[^}]*}$/)
	gsub(/^.+\\label{[^}]*}$/, "\\paragraph{&}");

    # Index subsections. We have to manually add the labels there.
    if (mode==9) {
	if (/^\\subsection{.*}$/)
	    $0 = gensub(/^\\subsection{(.*)}$/, "\\\\subsection*{\\1\\\\label{\\1}}", "g");
	else if (/^\\textbf{[a-z]}$/)
	    $0 = gensub(/^\\textbf{(.*)}$/, "\\\\subsection*{\\1\\\\label{cap-\\1}}", "g");
	else if (/^~$/ || /^\\emph{}$/ || /^\\includegraphics.*{.*}$/)
	    $0 = "";
	else if (/^~~~/)
	    gsub(/^~~~/, "\\ \\ \\ ");
    }

    # If we have a nonempty line and some pending labels, output them now.
    # This is done to prevent spurious empty lines.
    if ($0 && !/^\\(begin|end){.*}/ && !/^\\item\[.*\]/ && lastlabels) {
	$0 = lastlabels $0;
	lastlabels = "";
    }

    # Translate pseudo table markup inserted by htmlpre.awk.
    if (/^[.]TA [0-9]+$/) {
	format = "";
	if (match($0, /^[.]TA ([0-9]+)$/, matches)) {
	    columns = strtonum(matches[1]);
	    for (i = 1; i <= columns; i++)
		format = format "l";
	}
	$0 = "\\begin{tabular}{" format "}";
	ta = 1; th = 0; rowno = 0; colno = 0;
    } else if (/^[.]TH$/) {
	$0 = "";
	th = 1; rowno = 0; colno = 0;
    } else if (/^[.]TB$/) {
	if (th && rowno > 0)
	    $0 = "\\\\\n\\hline";
	else
	    $0 = "";
	th = 0; rowno = 0; colno = 0;
    } else if (/^[.]TR$/) {
	if (rowno > 0)
	    $0 = "\\\\";
	else
	    $0 = "";
	rowno++;
	colno = 0;
    } else if (/^[.]TC$/) {
	colno++;
	if (colno > 1)
	    $0 = "&";
	else
	    $0 = "";
    } else if (/^[.]TE$/) {
	$0 = "\\end{tabular}\n";
	rowno = colno = 0; ta = 0;
    }

    # Finally, mangle all link targets containing characters with a special
    # meaning in TeX. TeXmacs doesn't like these.
    x = $0; $0 = "";
    while (match(x, /(\\label|\\href){[^}]*}/)) {
	y = substr(x, RSTART, RLENGTH);
	gsub(/%40/, "@", y);
	gsub(/%7C/, "|", y);
	gsub(/%3C/, "<", y);
	gsub(/%3E/, ">", y);
	y = gensub(/%([0-9a-fA-F][0-9a-fA-F])/, "?\\1", "g", y);
	gsub(/&|\\&/, "-amp", y);
	gsub(/~|\\~/, "-tilde", y);
	gsub(/\$|\\\$/, "-dollar", y);
	gsub(/_|\\_/, "-", y);
	gsub(/%|\\%/, "\\%", y);
	$0 = $0 substr(x, 1, RSTART-1) y;
	x = substr(x, RSTART+RLENGTH);
    }
    $0 = $0 x;

    if (!ta || $0) print;
}
