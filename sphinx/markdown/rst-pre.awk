# rst-pre.awk

# This script requires GNU awk.

# This script recognizes some RST and Sphinx constructs which pandoc doesn't
# know about or doesn't translate correctly. The script "escapes" these using
# special syntax which can be parsed by pandoc in order to translate the RST
# document to Markdown.

# The companion rst-post.awk script can then be used to process the resulting
# Markdown document, turning the special constructs into corresponding
# Markdown code. So the two scripts are supposed to be used in a pipeline
# like this:

# gawk -f rst-pre.awk file | pandoc -f rst -t markdown | gawk -f rst-post.awk -

# In addition, the rst-pre.awk script also expands the @version@ and |today|
# variables in a Sphinx document. To these ends, the version number and date
# to be used can be specified using the version and date Awk variables.

# Please note that these scripts are mainly intended to deal with the
# idiosyncrasies of the Pure documentation files which are in Sphinx format.
# They may or may not work with other Sphinx/RST documentation.

BEGIN {
    mode = 0; verbatim = 0; skipped = 0; def = ""; counter = 0; prog = "";
    blanks = "                                                             ";
    # These are just defaults, you can specify values for these on the command
    # line.
    if (!version) version = "@version@";
    if (!date) date = strftime("%B %d, %Y", systime());
    if (!tmpfile) tmpfile = ".rst-markdown-targets";
    if (!raw) raw = "no";
    # Initialize the index file.
    system("rm -f " tmpfile);
}

# If the file didn't end with an empty line, we add one, just in case.
ENDFILE { if (prev) print ""; }

# Keep track of the previous line.
{ prev = current; current = $0; }

# Deal with left-overs from the previous line.
link_prefix {
    if (match($0, /^(\s*)(([^`]|\\`)+)`__\>/, matches)) {
	print link_line;
	gsub(/^(\s*)(([^`]|\\`)+)`__\>/,
	     sprintf("%s!hrefx(id%d)!%s%s!end!", matches[1], counter, link_prefix, matches[2]));
    } else if (match($0, /^(\s*)(([^`]|\\`)+)`_\>/, matches)) {
	print link_line;
	gsub(/^(\s*)(([^`]|\\`)+)`_\>/,
	     sprintf("%s!href!%s%s!end!", matches[1], link_prefix, matches[2]));
    } else {
	print link_prev;
    }
    link_prefix = link_line = link_prev = "";
}

# Nothing gets expanded during a verbatim code section (see below).
verbatim > 0 {
    if (match($0, /^(\s*)/))
	indent = RLENGTH+1;
    else
	indent = 1;
    if (indent <= verbatim && !match($0, /^\s*$/))
	verbatim = skipped = 0;
    if (skipped == 0) print; next;
}

# Substitute version and date placeholders.
/@version@/ {
    gsub(/@version@/, version);
}

/\|today\|/ {
    gsub(/\|today\|/, date);
}

# pandoc doesn't seem to understand the double colon at the end of the line,
# so we expand it to a proper code section.
/\s+::\s*$/ {
    if (match($0, /^(\s*)/))
	verbatim = RLENGTH+1;
    else
	verbatim = 1;
    gsub(/\s+::\s*$/, sprintf("\n\n%s::", substr(blanks, 1, verbatim-1)));
}

# Handle verbatim code sections. Pandoc handles these all right by itself, but
# we need to be aware of these so that we don't mess with them.
/^(\s*)\.\.\s+(code-block|sourcecode)::\s+.*/ || /^(\s*)::\s*$/ {
    if (match($0, /^(\s*)/))
	verbatim = RLENGTH+1;
    else
	verbatim = 1;
    print; next;
}

# Raw code sections are treated similarly, but we remove these by default.
/^(\s*)\.\.\s+raw::\s+.*/ || /^(\s*)::\s*$/ {
    if (match($0, /^(\s*)/))
	verbatim = RLENGTH+1;
    else
	verbatim = 1;
    if (raw == "no")
	skipped = 1;
    else
	print;
    next;
}

# Continuation lines of Sphinx decriptions (see below).
mode == 1 && /\s+.+/ {
    print "";
    printf("%s%s\n", def, gensub(/\s+(.+)/, "``\\1``", "g"));
    next;
}

mode == 1 {
    mode = 0;
}

# Special RST link targets. Pandoc doesn't seem to understand these either.
/^(\s*)__\s+.*/ {
    print gensub(/^(\s*)__\s+(.*)/, sprintf("\\1!hdefx(``id%d``)!``\\2``", counter++), "g");
    next;
}

/^(\s*)\.\.\s+_[^:]+:.*/ {
    print gensub(/^(\s*)..\s+_([^:]+):\s*(.*)/, "\\1!hdefx(``\\2``)!``\\3``", "g");
    print gensub(/^(\s*)..\s+_([^:]+):\s*(.*)/, "\\2", "g") >> tmpfile;
    next;
}

# Look for RST constructs which might be mistaken for Sphinx descriptions
# below; we simply pass these through to Pandoc instead.
/^(\s*)\.\.\s+[a-z:]+::\s+.*/ &&
! /^(\s*)\.\. ([a-z:]+:)?(program|option|function|macro|variable|constant|constructor|type|index)::/ {
    print; next;
}

# Program/options.
/^(\s*)\.\.\s+program::\s+.*/ {
    prog = gensub(/^(\s*)\.\.\s+program::\s+(.*)/, "-\\2", "g");
    next;
}

/^(\s*)\.\.\s+option::\s+.*/ {
    print gensub(/^(\s*)\.\.\s+option::\s+(.*)/, sprintf("\\1!opt(%s)!``\\2``", "cmdoption" prog), "g");
    def = gensub(/^(\s*)\.\.\s+option::\s+(.*)/, sprintf("\\1!opt(%s)!", "cmdoption" prog), "g");
    mode = 1;
    next;
}

# Other Sphinx descriptions (.. foo:: bar ...)
/^(\s*)\.\.\s+[a-z:]+::\s+.*/ {
    print gensub(/^(\s*)\.\.\s+([a-z:]+)::\s+(.*)/, "\\1!hdef(\\2)!``\\3``", "g");
    def = gensub(/^(\s*)\.\.\s+([a-z:]+)::\s+(.*)/, "\\1!hdef(\\2)!", "g");
    mode = 1;
    next;
}

# Sphinx cross references (:foo:`bar`)
/:[a-z:]+:`([^`]|\\`)+`/ {
    # Iterate over all matches, to fill in the proper link classes (:doc:,
    # :mod: and :opt: are handled for now).
    x = $0; $0 = "";
    while (match(x, /:([a-z:]+):`(([^`]|\\`)+)`/, matches)) {
	class = matches[1];
	text = matches[2];
	target = text;
	if (class == "doc")
	    target = "doc-" text;
	else if (class == "mod")
	    target = "module-" text;
	else if (class == "option")
	    target = "cmdoption" prog text;
	y = sprintf("!href(%s)!%s!end!", target, text);
	$0 = $0 substr(x, 1, RSTART-1) y;
	x = substr(x, RSTART+RLENGTH);
    }
    $0 = $0 x;
}

# RST links (`foo bar`_ and similar)

{
    # Full links on the current line.
    $0 = gensub(/`(([^`]|\\`)+)`__\>/, sprintf("!hrefx(id%d)!\\1!end!", counter), "g");
    $0 = gensub(/`(([^`]|\\`)+)`_\>/, "!href!\\1!end!", "g");
    # "Naked" links (without the backticks). We need to be careful here not to
    # match literals which happen to look like a link. XXXFIXME: This can't
    # really be done reliably without looking at an arbitrarily large amount
    # of context. Right now we just check that the pattern isn't surrounded by
    # backticks, so there may still be false positives.
    $0 = gensub(/(^|[^`])\<(\w+)__\>($|[^`])/, sprintf("\\1!hrefx(id%d)!\\2!end!\\3", counter), "g");
    $0 = gensub(/(^|[^`])\<(\w+)_\>($|[^`])/, "\\1!href!\\2!end!\\3", "g");
    # Incomplete link at the end of a line.
    if (match($0, /`(([^`]|\\`)+)$/, matches) && (RSTART==1||substr($0, RSTART-1, 1)!="`")) {
	# We need to peek ahead here so that we can be sure that this is
	# actually a link completed at the beginning of the next line. So we
	# output nothing yet and defer all further processing until the next
	# cycle.
	link_prefix = matches[1] " ";
	link_prev = $0;
	link_line = gensub(/`(([^`]|\\`)+)$/, "", "g");
	next;
    }
    print;
}

# end rst-pre.awk
