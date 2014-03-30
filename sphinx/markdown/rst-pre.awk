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
    mode = 0; def = ""; counter = 0;
    # These are just defaults, you can specify values for these on the command
    # line.
    if (!version) version = "@version@";
    if (!date) date = strftime("%B %d, %Y", systime());
    if (!tmpfile) tmpfile = ".rst-markdown-targets";
    # Initialize the index file.
    system("rm -f " tmpfile);
}

# If the file didn't end with an empty line, we add one, just in case.
ENDFILE { if (prev) print ""; }

# Substitute version and date placeholders.
/@version@/ {
    gsub(/@version@/, version);
}

/\|today\|/ {
    gsub(/\|today\|/, date);
}

# pandoc doesn't understand this, so expand it to a proper code section.
/\s+::\s*$/ {
    gsub(/\s+::\s*$/, "\n\n::");
}

# Continuation lines of Sphinx decriptions (see below).
mode == 1 && /[[:blank:]]+.+/ {
    print "";
    printf("%s%s\n", def, gensub(/[[:blank:]]+(.+)/, "``\\1``", "g"));
    next;
}

mode == 1 {
    mode = 0;
}

# Special RST link targets. Pandoc doesn't seem to understand these either.
/^__\s+.*/ {
    print gensub(/^__\s+(.*)/, sprintf("!hdefx(``id%d``)!``\\1``", counter++), "g");
    next;
}

/^\.\.\s+_[^:]+:.*/ {
    print gensub(/^..\s+_([^:]+):\s*(.*)/, "!hdefx(``\\1``)!``\\2``", "g");
    print gensub(/^..\s+_([^:]+):\s*(.*)/, "\\1", "g") >> tmpfile;
    next;
}

# Look for RST constructs which might be mistaken for Sphinx descriptions
# below; we simply pass these through to Pandoc instead.
/\.\.[[:blank:]]+[a-z:]+::[[:blank:]]+.*/ &&
! /\.\. ([a-z:]+:)?(function|macro|variable|constant|constructor|type|index)::/ {
    print; next;
}

# Sphinx descriptions (.. foo:: bar ...)
/\.\.[[:blank:]]+[a-z:]+::[[:blank:]]+.*/ {
    print gensub(/\.\.[[:blank:]]+([a-z:]+)::[[:blank:]]+(.*)/, "!hdef(\\1)!``\\2``", "g");
    def = gensub(/\.\.[[:blank:]]+([a-z:]+)::[[:blank:]]+(.*)/, "!hdef(\\1)!", "g");
    mode = 1;
    next;
}

# Sphinx cross references (:foo:`bar`)
/:[a-z:]+:`[^`]+`/ {
    # Iterate over all matches, to fill in the proper link classes (:doc: and
    # :mod: are handled for now).
    x = $0; $0 = "";
    while (match(x, /:([a-z:]+):`([^`]+)`/, matches)) {
	class = matches[1];
	text = matches[2];
	target = text;
	if (class == "doc")
	    target = "doc-" text;
	else if (class == "mod")
	    target = "module-" text;
	y = sprintf("!href(%s)!%s!end!", target, text);
	$0 = $0 substr(x, 1, RSTART-1) y;
	x = substr(x, RSTART+RLENGTH);
    }
    $0 = $0 x;
}

# RST links (`foo bar`_ and similar)

# Deal with left-overs from the previous line.
/^[^`]+`_/ {
    $0 = gensub(/^([^`]+)`_/, "\\1!end!", "g");
}

# Complete links on the current line.
/`[^`]+`__/ {
    $0 = gensub(/`([^`]+)`__/, sprintf("!hrefx(id%d)!\\1!end!", counter), "g");
}

/\<[^[:space:]_]+__\>/ {
    $0 = gensub(/\<([^[:space:]_]+)__\>/, sprintf("!hrefx(id%d)!\\1!end!", counter), "g");
}

/`[^`]+`_/ {
    $0 = gensub(/`([^`]+)`_/, "!href!\\1!end!", "g");
}

/\<[^[:space:]_]+_\>/ {
    $0 = gensub(/\<([^[:space:]_]+)_\>/, "!href!\\1!end!", "g");
}

# These are continued across line breaks.
/\<`[^`]+$/ {
    $0 = gensub(/`([^`]+)$/, "!href!\\1", "g");
}

{ print; }

# end rst-pre.awk
