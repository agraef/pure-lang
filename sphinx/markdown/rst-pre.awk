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
    # These are just defaults, you should specify some real values on the
    # command line.
    if (!version) version = "@version@";
    if (!date) date = strftime("%B %d, %Y", systime());
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
    printf("%s%s\n", def, gensub(/[[:blank:]]+(.+)/, "``\\1``", "g", $0));
    next;
}

mode == 1 {
    mode = 0;
}

/\.\. note::/ { print; next; }

# Sphinx descriptions (.. foo:: bar ...)
/\.\.[[:blank:]]+[a-z:]+::[[:blank:]]+.*/ {
    print gensub(/\.\.[[:blank:]]+([a-z:]+)::[[:blank:]]+(.*)/, "!hdef(\\1)!``\\2``", "g", $0);
    def = gensub(/\.\.[[:blank:]]+([a-z:]+)::[[:blank:]]+(.*)/, "!hdef(\\1)!", "g", $0);
    mode = 1;
    next;
}

# Sphinx cross references (:foo:`bar`)
/:[a-z:]+:`[^`]+`/ {
    print gensub(/:([a-z:]+):`([^`]+)`/, "!href(\\2)!\\2!end!", "g", $0);
    next;
}

# Special link targets.
/^__\s+.*/ {
    print gensub(/^__\s+(.*)/, sprintf("!hdefx(``id%d``)!``\\1``", counter++), "g", $0);
    next;
}

/^..\s+_[^:]+:.*/ {
    print gensub(/^..\s+_([^:]+):\s*(.*)/, "!hdefx(``\\1``)!``\\2``", "g", $0);
    next;
}

# RST links (`foo bar`_)
/`[^`]+`__/ {
    print gensub(/`([^`]+)`__/, sprintf("!hrefx(id%d)!\\1!end!", counter), "g", $0);
    next;
}

/\<\S+__\>/ {
    print gensub(/\<(\S+)__\>/, sprintf("!hrefx(id%d)!\\1!end!", counter), "g", $0);
    next;
}

/`[^`]+`_/ {
    print gensub(/`([^`]+)`_/, "!href!\\1!end!", "g", $0);
    next;
}

/\<\S+_\>/ {
    print gensub(/\<(\S+)_\>/, "!href!\\1!end!", "g", $0);
    next;
}

# These are sometimes continued across line breaks.
/\<`[^`]+$/ {
    print gensub(/`([^`]+)$/, "!href!\\1", "g", $0);
    next;
}
/^[^`]+`_/ {
    print gensub(/^([^`]+)`_/, "\\1!end!", "g", $0);
    next;
}

{ print; }

# end rst-pre.awk
