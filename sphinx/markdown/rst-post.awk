# rst-post.awk

# This script requires GNU awk.

# This is the companion to the rst-pre.awk script which parses the special RST
# and Sphinx constructs recognized in the original RST document, and
# tries to translate them to the corresponding markdown syntax.

BEGIN {
    mode = 0; level = 0; prev = ""; header = "######";
}

# We get rid of escaped underscores in identifiers here. Pandoc/markdown is
# unlikely to mistake these for emphasis anyway.
/\\_/ { gsub(/\\_/, "_"); }

# Keep track of Markdown headers. We need to know at which header level we
# currently are so that we can assign a suitable level below that to Sphinx
# definitions.
/^==[=]+\s*$/ && !prev { level = 1; }
/^--[-]+\s*$/ && !prev { level = 2; }
/^#+/ && !prev { level = length(gensub(/^(#+).*/, "\\1", "g")); }

# Keep track of the previous line (trimming whitespace), so that we can detect
# header lines.
{ prev = gensub(/^\s*/, "", "g"); }

# If mode is 1, we're parsing a Sphinx definition, check whether it's
# terminated on the current line.
mode == 1 && !/^[[:space:]]*$/ && !/^>[[:space:]]*/ &&
!/!hdef\([^)]+\)![[:space:]]+.*/ {
    mode = 0;
}

# pandoc will treat the text of a description as a block quote, turn it back
# into ordinary text.
mode == 1 && /^> / {
    gsub(/^> /, "");
}

# Get rid of a lone ">" from a block quote in the description text.
mode == 1 && /^>[[:space:]]*$/ {
    print ""; next;
}

# Recognize Sphinx definitions, turn them into Markdown headers.
/!hdef\([^)]+\)!`.*`/ {
    # Parse the name of the object, so that we can create the appropriate link
    # target.
    if (match($0, /!hdef\(([^)]+)\)!`(.*)`/, matches)) {
	target = matches[2];
	if (match(target, /^(public|private)?\s*extern\s+\w+(\*|\s)+(\w+)/, m)) {
	    target = m[3];
	} else if (match(target, /^(infix[lr]?|prefix|postfix|nonfix)\s*\s+(\S+)\s+((\/\w+)?)/, m)) {
	    target = m[2] m[3];
	}
    }
    hdr = substr(header, 1, level+1);
    printf("%s {#%s}\n", gensub(/!hdef\(([^)]+)\)!(.*)/, hdr " \\2", "g", $0), target);
    mode = 1;
    next;
}

/!hdefx\([^)]+\)!.*/ {
    print gensub(/!hdefx\(`([^)]+)`\)!`(.*)`/, "[\\1]: \\2", "g", $0);
    next;
}

# Recognize Sphinx cross references, turn them into Markdown reference links.
# XXXFIXME: In Sphinx, these may point to other documents, how to handle that
# here?
/!href\([^)]+\)![^!]+!end!/ {
    print gensub(/!href\(([^)]+)\)!([^!]+)!end!/, "[\\2](#\\1)", "g", $0);
    next;
}

/!hrefx\([^)]+\)![^!]+!end!/ {
    print gensub(/!hrefx\(([^)]+)\)!([^!]+)!end!/, "[\\2][\\1]", "g", $0);
    next;
}

# Recognize a RST link, turn it into a Markdown reference link.
{
    gsub(/!href!/, "[");
    gsub(/!end!/, "]");
    print;
}

# end rst-post.awk
