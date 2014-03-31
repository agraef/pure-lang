# rst-post.awk

# This script requires GNU awk.

# This is the companion to the rst-pre.awk script which parses the special RST
# and Sphinx constructs recognized in the original RST document, and
# tries to translate them to the corresponding markdown syntax.

BEGIN {
    mode = 0; level = 0; prev = ""; header = "######"; saved_text = "";
    # These are just defaults, you can specify values for these on the command
    # line.
    if (!reference_links) reference_links = "no";
    if (!tmpfile) tmpfile = ".rst-markdown-targets";
    while ((getline line < tmpfile) > 0)
	targets[line] = 1
    close(tmpfile)
    system("rm -f " tmpfile);
}

# We get rid of escaped underscores in identifiers here. Pandoc/markdown is
# usually clever enough not to mistake these for emphasis.
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

# Rules to translate Sphinx definitions. These generally take a form like
# .. function:: fun args ... in the source, with the description of the item
# (function, variable, etc.) on subsequent lines, indented by three spaces.
# The preprocessor escapes them using the format !hdef($1)!`$2` where $1 is
# the item class (function, variable etc.), $2 the prototype (either fun args,
# var, or extern type fun(args...) for an external C function). The following
# rules turn these into sections with the appropriate Markdown link targets of
# the form {#...}, so that the described items can be linked to in the
# document text, as it is in the Sphinx-generated documentation. XXXFIXME:
# We'd rather use Markdown definition lists for these, but unfortunately items
# in these lists can't be link targets.

# If mode is 1, we're parsing a Sphinx definition, check whether it's
# terminated on the current line.
mode == 1 && !/^[[:space:]]*$/ && !/^>[[:space:]]*/ &&
!/^!hdef\([^)]+\)![[:space:]]+.*/ {
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
/^!hdef\([^)]+\)!`.*`/ {
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
    printf("%s {#%s}\n", gensub(/!hdef\(([^)]+)\)!(.*)/, hdr " \\2", "g"), target);
    mode = 1;
    next;
}

/^!opt\([^)]+\)!`.*`/ {
    if (match($0, /!opt\(([^)]+)\)!`(.*)`/, matches)) {
	opt = matches[1]; target = matches[2];
	if (match(target, /^\s*([+-]*\w+)/, m)) {
	    target = opt m[1];
	}
    }
    hdr = substr(header, 1, level+1);
    printf("%s {#%s}\n", gensub(/!opt\(([^)]+)\)!(.*)/, hdr " \\2", "g"), target);
    mode = 1;
    next;
}

/^!hdefx\([^)]+\)!.*/ {
    print gensub(/!hdefx\(`([^)]+)`\)!`(.*)`/, "[\\1]: \\2", "g");
    next;
}

# Recognize Sphinx cross references, turn them into Markdown reference links.
# XXXFIXME: In Sphinx, these may point to other documents, how to handle that
# here?
/!href\([^)]+\)![^!]+!end!/ {
    $0 = gensub(/!href\(([^)]+)\)!([^!]+)!end!/, "[`\\2`](#\\1)", "g");
}

/!hrefx\([^)]+\)![^!]+!end!/ {
    $0 = gensub(/!hrefx\(([^)]+)\)!([^!]+)!end!/, "[\\2][\\1]", "g");
}

# Finally the regular RST links. By default (unless the reference_links option
# is set), we turn these into inline links (using Pandoc's rules for
# generating a link target from the link text) so that they will hopefully
# work with Pandoc's auto-generated link targets for section headers, at least
# in Pandoc-generated html and pdf documents. Unfortunately, these won't work
# with 3rd party tools like "Marked" on the Mac. However, simple reference
# links of the form [link] do work in Marked, so we support them as an option
# as well, but note that these are unreliable in Pandoc because, in difference
# to RST, the auto-generated link targets are case-sensitive.

# Deal with an incomplete reference continued across a line break.
/^[^!]+!end!/ {
    if (match($0, /^([^!]+)!end!/, matches)) {
	text = saved_text matches[1];
	if (reference_links == "no" && !(text in targets)) {
	    target = text;
	    saved_text = "";
	    gsub(/\s+/, "-", target);
	    gsub(/[^[:alnum:]_.-]/, "", target);
	    target = tolower(target);
	    gsub(/^[^[:alpha:]]+/, "", target);
	    y = sprintf("[%s](#%s)", text, target);
	} else {
	    y = sprintf("[%s][]", text);
	}
	$0 = y substr($0, RSTART+RLENGTH);
    }
}

# Complete references on a line are dealt with here.
/!href![^!]+!end!/ {
    if (reference_links == "no") {
	# Iterate over all matches, so that we can generate the needed link
	# targets.
	x = $0; $0 = "";
	while (match(x, /!href!([^!]+)!end!/, matches)) {
	    text = matches[1];
	    if (!(text in targets)) {
		# Mangle the target name according to Pandoc's rules.
		target = text;
		gsub(/\s+/, "-", target);
		gsub(/[^[:alnum:]_.-]/, "", target);
		target = tolower(target);
		gsub(/^[^[:alpha:]]+/, "", target);
		y = sprintf("[%s](#%s)", text, target);
	    } else {
		y = sprintf("[%s][]", text);
	    }
	    $0 = $0 substr(x, 1, RSTART-1) y;
	    x = substr(x, RSTART+RLENGTH);
	}
	$0 = $0 x;
    } else {
	# Produce reference links.
	$0 = gensub(/!href!([^!]+)!end!/, "[\\1][]", "g");
    }
}

# Since we can't look ahead in the input, a trailing incomplete reference is
# saved for the next cycle.
/!href![^!]+$/ {
    if (match($0, /!href!([^!]+)$/, matches)) {
	saved_text = matches[1] " ";
	$0 = substr($0, 1, RSTART-1);
    }
}

# Finally output the modified line.
{ print; }

# end rst-post.awk
