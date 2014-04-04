# rst-post.awk

# This script requires GNU awk.

# This is the companion to the rst-pre.awk script which parses the special RST
# and Sphinx constructs recognized in the original RST document, and
# tries to translate them to the corresponding markdown syntax.

# Helper function to mangle the target name according to Pandoc's rules.
function mangle(target) {
    gsub(/\s+/, "-", target);
    gsub(/[^[:alnum:]_.-]/, "", target);
    target = tolower(target);
    gsub(/^[^[:alpha:]]+/, "", target);
    gsub(/-+/, "-", target);
    return target;
}

# Minimal mangler to get a valid html link name.
function html_name(target) {
    gsub(/>/, "\\&gt;", target);
    return target;
}

# Expand an RST link in a link target.
function rst_link(target)
{
    if (match(target, /^!href!([^!]+)!end!$/, matches)) {
	text = matches[1];
	if (no_links == "yes")
	    return sprintf("'%s'", text);
	else if (!(text in targets)) {
	    target = mangle(text);
	    return sprintf("[%s](#%s)", text, target);
	} else {
	    return sprintf("[%s][]", text);
	}
    } else
	return target;
}

BEGIN {
    mode = 3; level = 0; prev = ""; header = "######"; saved_text = "";
    # These are just defaults, you can specify values for these on the command
    # line.
    if (!max_items) max_items = 0;
    if (!no_links) no_links = "no";
    if (!tmpfile) tmpfile = ".rst-markdown-targets";
    while ((getline line < tmpfile) > 0)
	targets[line] = 1
    close(tmpfile)
    system("rm -f " tmpfile);
}

# Process a generated pandoc-style header block at the beginning of the
# document.
mode == 3 && /^(```|~~~).*\.pandoc-title-block/ { mode = 2; next; }
mode == 2 && /^(```|~~~)/ { mode = 0; next; }
mode == 2 { print; next; }
mode == 3 { mode = 0; }

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
# The preprocessor escapes them using the format !hdef($1)!$2 where $1 is
# the item class (function, variable etc.), $2 the prototype (either fun args,
# var, or extern type fun(args...) for an external C function). The following
# rules turn these into Markdown definition lists by default. If you prefer,
# you can also generate proper section headers instead (set the headers option
# if you want this). In either case link targets will be created, so that the
# described items can be linked to in the document text. (In the case of
# definition lists, this is done with inline html, as there is no special
# Markdown syntax for link anchors in definition list items.)

# If mode is 1, we're parsing a Sphinx definition, check whether it's
# terminated on the current line and do some preprocessing of the headers
# along the way.
mode == 1 && !/^\s*$/ && !/^\s*>\s*/ {
    if (match($0, /^>?\s*!hdef\(([^)]+)\)!(.*)/, matches)) {
	class = matches[1]; target = matches[2];
	gsub(/^(\w+:)+/, "", class);
	if (headers != "yes" && last_class == class &&
	    num_items < max_items &&
	    (class == "constant" || class == "constructor" ||
	     class == "type" || class == "macro" || class == "function" &&
	     !match(target, /^(public|private)?\s*extern\s+/) ||
	     class == "variable" || class == "envvar") &&
	    num_items < max_items) {
	    # This collapses adjacent headers for some item classes. Looks
	    # nicer IMHO. TODO: Maybe this should be an option.
	    if (buffer) buffer = buffer ", ";
	    num_items++;
	} else {
	    if (buffer) print buffer; buffer = "";
	    if (indent) print indent;
	    num_items = 1;
	}
    } else if (match($0, /^\s*!opt\([^)]+\)!.*/)) {
	if (num_items < max_items) {
	    if (buffer) buffer = buffer ", ";
	    num_items++;
	} else {
	    if (buffer) print buffer; buffer = "";
	    if (indent) print indent;
	    num_items = 1;
	}
    } else {
	if (buffer) print buffer;
	if (indent) print indent;
	mode = 0; indent = buffer = last_class = "";
	num_items = 0;
    }
}

# Get rid of a lone ">" from a block quote in the description text.
mode == 1 && /^\s*>\s*$/ {
    print "";
    next;
}

# pandoc will treat the text of a description as a block quote, turn it back
# into ordinary text.
mode == 1 && /^\s*> / {
    if (buffer) print buffer; buffer = "";
    num_items = 0;
    gsub(/^\s*> /, "");
    if (indent) {
	$0 = indent $0;
	gsub(/\S/, " ", indent);
    }
}

# Recognize Sphinx definitions, turn them into Markdown headers or descriptions.
# Note that program options are dealt with in a separate rule below.
/^>?\s*!hdef\([^)]+\)!.*/ {
    # Parse the name of the object, so that we can create the appropriate link
    # target.
    if (match($0, /^(>?\s*)!hdef\(([^)]+)\)!(.*)/, matches)) {
	indent = matches[1]; class = matches[2]; target = matches[3];
	gsub(/^(\w+:)+/, "", class);
	target = gensub(/\\(.)/, "\\1", "g", target);
	text = target;
	# For clarity, some item classes have a descriptive label shown before
	# the item header.
	if (buffer && last_class == class)
	    label = "";
	else if (class == "constant" || class == "variable" ||
		 class == "macro" || class == "type")
	    label = "*" class "* ";
	else
	    label = "";
	# XXXFIXME: This is highly domain-specific, so surely needs
	# adjustments for domains other than Pure. The syntax recognized for
	# functions and similar items is that of Pure, as it is written in the
	# Pure docs (basically extern declarations and Pure function headers).
	if (class == "envvar")
	    target = "envvar-" target;
	else if (class == "describe")
	    target = "";
	else if (match(target, /^(public|private)?\s*extern\s+\w+(\*|\s)+(\w+)/, m))
	    target = m[3];
	else if (match(target, /^outfix\s+(\S+)\s+(\S+)(\s+(\/\w+)?)(.*)/, m)) {
	    leftop = m[1]; rightop = m[2]; tag = m[4]; args = m[5];
	    if (namespace && index(leftop, "::") == 0) leftop = namespace "::" leftop;
	    if (namespace && index(rightop, "::") == 0) rightop = namespace "::" rightop;
	    gsub(/^\s+/, "", args);
	    target = leftop tag;
	    text = leftop " " args " " rightop;
	} else if (match(target, /^((infix[lr]?|prefix|postfix|nonfix)\s+)?(\S+)(\s+(\/\w+)?)(.*)/, m)) {
	    decl = m[2]; op = m[3]; tag = m[5]; args = m[6];
	    if (namespace && index(op, "::") == 0) op = namespace "::" op;
	    gsub(/^\s+/, "", args);
	    target = op tag;
	    # Handle operator descriptions (bring the operands in the right
	    # order, depending on the kind of operator).
	    if (decl == "infix" || decl == "infixl" || decl == "infixr") {
		if (match(args, /(\S+)\s+(.*)/, m))
		    text = m[1] " " op " " m[2];
		else
		    text = op " " args;
	    } else if (decl == "postfix") {
		text = args " " op;
	    } else {
		text = op " " args;
	    }
	}
    }
    target = html_name(target);
    if (headers == "yes") {
	hdr = substr(header, 1, level+1);
	if (target)
	    printf("%s {#%s}\n", hdr " " label "`" text "`", target);
	else
	    printf("%s\n", hdr " " label "`" text "`");
	indent = buffer = last_class = "";
	num_items = 0;
    } else {
	# For definition list items there isn't any special syntax to create
	# the link target, so we do it in html. Also note that we defer output
	# of the item header since adjacent headers for some item classes are
	# collapsed to a single item (see above).
	if (target)
	    buffer = buffer sprintf("%s<a name=\"%s\"></a>%s", indent, target,
				    label "`" text "`");
	else
	    buffer = buffer sprintf("%s%s", indent,
				    label "`" text "`");
	indent = indent ":   "; last_class = class;
    }
    mode = 1;
    next;
}

/^>?\s*!opt\([^)]+\)!.*/ {
    if (match($0, /^(>?\s*)!opt\(([^)]+)\)!(.*)/, matches)) {
	indent = matches[1]; opt = matches[2]; target = matches[3];
	if (match(target, /^\s*([+-]*[^\[=[:space:]]+)/, m)) {
	    target = opt m[1];
	}
    }
    target = html_name(target);
    if (headers == "yes") {
	hdr = substr(header, 1, level+1);
	printf("%s {#%s}\n", gensub(/!opt\(([^)]+)\)!(.*)/, hdr " `\\2`", "g"), target);
	indent = buffer = last_class = "";
	num_items = 0;
    } else {
	buffer = buffer sprintf("%s<a name=\"%s\"></a>%s", indent, target, gensub(/!opt\(([^)]+)\)!(.*)/, "`\\2`", "g"));
	indent = indent ":   ";
    }
    last_class = "";
    mode = 1;
    next;
}

# Short option lists. We render these as definition lists.
/^>?\s*!optx\(`[^)]+`\)!.*/ {
    if (match($0, /^(>?\s*)!optx\((`[^)]+`)\)!(.*)/, matches)) {
	spc = matches[1]; opt = matches[2]; descr = matches[3];
	print sprintf("%s%s\n%s  : %s", spc, opt, spc, descr);
    }
    next;
}

/^>?\s*!hdefx\(`[^)]+`\)!.*/ {
    if (match($0, /^(>?\s*)!hdefx\(`([^)]+)`\)!(.*)/, matches)) {
	spc = matches[1]; target = matches[2]; link = matches[3];
	if (link) {
	    #link = rst_link(link);
	    print sprintf("%s[%s]: %s", spc, target, link);
	} else {
	    # Internal link target, create it on the spot (the only way to do
	    # that in Markdown is with inline html):
	    link = mangle(target);
	    print sprintf("%s[%s]: #%s", spc, target, link);
	    print sprintf("\n%s<a name=\"%s\"></a>", spc, link);
	}
    }
    next;
}

/^>?\s*!hdefns\(`[^)]+`\)!$/ {
    if (match($0, /^>?\s*!hdefns\(`([^)]+)`\)!/, matches))
	namespace = matches[1];
    next;
}

# Recognize Sphinx cross references, turn them into Markdown reference links.
# XXXFIXME: In Sphinx, these may point to other documents. To properly resolve
# such links, we'd need to have some kind of indexing facility which can be
# applied either here or in the rule in rst-pre.awk which generates the links.
/!href\(`(([^`]|\\`)+)`\)!([^!]|\\!)+!end!/ {
    x = $0; $0 = "";
    while (match(x, /!href\(`(([^`]|\\`)+)`\)!(([^!]|\\!)+)!end!/, matches)) {
	link = matches[1]; text = matches[3];
	text = gensub(/\\(.)/, "\\1", "g", text);
	if (no_links == "yes")
	    y = sprintf("'%s'", text);
	else
	    y = sprintf("[%s](%s)", text, link);
	$0 = $0 substr(x, 1, RSTART-1) y;
	x = substr(x, RSTART+RLENGTH);
    }
    $0 = $0 x;
}

/!hrefx\([^)]+\)![^!]+!end!/ {
    if (no_links == "yes")
	$0 = gensub(/!hrefx\(([^)]+)\)!([^!]+)!end!/, "'\\2'", "g");
    else
	$0 = gensub(/!hrefx\(([^)]+)\)!([^!]+)!end!/, "[\\2][\\1]", "g");
}

# Finally the regular RST links. We turn these into inline links (using
# Pandoc's rules for generating a link target from the link text) so that they
# will work with Pandoc's auto-generated link targets for section headers (at
# least in Pandoc-generated html and pdf documents).

# Deal with an incomplete reference continued across a line break.
/^[^!]+!end!/ {
    if (match($0, /^([^!]+)!end!/, matches)) {
	text = saved_text matches[1];
	saved_text = "";
	if (no_links == "yes")
	    y = sprintf("'%s'", text);
	else {
	    target = mangle(text);
	    y = sprintf("[%s](#%s)", text, target);
	}
	$0 = y substr($0, RSTART+RLENGTH);
    }
}

# Complete references on a line are dealt with here.
/!href![^!]+!end!/ {
    # Iterate over all matches, so that we can generate the needed link
    # targets.
    x = $0; $0 = "";
    while (match(x, /!href!([^!]+)!end!/, matches)) {
	text = matches[1];
	if (no_links == "yes")
	    y = sprintf("'%s'", text);
	else if (!(text in targets)) {
	    target = mangle(text);
	    y = sprintf("[%s](#%s)", text, target);
	} else {
	    y = sprintf("[%s][]", text);
	}
	$0 = $0 substr(x, 1, RSTART-1) y;
	x = substr(x, RSTART+RLENGTH);
    }
    $0 = $0 x;
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
