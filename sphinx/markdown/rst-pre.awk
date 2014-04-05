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

# Helper function to mangle the target name according to Pandoc's rules.
function mangle(target) {
    gsub(/\s+/, "-", target);
    gsub(/[^[:alnum:]_.-]/, "", target);
    target = tolower(target);
    gsub(/^[^[:alpha:]]+/, "", target);
    gsub(/-+/, "-", target);
    return target;
}

# Helper function to format a recursive RST link inside a local RST link
# target such as `.. _target: link_`. This kind of indirection doesn't work in
# Markdown, however, so we just mangle the link, interpreting it as an html
# link, and hope for the best.
function rst_link(target)
{
    if (match(target, /^`(.+)`_$/, matches) ||
	match(target, /^(.+)_$/, matches))
	return "#" mangle(matches[1]);
    else
	return target;
}

# Helper function to format a Sphinx cross-reference (or RST text role) of
# the form :class:`text`, filling in the proper link target and text for the
# given class. The classes :doc:, :mod:, :opt:, :envvar:, :program: and :ref:
# are given special treatment for now, as well as various common RST text
# roles. Other links are assumed to point to descriptions of functions,
# variables, etc. XXXFIXME: Apart from :doc:, the following code produces
# local links only, which of course won't work for cross-document links. Some
# kind of indexing utility would be needed to properly resolve the links and
# fill in the appropriate target documents, but this hasn't been written yet.
function rst_role(class, text)
{
    # Handle RST text roles.
    if (!class) class = default_role;
    if (!class) class = "emphasis";
    if (class in roles) {
	while (class in roles && roles[class] != 1)
	    class = roles[class];
	if (roles[class] == 1) {
	    # Built-in role. We expand a few roles here, so that we don't have
	    # to rely on Pandoc to do the right thing with them.
	    if (class == "literal" || class == "code")
		return "``" text "``";
	    else if (class == "emphasis")
		return "*" text "*";
	    else if (class == "strong")
		return "**" text "**";
	    else
		# Anything else goes right through to Pandoc.
		return ":" class ":`" text "`";
	} else {
	    # We couldn't resolve the role. Let's just pass the darn thing to
	    # Pandoc and hope it can handle it (most likely it will just
	    # ignore it anyway).
	    return ":" class ":`" text "`";
	}
    }
    # Sphinx allows a link text to be specified explicitly using the syntax
    # text<name>.
    i = index(text, "<"); l = length(text);
    if (i > 0 && substr(text, l) == ">") {
	target = substr(text, i+1, l-i-1); text = substr(text, 1, i-1);
	gsub(/\s+$/, "", text);
    } else
	target = text;
    if (class == "doc")
	target = target ".html";
    else if (class == "mod")
	target = "#module-" target;
    else if (class == "ref")
	target = "#" mangle(target);
    else if (class == "option") {
	i = index(target, " ");
	if (i > 0) {
	    # program is given explicitly
	    myprog = substr(target, 1, i-1);
	    myopt = substr(target, i+1);
	    gsub(/^\s+/, "", myopt);
	    target = "#cmdoption-" myprog myopt;
	} else
	    target = "#cmdoption" prog target;
	text = "``" text "``";
    } else if (class == "envvar") {
	target = "#envvar-" target;
	text = "``" text "``";
    } else {
	# Anything else probably denotes a function, variable or similar
	# description item. In the case of Pure domain, these can have a tag
	# of the from `/tag` attached to them in order to distinguish
	# overloaded instances of operations. Please note that this part is
	# somewhat domain-specific and might need to be adjusted for other
	# Sphinx domains.
	i = index(text, "/");
	if (i > 1) {
	    # Tagged link, remove tag from link text.
	    text = substr(text, 1, i-1);
	}
	if (namespace && index(target, "::") == 0) {
	    # Unqualified target name, add the namespace if set.
	    target = namespace "::" target;
	} else if (index(target, "::") > 0 && index(target, "~") == 1) {
	    # Leading ~ => namespace is suppressed in display.
	    gsub(/^~/, "", target);
	    gsub(/^~.*::/, "", text);
	}
	gsub(/^::/, "", target);
	target = "#" target;
	text = "``" text "``";
    }
    if (target)
	return sprintf("!href(``%s``)!%s!end!", target, text);
    else
	return text;
}

BEGIN {
    mode = 0; verbatim = 0; skipped = 0; def = ""; counter = 0; prog = "";
    blanks = "                                                             ";
    # These are just defaults, you can specify values for these on the command
    # line.
    if (!version) version = "@version@";
    if (!date) date = strftime("%B %d, %Y", systime());
    if (!title_block) title_block = "no";
    if (!tmpfile) tmpfile = ".rst-markdown-targets";
    if (!raw) raw = "no";
    if (!callouts) callouts = "no";
    # Initialize the index file.
    system("rm -f " tmpfile);
    if (title_block == "yes") mode = 2;
    # RST text roles we know about. This is from the RST documentation and
    # should cover the built-in roles. User-defined roles are added as we
    # parse the document.
    roles["emphasis"] = 1;
    roles["literal"] = 1;
    roles["code"] = 1;
    roles["math"] = 1;
    roles["pep-reference"] = roles["PEP"] = 1;
    roles["rfc-reference"] = roles["RFC"] = 1;
    roles["strong"] = 1;
    roles["subscript"] = roles["sub"] = 1;
    roles["superscript"] = roles["sup"] = 1;
    roles["title-reference"] = roles["title"] = roles["t"] = 1;
    roles["raw"] = 1;
    # Sphinx also defines some roles of its own. For now we only support those
    # which are commonly used in the Pure docs, and mostly alias them to the
    # equivalent built-in roles.
    roles["command"] = "literal";
    roles["dfn"] = "emphasis";
    roles["file"] = "literal";
    roles["guilabel"] = "literal";
    roles["kbd"] = "literal";
    roles["program"] = "strong";
    roles["samp"] = "literal";
    # The default role which is used if no role is specified explicitly.
    # This can be set in the document.
    default_role = "emphasis";
}

ENDFILE {
    # If the file didn't end with an empty line, we add one, just in case.
    if (prev) print "";
    # We also reset the mode, namespace and various other state variables, so
    # that the next file starts from a clean slate.
    verbatim = productionlist = skipped = quote = 0;
    link_prefix = link_line = link_prev = "";
    if (namespace) print "!hdefns()!\n";
    namespace = prog = "";
    mode = 0;
}

# Keep track of the previous line.
{ prev = current; current = $0; }

# Scrape the title block, if requested. This stops as soon as we have
# processed the title block. Any extra frontmatter is discarded.

# This marks the beginning of the title block.
mode == 2 && /^\s*\.\.\s*(%.*)$/ { print ".. code-block:: pandoc-title-block\n"; mode = 3; }
# Get rid of extra frontmatter.
mode == 2 { next; }
# Scrape the title block.
mode == 3 && /^\s*\.\.\s*(.*)$/ {
    gsub(/@version@/, version);
    gsub(/\|today\|/, date);
    print gensub(/^\s*\.\.\s*(.*)$/, "   \\1", "g");
    next;
}
# The title block stops at the first empty line or anything else which doesn't
# look like a RST directive.
mode == 3 { mode = 0; }

# Processing of the document body starts here.

# Nothing gets expanded during a verbatim code section (see below).
verbatim > 0 {
    if (match($0, /^(\s*)/))
	indent = RLENGTH+1;
    else
	indent = 1;
    if (indent <= verbatim && !match($0, /^\s*$/))
	verbatim = productionlist = skipped = 0;
    if (verbatim > 0) {
	if (productionlist > 0) {
	    gsub(/`/, "");
	    gsub(/^\s+: \|/, substr(blanks, 1, verbatim-1) "       |");
	    gsub(/^\s+:/, substr(blanks, 1, verbatim-1) "         ");
	}
	if (skipped == 0) print;
	next;
    }
}

# Handle quote blocks such as notes (see below).
quote > 0 {
    if (match($0, /^(\s*)/))
	indent = RLENGTH+1;
    else
	indent = 1;
    if (indent <= quote && !match($0, /^\s*$/)) {
	print substr(blanks, 1, quote-3) "-----\n";
	quote = 0;
    }
}

# Deal with left-overs from the previous line.
link_prefix {
    if (match(link_prefix, /^:([a-z:]+)+:/) &&
	match($0, /^(\s*)(([^`]|\\`)+`)(.*)/, matches)) {
	spc = matches[1]; link_suffix = matches[2]; rest = matches[4];
	x = link_prefix link_suffix;
	if (match(x, /:([a-z:]+):`(([^`]|\\`)+)`/, matches)) {
	    print link_line;
	    class = matches[1]; text = matches[2];
	    y = rst_role(class, text);
	    $0 = spc y rest;
	} else
	    print link_prev;
    } else if (match($0, /^(\s*)(([^`]|\\`)+)`__\>/, matches)) {
	print link_line;
	gsub(/^(\s*)(([^`]|\\`)+)`__\>/,
	     sprintf("%s!hrefx(id%d)!%s%s!end!", matches[1], counter, link_prefix, matches[2]));
    } else if (match($0, /^(\s*)(([^`]|\\`)+)`_\>/, matches)) {
	print link_line;
	gsub(/^(\s*)(([^`]|\\`)+)`_\>/,
	     sprintf("%s!href!%s%s!end!", matches[1], link_prefix, matches[2]));
    } else if (match($0, /^(\s*)(([^`]|\\`)+)`($|\s|[.!?,;:])/, matches)) {
	print link_line;
	gsub(/^(\s*)(([^`]|\\`)+)`/,
	     sprintf("%s%s", matches[1], rst_role(default_role, link_prefix matches[2])));
    } else {
	print link_prev;
    }
    link_prefix = link_line = link_prev = "";
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
/\s+::\s*$/ && !/^(\s*)\.\.\s/ {
    if (match($0, /^(\s*)/))
	verbatim = RLENGTH+1;
    else
	verbatim = 1;
    gsub(/\s+::\s*$/, sprintf("\n\n%s::", substr(blanks, 1, verbatim-1)));
}

# Handle verbatim code sections. Pandoc handles these all right by itself, but
# we need to be aware of these so that we don't mess with them.
/^(\s*)\.\.\s+(code-block|sourcecode)::.*/ ||
/::\s*$/ && !/^\s*\.\.\s/ {
    if (match($0, /^(\s*)/))
	verbatim = RLENGTH+1;
    else
	verbatim = 1;
    if (match($0, /^\s*(::\s*|\.\.\s+(code-block|sourcecode)::.*)$/)) {
	print; next;
    }
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

# XXXFIXME: Not really sure what to do with these; rendered as a code block
# for now.
/^(\s*)\.\.\s+productionlist::.*/ {
    if (match($0, /^(\s*)/))
	verbatim = RLENGTH+1;
    else
	verbatim = 1;
    productionlist = 1;
    print gensub(/^(\s*)\.\.\s+productionlist::(.*)/, "\\1.. code-block:: bnf\n", "g");
    next;
}

# Continuation lines of Sphinx descriptions (see below).
mode == 1 && /^\s+\S.*/ {
    printf("\n%s%s\n", def, gensub(/\s+(.+)/, "``\\1``", "g"));
    next;
}

mode == 1 {
    mode = 0; if (match($0, /\S/)) print "";
}

# RST substitutions. Pandoc handles these, but we need to keep track of them
# to make substituted targets work correctly (see below).
/^\s*\.\.\s+\|[^|]+\|\s*replace::\s*.*/ {
    if (match($0, /^\s*\.\.\s+\|([^|]+)\|\s*replace::\s*(.*)/, matches)) {
	name = matches[1]; repl = matches[2];
	sub(/\s*$/, "", repl);
	replacement[name] = repl;
    }
    print; next;
}

# Special RST link targets. Pandoc doesn't seem to understand these, so we
# produce explicit link targets for them. We also record the targets in a
# temporary index file so that links to these targets can be resolved
# correctly.
/^(\s*)__\s+.*/ {
    if (match($0, /^(\s*)__\s+(.*)/, matches)) {
	spc = matches[1]; link = matches[2];
	# The given link might actually be an RST link instead of a real URL,
	# expand that if needed.
	link = rst_link(link);
	print sprintf("\n%s!hdefx(``id%d``)!%s", spc, counter++, link);
    }
    next;
}

# XXXFIXME: This requires that the link is on the same line, apparently RST
# also allows it to be on the next line if it's properly indented.
/^(\s*)\.\.\s+_[^:]+:.*/ {
    if (match($0, /^(\s*)\.\.\s+_([^:]+):\s*(.*)/, matches)) {
	spc = matches[1]; name = matches[2]; link = matches[3];
	gsub(/^(`|\s)+/, "", name);
	gsub(/(`|\s)+$/, "", name);
	if (name in replacement) name = replacement[name];
	# The given link might actually be an RST link instead of a real URL,
	# expand that if needed.
	link = rst_link(link);
	print sprintf("\n%s!hdefx(``%s``)!%s", spc, name, link);
	print name >> tmpfile;
    }
    next;
}

# Likewise for Sphinx module markup.
/^(\s*)\.\.\s+module::.*/ {
    if (match($0, /^(\s*)\.\.\s+module::\s*(.*)/, matches)) {
	spc = matches[1]; name = matches[2];
	name = "module-" name;
	print sprintf("\n%s!hdefx(``%s``)!", spc, name);
	print name >> tmpfile;
    }
    next;
}

# Keep track of namespaces in Sphinx markup.
/^(\s*)\.\.\s+namespace::.*/ {
    if (match($0, /^(\s*)\.\.\s+namespace::\s*(.*)/, matches)) {
	spc = matches[1]; namespace = matches[2];
	if (namespace == "None") namespace = "";
	gsub(/^::/, "", namespace);
	gsub(/\s*$/, "", namespace);
	print sprintf("\n%s!hdefns(%s)!", spc, namespace);
    }
    next;
}

# Keep track of text roles. We also pass these directives through to Pandoc,
# just in case. However, Pandoc doesn't seem to handle most text roles
# understood by RST and Sphinx, so we do our own handling of those.
/^(\s*)\.\.\s+default-role::.*/ {
    if (match($0, /^\s*\.\.\s+default-role::\s*(.*)/, matches)) {
	default_role = matches[1];
    }
    print; next;
}

/^(\s*)\.\.\s+role::.*/ {
    if (match($0, /^\s*\.\.\s+role::\s*(\w+)\((\w+)\)\s*$/, matches)) {
	name = matches[1]; role = matches[2];
	while (role in roles && roles[role] != 1)
	    role = roles[role];
	roles[name] = role;
    }
    print; next;
}

# Notes aren't rendered very nicely by pandoc, fix them up a bit if requested.
callouts == "yes" && /^(\s*)\.\.\s+(note|NOTE)::.*/ {
    if (match($0, /^(\s*)\.\.\s+/))
	quote = RLENGTH;
    else
	quote = 1;
    $0 = gensub(/^(\s*)\.\.\s+(note|NOTE)::\s*/, "\n\\1-----\n\n\\1   **Note:** ", "g");
}

# Field values. Pandoc will render these in a generic fashion, but we handle
# some common cases here to make them look a little nicer.

# This is associated with the module markup.
/^(\s*):platform:.*/ {
    print gensub(/^(\s*):platform:(.*)/, "\n*Platforms:* \\2", "g");
    next;
}

# Parameter descriptions, associated with the function markup.
/^(\s*):param\s+[^:]+:.*/ {
    print gensub(/^(\s*):param\s+([^:]+):(.*)/, "\\1:\\2: \\3", "g");
    next;
}

# RST short option lists. These actually look an awful lot like ordinary text,
# so we need to be *very* specific about the syntax here.
/^(\s*)(-[a-zA-Z]( ?[a-zA-Z0-9_-]+| ?<[^>]+>)?|--[a-zA-Z0-9_-]+([ =][a-zA-Z0-9_-]+|[ =]<[^>]+>)?)(, (-[a-zA-Z]( ?[a-zA-Z0-9_-]+| ?<[^>]+>)?|--[a-zA-Z0-9_-]+([ =][a-zA-Z0-9_-]+|[ =]<[^>]+>)?))*  .*/ && !/^(\s*)---/ {
    $0 = gensub(/^(\s*)(\S+( \S+)*)  \s*(.*)/, "\n\\1!optx(``\\2``)!``\\4``", "g");
}

/^(\s*)(-[a-zA-Z]( ?[a-zA-Z0-9_-]+| ?<[^>]+>)?|--[a-zA-Z0-9_-]+([ =][a-zA-Z0-9_-]+|[ =]<[^>]+>)?)(, (-[a-zA-Z]( ?[a-zA-Z0-9_-]+| ?<[^>]+>)?|--[a-zA-Z0-9_-]+([ =][a-zA-Z0-9_-]+|[ =]<[^>]+>)?))*\s*$/ && !/^(\s*)---/ {
    $0 = gensub(/^(\s*)(\S+( \S+)*)\s*$/, "\n\\1``\\2``", "g");
}

# Look for RST constructs which might be mistaken for Sphinx descriptions
# below; we simply pass these through to Pandoc instead.
/^(\s*)\.\.\s+[a-z:]+::\s+.*/ &&
! /^(\s*)\.\. ([a-z:]+:)?(program|option|envvar|function|macro|variable|constant|constructor|type|describe|index)::/ {
    print; next;
}

# Index entries. Not sure what to do with these, they're just ignored for now.
/^(\s*)\.\.\s+index::\s+.*/ { next; }

# Program/options. We need to keep track of the program names here, so that
# the proper links for option descriptions can be constructed.
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

# RST text roles and Sphinx cross references (:foo:`bar` or just `bar`).
/:[a-z:]+:`([^`]|\\`)+`/ {
    # Iterate over all matches, to fill in the proper link targets and texts
    # for the corresponding classes (see rst_role()).
    x = $0; $0 = "";
    while (match(x, /:([a-z:]+):`(([^`]|\\`)+)`/, matches)) {
	class = matches[1]; text = matches[2];
	y = rst_role(class, text);
	$0 = $0 substr(x, 1, RSTART-1) y;
	x = substr(x, RSTART+RLENGTH);
    }
    $0 = $0 x;
}

/`([^`]|\\`)+`/ {
    x = $0; $0 = "";
    while (match(x, /(^|[^`_[:alnum:]])`(([^`]|\\`)+)`($|[^`_[:alnum:]])/, matches)) {
	ldelim = matches[1]; text = matches[2]; rdelim = matches[4];
	y = rst_role(default_role, text);
	$0 = $0 substr(x, 1, RSTART-1+length(ldelim)) y;
	x = substr(x, RSTART+RLENGTH-length(rdelim));
    }
    $0 = $0 x;
}

# RST links (`foo bar`_ and similar)
{
    # Full links on the current line.
    $0 = gensub(/`(([^`]|\\`)+)`__\>/, sprintf("!hrefx(id%d)!\\1!end!", counter), "g");
    $0 = gensub(/`(([^`]|\\`)+)`_\>/, "!href!\\1!end!", "g");
    $0 = gensub(/(\|[^|]+\|)_\>/, "!href!\\1!end!", "g");
    # "Naked" links (without the backticks). We need to be careful here not to
    # match literals which happen to look like a link. XXXFIXME: This can't
    # really be done reliably without looking at an arbitrarily large amount
    # of context. To be on the safe side, we look for a word (alphanumeric
    # characters only) surrounded by whitespace or a few selected
    # delimiters. Hopefully this won't give too many false positives.
    $0 = gensub(/(^|\s)\<([[:alnum:]-]+)__\>($|\s|[),;:.!?])/, sprintf("\\1!hrefx(id%d)!\\2!end!\\3", counter), "g");
    $0 = gensub(/(^|\s)\<([[:alnum:]-]+)_\>($|\s|[),;:.!?])/, "\\1!href!\\2!end!\\3", "g");
    # Incomplete (Sphinx or RST) link or text role at the end of a line.
    if (match($0, /(:[a-z:]+:`([^`]|\\`)+)$/, matches) ||
	match($0, /`(([^`]|\\`)+)$/, matches) &&
	(RSTART==1||!match(substr($0, RSTART-1, 1), /[`[:alnum:]]/)) &&
	!match(substr(matches[1], 1, 1), /\s|[_,;:.!?)]/)) {
	# We need to peek ahead here so that we can be sure that this is
	# actually a link completed at the beginning of the next line. So we
	# just record the state of the match, output nothing yet and defer all
	# further processing until the next cycle.
	link_prefix = matches[1] " ";
	link_prev = $0;
	link_line = gensub(/(:[a-z:]+:)?`(([^`]|\\`)+)$/, "", "g");
	next;
    }
    print;
}

# end rst-pre.awk
