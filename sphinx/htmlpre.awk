BEGIN { mode = 0; lastid = ""; }

# We get rid of a Sphinx-generated toc here.
/^<div class="contents local topic" id="contents">$/ {
    mode = 1; next;
}
/^<\/div>$/ {
    mode = 0; next;
}

# Make sure that we don't loose the line breaks in block-formatted input.
/<div class="line">.*<\/div>/ {
    if (/<span class="pre">[^<]*<\/span>/) {
	gsub(/<span class="pre">/, "<code>");
	gsub(/<\/span>/, "</code>");
    }
    print $0 "<p>";
    next;
}
/<\/blockquote>/ {
    if (/<span class="pre">[^<]*<\/span>/) {
	gsub(/<span class="pre">/, "<code>");
	gsub(/<\/span>/, "</code>");
    }
    print $0 "<p>";
    next;
}

# Handle verbatim text.
/<span class="pre">[^<]*<\/span>/ {
    gsub(/<span class="pre">/, "<code>");
    gsub(/<\/span>/, "</code>");
}

# RST link targets. We massage these into a form which enables us to pick them
# up later. TODO: There's probably other Sphinx-generated markup carrying such
# labels, we should add them as we discover them.
/<p id="[^"]*">/ {
    print gensub(/<p id="([^"]*)">/, "<a href=\"#\\1\"></a>&", "g");
    next;
}
/<span class="target" id="[^"]*">/ {
    print gensub(/<span class="target" id="([^"]*)">/, "<a href=\"#\\1\"></a>&", "g");
    next;
}
/<dl class="docutils" id="[^"]*">/ {
    if (match($0, /<dl class="docutils" id="([^"]*)">/, matches))
	lastid = matches[1];
    print;
    next;
}
/<\/dt>/ {
    if (lastid) {
	gsub(/<\/dt>/, sprintf("<a href=\"#%s\"></a></dt>", lastid));
	lastid = "";
    }
    print;
    next;
}
/<\/dl>/ {
    lastid = "";
    print;
    next;
}

{ if (mode!=1) print; }
