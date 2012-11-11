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

mode==1 { next; }

{
    # Handle verbatim text.
    if (/<span class="pre">[^<]*<\/span>/) {
	gsub(/<span class="pre">/, "<code>");
	gsub(/<\/span>/, "</code>");
    }
    # RST link targets. We massage these into a form which enables us to pick
    # them up later. TODO: There's probably other Sphinx-generated markup
    # carrying such labels, we should add them as we discover them.
    if (/<p id="[^"]*">/)
	$0 = gensub(/<p id="([^"]*)">/, "<a href=\"#\\1\"></a>&", "g");
    else if (/<span class="target" id="[^"]*">/)
	$0 = gensub(/<span class="target" id="([^"]*)">/, "<a href=\"#\\1\"></a>&", "g");
    else if (/<dl class="docutils" id="[^"]*">/) {
	if (match($0, /<dl class="docutils" id="([^"]*)">/, matches))
	    lastid = matches[1];
    } else if (/<\/dt>/) {
	if (lastid) {
	    gsub(/<\/dt>/, sprintf("<a href=\"#%s\"></a></dt>", lastid));
	    lastid = "";
	}
    } else if (/<\/dl>/) {
	lastid = "";
    }
    # Basic support for tables. We translate the relevant markup to ordinary
    # text so that we can pick it up later. Note that the output gets diverted
    # to a text buffer here, so that we know the number of columns when
    # outputting the table.
    if (/^<table .*>$/ && !/<table .*class="[^"]*(list|note|table)[^"]*"/) {
	mode = 2; div = $0;
	columns = maxcolumns = 0;
    } else if (mode==2 && /^<\/table>$/) {
	if (columns > maxcolumns) maxcolumns = columns;
	mode = 0;
	div = ".TA " maxcolumns "\n" div "\n" $0 "\n.TE";
	print div; div = "";
    } else if (mode==2) {
	if (/^<thead .*>$/)
	    div = div "\n.TH";
	else if (/^<tbody .*>$/)
	    div = div "\n.TB";
	if (/^<tr[^>]*>/) {
	    if (columns > maxcolumns) maxcolumns = columns;
	    columns = 0;
	    div = div "\n.TR";
	}
	if (/<(th|td)[^>]*>/) {
	    gsub(/<(th|td)( [^>]*)?>/, "\n.TC\n&");
	    columns++;
	}
	div = div "\n" $0;
    } else
	print;
}
