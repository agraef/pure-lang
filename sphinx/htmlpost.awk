BEGIN { verbatim = 0; }

{
    if (/<\\verbatim>/)
	verbatim = 1;
    else if (/<\/verbatim>/)
	verbatim = 0;
    # Add the puredoc style option. There's probably a way to do that in
    # texmacs/ guile, but it seems easier to just edit the texmacs document
    # preamble instead.
    gsub(/<style\|generic>/, "<style|<tuple|generic|puredoc>>");
    # Newer TeXmacs versions use the std-latex style:
    gsub(/<style\|<tuple\|generic\|std-latex>>/, "<style|<tuple|generic|puredoc>>");
    # Turn standard images into puredoc-image, so that we can do some extra
    # tweaks with the formatting.
    gsub(/<image\|/, "<puredoc-image|");
    # Stuff that the texmacs latex importer gets wrong.
    gsub(/\\<copyright\\>/, "<copyright>");
    if (!verbatim) $0 = gensub(/^([ ]+)\\ /, "\\1", "g");
    $0 = gensub(/([^\\]>) \\ /, "\\1 ", "g");
    gsub(/<quote-env\| +/, "<quote-env|");
    gsub(/<cell\| +/, "<cell|");
    print $0;
}
