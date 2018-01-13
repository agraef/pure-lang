#! /bin/sh

# This is a hack to convert Sphinx-generated html into a texmacs document
# which at least resembles the original, using a combination of pandoc and
# various awk passes. I readily admit that this is an unholy mess; some
# horrible kludges are needed since pandoc doesn't support all the fancy
# markup that Sphinx uses and the TeXmacs latex import isn't perfect either.

tmp=`basename "$1" .html`
echo Converting "$1" to "$tmp".tm

# Use pandoc to convert the html file to LaTeX, pre- and postprocess with awk.
awk -f htmlpre.awk "$1" | pandoc -f html -t latex -N --no-wrap | awk -f html2ltx.awk name="$tmp" > "$tmp".tex

# Run texmacs to convert the LaTeX source to TeXmacs format. Note that for
# larger documents this step can be very slow.
texmacs --convert "$tmp".tex "$tmp".tm --quit >/dev/null 2>&1
# Older texmacs versions might need to be run via xvfb-run, to prevent it from
# popping up an X11 window.
#xvfb-run texmacs --convert "$tmp".tex "$tmp".tm --quit >/dev/null 2>&1

# Postprocess the TeXmacs file with awk again, to work around various glitches
# in the TeXmacs latex import.
awk -f htmlpost.awk < "$tmp".tm > "$tmp".tm1

# Rename the temp file and get rid of intermediate files.
rm -f "$tmp".tm && mv "$tmp".tm1 "$tmp".tm
rm -f "$tmp".tex
