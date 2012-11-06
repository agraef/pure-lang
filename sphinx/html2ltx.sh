#! /bin/sh
pandoc "$1" -t latex -N --no-wrap | awk -f html2ltx.awk name="$tmp"
