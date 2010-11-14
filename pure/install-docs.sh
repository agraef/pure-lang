#! /bin/sh

docs=$1
tmpdir=pure-docs

if test -z "$docs"; then
   echo "Usage: install-docs url-or-filename"
   exit 1
fi

rm -rf $tmpdir && mkdir $tmpdir
trap "rm -rf $tmpdir" EXIT

case $docs in
  http:* | https:* | ftp:*)
    if ! wget -nv $docs -O $tmpdir/docs.tar.gz; then
      echo "$0: Error downloading $docs" > /dev/stderr
      exit 1
    elif ! (cd $tmpdir && tar -z -xf docs.tar.gz); then
      echo "$0: Error extracting archive" > /dev/stderr
      exit 1
    elif ! test -d $tmpdir/pure-docs* 2>/dev/null; then
      echo "$0: Documentation not found in archive" > /dev/stderr
      exit 1
    elif $MAKE -C $tmpdir/pure-docs* install; then
      echo "Documentation installed successfully"
      exit 0
    else
      echo "$0: Error installing documentation" > /dev/stderr
      exit 1
    fi;;
  *)
    if ! tar -z -xf $docs -C $tmpdir; then
      echo "$0: Error extracting archive" > /dev/stderr
      exit 1
    elif ! test -d $tmpdir/pure-docs* 2>/dev/null; then
      echo "$0: Documentation not found in archive" > /dev/stderr
      exit 1
    elif $MAKE -C $tmpdir/pure-docs* install; then
      echo "Documentation installed successfully"
      exit 0
    else
      echo "$0: Error installing documentation" > /dev/stderr
      exit 1
    fi;;
esac
