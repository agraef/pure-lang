<a name="doc-pure-readline"></a>

<a name="module-readline"></a>

pure-readline
=============

Version 0.3, March 06, 2017

Albert Graef &lt;<aggraef@gmail.com>&gt;

Get the latest source from
<https://bitbucket.org/purelang/pure-lang/downloads/pure-readline-0.3.tar.gz>.

This is a trivial wrapper around GNU readline, which gives Pure scripts access
to the most important facilities of the readline interface. This includes
support for the `readline` function itself (without custom completion at
present) and basic history management. The wrapper can also be used with the
BSD editline a.k.a. libedit library, a readline replacement licensed under the
3-clause BSD license. You can find these at:

-   GNU readline: <http://tiswww.tis.case.edu/~chet/readline/rltop.html>
-   BSD editline/libedit: <http://www.thrysoee.dk/editline>

We recommend GNU readline because it's easier to use and has full UTF-8
support, but in some situations BSD editline/libedit may be preferable for
license reasons or because it's what the operating system provides. Note that
in either case Pure programs using this module are subject to the license
terms of the library that you use (GPLv3+ in case of GNU readline, BSD license
in the case of BSD editline/libedit).

Normally, you should choose the same library that you use with the Pure
interpreter, to avoid having two different versions of the library linked into
your program. (This doesn't matter if you only use this module with
batch-compiled scripts, though, since the Pure runtime doesn't depend on
readline in any way.) By default, the module will be built with GNU readline.
To select editline/libedit instead, you only have to uncomment a line at the
beginning of the Makefile. Also, you might want to check the beginning of
readline.c for the proper location of the corresponding header files.

The module provides the following functions:

<a name="readline"></a>`readline prompt`
:   Read a line of input from the user, with prompting and command line
    editing. Returns the input line (with the trailing newline removed), or
    [`NULL`](#NULL) when reaching end of file.

<a name="add_history"></a>`add_history line`
:   Adds the given line (a string) to the command history.

<a name="clear_history"></a>`clear_history`
:   Clears the command history.

<a name="read_history"></a>`read_history fname`
:   Reads the command history from the given file. Note that this in fact
    *adds* the contents of the history file to the current history, so you may
    want to call [`clear_history`](#clear_history) beforehand if this function
    is called multiple times.

<a name="write_history"></a>`write_history fname`
:   Writes the current command history to the given file.

<!-- -->
Example:

    > using readline;
    > readline "input> ";
    input> Hello, world!
    "Hello, world!"
    > add_history ans;
    ()
    > readline "input> ";
    input> <EOF>
    #<pointer 0x0>
    > write_history "history"; // save the history
    0
    > clear_history;
    > read_history "history"; // read the history
    0
