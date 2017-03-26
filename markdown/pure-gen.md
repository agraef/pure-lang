<a name="doc-pure-gen"></a>

pure-gen: Pure interface generator
==================================

Version 0.20, March 06, 2017

Albert Gräf &lt;<aggraef@gmail.com>&gt;

Name
----

pure-gen -- Pure interface generator

pure-gen is a C interface generator for the Pure language. It takes a C header
file as input and generates a corresponding Pure module with the constant
definitions and extern declarations needed to use the C module from Pure.
pure-gen can also generate FFI interfaces rather than externs (using the
[pure-ffi](pure-ffi.html) module), and it can optionally create a C wrapper
module which allows you to create interfaces to pretty much any code which can
be called via C.

Synopsis
--------

    pure-gen [options ...] input-file

Options
-------

### General Options

<a name="cmdoption-pure-gen-h"></a>`-h`, <a name="cmdoption-pure-gen--help"></a>`--help`
:   Print a brief help message and exit.

<a name="cmdoption-pure-gen-V"></a>`-V`, <a name="cmdoption-pure-gen--version"></a>`--version`
:   Print version number and exit.

<a name="cmdoption-pure-gen-e"></a>`-e`, <a name="cmdoption-pure-gen--echo"></a>`--echo`
:   Echo preprocessor lines. Prints all processed `#define`s, useful for
    debugging purposes.

<a name="cmdoption-pure-gen-v"></a>`-v`, <a name="cmdoption-pure-gen--verbose"></a>`--verbose`
:   Show parameters and progress information. Gives useful information about
    the conversion process.

<a name="cmdoption-pure-gen-w"></a>`-w[level]`, <a name="cmdoption-pure-gen--warnings"></a>`--warnings[=level]`
:   Display warnings, `level` = 0 (disable most warnings), 1 (default, shows
    important warnings only) or 2 (lots of additional warnings useful for
    debugging purposes).

<!-- -->
### Preprocessor Options

<a name="cmdoption-pure-gen-I"></a>`-I path`, <a name="cmdoption-pure-gen--include"></a>`--include path`
:   Add include path. Passed to the C preprocessor.

<a name="cmdoption-pure-gen-D"></a>`-D name[=value]`, <a name="cmdoption-pure-gen--define"></a>`--define name[=value]`
:   Define symbol. Passed to the C preprocessor.

<a name="cmdoption-pure-gen-U"></a>`-U name`, <a name="cmdoption-pure-gen--undefine"></a>`--undefine name`
:   Undefine symbol. Passed to the C preprocessor.

<a name="cmdoption-pure-gen-C"></a>`-C option`, <a name="cmdoption-pure-gen--cpp"></a>`--cpp option`
:   Pass through other preprocessor options and arguments.

<!-- -->
### Generator Options

<a name="cmdoption-pure-gen-f"></a>`-f iface`, <a name="cmdoption-pure-gen--interface"></a>`--interface iface`
:   Interface type (`extern`, `c`, `ffi` or `c-ffi`). Default is `extern`. The
    `extern` and `c` types generate Pure `extern` declarations, which is what
    you want in most cases. `ffi` and `c-ffi` employ Pure's libffi interface
    instead. The `c` and `c-ffi` types cause an additional C wrapper module to
    be created (see [Generating C Code](#generating-c-code)). These can also
    be combined with the `-auto` suffix which creates C wrappers only when
    needed to get C struct arguments and returns working, see [Dealing with C
    Structs](#dealing-with-c-structs) for details.

<a name="cmdoption-pure-gen-l"></a>`-l lib`, <a name="cmdoption-pure-gen--lib-name"></a>`--lib-name lib`
:   Add dynamic library module to be imported in the Pure output file. Default
    is `-l c-file` (the filename specified with [`-c`](#cmdoption-pure-gen-c),
    see below, without filename extension) if one of the `-fc` options was
    specified, none otherwise.

<a name="cmdoption-pure-gen-m"></a>`-m name`, <a name="cmdoption-pure-gen--namespace"></a>`--namespace name`
:   Module namespace in which symbols should be declared.

<a name="cmdoption-pure-gen-p"></a>`-p prefix`, <a name="cmdoption-pure-gen--prefix"></a>`--prefix prefix`
:   Module name prefix to be removed from C symbols.

<a name="cmdoption-pure-gen-P"></a>`-P prefix`, <a name="cmdoption-pure-gen--wrap"></a>`--wrap prefix`
:   Prefix to be prepended to C wrapper symbols (`-fc` and friends). Default
    is `Pure_`.

<a name="cmdoption-pure-gen-a"></a>`-a`, <a name="cmdoption-pure-gen--all"></a>`--all`
:   Include "hidden" symbols in the output. Built-in preprocessor symbols and
    symbols starting with an underscore are excluded unless this option is
    specified.

<a name="cmdoption-pure-gen-s"></a>`-s pattern`, <a name="cmdoption-pure-gen--select"></a>`--select pattern`
:   Selection of C symbols to be included in the output. `pattern` takes the
    form `[glob-patterns::][regex-pattern]`, designating a comma separated
    list of glob patterns matching the source filenames, and an extended
    regular expression matching the symbols to be processed. See glob(7) and
    regex(7). The default `pattern` is empty which matches all symbols in all
    source modules.

<a name="cmdoption-pure-gen-x"></a>`-x pattern`, <a name="cmdoption-pure-gen--exclude"></a>`--exclude pattern`
:   Like [`-s`](#cmdoption-pure-gen-s), but *excludes* all matching C symbols
    from the selection.

<a name="cmdoption-pure-gen-t"></a>`-t file`, <a name="cmdoption-pure-gen--template"></a>`--template file`
:   Specify a C template file to be used with C wrapper generation (`-fc`).
    See [Generating C Code](#generating-c-code) for details.

<a name="cmdoption-pure-gen-T"></a>`-T file`, <a name="cmdoption-pure-gen--alt-template"></a>`--alt-template file`
:   Specify an alternate C template file to be used with C wrapper generation
    (`-fc`). See [Generating C Code](#generating-c-code) for details.

<!-- -->
### Output Options

<a name="cmdoption-pure-gen-n"></a>`-n`, <a name="cmdoption-pure-gen--dry-run"></a>`--dry-run`
:   Only parse without generating any output.

<a name="cmdoption-pure-gen-N"></a>`-N`, <a name="cmdoption-pure-gen--noclobber"></a>`--noclobber`
:   Append output to existing files.

<a name="cmdoption-pure-gen-o"></a>`-o file`, <a name="cmdoption-pure-gen--output"></a>`--output file`
:   Pure output (.pure) filename. Default is `input-file` with new extension
    .pure.

<a name="cmdoption-pure-gen-c"></a>`-c file`, <a name="cmdoption-pure-gen--c-output"></a>`--c-output file`
:   C wrapper (.c) filename (`-fc`). Default is `input-file` with new
    extension .c.

<!-- -->
Description
-----------

pure-gen generates Pure bindings for C functions from a C header file. For
instance, the command

    pure-gen foo.h

creates a Pure module foo.pure with `extern` declarations for the constants
(`#define`s and enums) and C routines declared in the given C header file and
(recursively) its includes.

pure-gen only accepts a single header file on the command line. If you need to
parse more than one header in a single run, you can just create a dummy header
with all the necessary `#include`s in it and pass that to pure-gen instead.

When invoked with the [`-n`](#cmdoption-pure-gen-n) option, pure-gen performs
a dry run in which it only parses the input without actually generating any
output files. This is useful for checking the input (possibly in combination
with the [`-e`](#cmdoption-pure-gen-e), [`-v`](#cmdoption-pure-gen-v) and/or
[`-w`](#cmdoption-pure-gen-w) options) before generating output. A
particularly useful example is

``` {.sourceCode .sh}
pure-gen -ne foo.h \
  | awk '$1=="#" && $2~/^[0-9]+$/ && $3!~/^"<.*>"$/  { print $3 }' \
  | sort | uniq
```

which prints on standard output all headers which are included in the source.
This helps to decide which headers you want to be included in the output, so
that you can set up a corresponding filter patterns
([`-s`](#cmdoption-pure-gen-s) and [`-x`](#cmdoption-pure-gen-x) options, see
below).

The [`-I`](#cmdoption-pure-gen-I), [`-D`](#cmdoption-pure-gen-D) and
[`-U`](#cmdoption-pure-gen-U) options are simply passed to the C preprocessor,
as well as any other option or argument escaped with the
[`-C`](#cmdoption-pure-gen-C) flag. This is handy if you need to define
additional preprocessor symbols, add directories to the include search path,
etc., see cpp(1) for details.

There are some other options which affect the generated output. In particular,
`-f c` generates a C wrapper module along with the Pure module (see
[Generating C Code](#generating-c-code) below), and `-f ffi` generates a
wrapper using Pure's ffi module. Moreover, `-l libfoo` generates a
`using "lib:libfoo"` declaration in the Pure source, for modules which require
a shared library to be loaded. Any number of [`-l`](#cmdoption-pure-gen-l)
options can be specified.

Other options for more advanced uses are explained in the following sections.

Filtering
---------

Note that pure-gen always parses the given header file as well as *all* its
includes. If the header file includes system headers, by default you will get
those declarations as well. This is often undesirable. As a remedy, pure-gen
normally excludes built-in `#define`s of the C preprocessor, as well as
identifiers with a leading underscore (which are often found in system
headers) from processing. You can use the [`-a`](#cmdoption-pure-gen-a) option
to disable this, so that all these symbols are included as well.

In addition, the [`-s`](#cmdoption-pure-gen-s) and
[`-x`](#cmdoption-pure-gen-x) options enable you to filter C symbols using the
source filename and the symbol as search criteria. For instance, to just
generate code for a single header foo.h and none of the other headers included
in foo.h, you can invoke pure-gen as follows:

    pure-gen -s foo.h:: foo.h

Note that even in this case all included headers will be parsed so that
`#define`d constants and enum values can be resolved, but the generated output
will only contain definitions and declarations from the given header file.

In general, the [`-s`](#cmdoption-pure-gen-s) option takes an argument of the
form `glob-patterns::regex-pattern` denoting a comma-separated list of glob
patterns to be matched against the source filename in which the symbol
resides, and an extended regex to be matched against the symbol itself. The
`glob-patterns::` part can also be omitted in which case it defaults to `::`
which matches any source file. The regex can also be empty, in which case it
matches any symbol. The generated output will contain only the constant and
function symbols matching the given regex, from source files matching any of
the the glob patterns. Thus, for instance, the option
`-s foo.h,bar.h::^(foo|bar)_` pulls all symbols prefixed with either `foo_` or
`bar_` from the files foo.h and bar.h in the current directory.

Instead of `::` you can also use a single semicolon `;` to separate glob and
regex pattern. This is mainly for Windows compatibility, where the msys shell
sometimes eats the colons or changes them to `;`.

The [`-x`](#cmdoption-pure-gen-x) option works exactly the same, but
*excludes* all matching symbols from the selection. Thus, e.g., the option
`-x ^bar_` causes all symbols with the prefix `bar_` to *not* be included in
the output module.

Processing of glob patterns is performed using the customary rules for
filename matching, see glob(7) for details. Note that some include files may
be specified using a full pathname. This is the case, in particular, for
system includes such as `#include <stdio.h>`, which are resolved by the C
preprocessor employing a search of the system include directories (as well as
any directories named with the [`-I`](#cmdoption-pure-gen-I) option).

Since the `*` and `?` wildcards never match the pathname separator `/`, you
have to specify the path in the glob patterns in such cases. Thus, e.g., if
the foo.h file actually lives in either /usr/include or /usr/local/include,
then it must be matched using a pattern like
`/usr/include/*.h,/usr/local/include/*.h::`. Just `foo.h::` will not work in
this case. On the other hand, if you have set up your C sources in some local
directory then specifying a relative pathname is ok.

Name Mangling
-------------

The [`-s`](#cmdoption-pure-gen-s) option is often used in conjuction with the
[`-p`](#cmdoption-pure-gen-p) option, which lets you specify a "module name
prefix" which should be stripped off from C symbols. Case is insignificant and
a trailing underscore will be removed as well, so `-p foo` turns `fooBar` into
`Bar` and `FOO_BAR` into `BAR`. Moreover, the [`-m`](#cmdoption-pure-gen-m)
option allows you to specify the name of a Pure namespace in which the
resulting constants and functions are to be declared. So, for instance,
`-s "^(foo|FOO)" -p foo -m foo` will select all symbols starting with the
`foo` or `FOO` prefix, stripping the prefix from the selected symbols and
finally adding a `foo::` namespace qualifier to them instead.

Generating C Code
-----------------

As already mentioned, pure-gen can be invoked with the `-fc` or `-fc-ffi`
option to create a C wrapper module along with the Pure module it generates.
There are various situations in which this is preferable, e.g.:

-   You are about to create a new module for which you want to generate some
    boilerplate code.
-   The C routines to be wrapped aren't available in a shared library, but in
    some other form (e.g., object file or static library).
-   You need to inject some custom code into the wrapper functions (e.g., to
    implement custom argument preprocessing or lazy dynamic loading of
    functions from a shared library).
-   The C routines can't be called directly through Pure externs.

The latter case might arise, e.g., if the module uses non-C linkage or calling
conventions, or if some of the operations to be wrapped are actually
implemented as C macros. (Note that in order to wrap macros as functions
you'll have to create a staged header which declares the macros as C
functions, so that they are wrapped in the C module. pure-gen doesn't do this
automatically.)

Another important case is that some of the C routines pass C structs by value
or return them as results. This is discussed in more detail in the following
section.

For instance, let's say that we want to generate a wrapper foo.c from the
foo.h header file whose operations are implemented in some library libfoo.a or
libfoo.so. A command like the following generates both the C wrapper and the
corresponding Pure module:

    pure-gen -fc foo.h

This creates foo.pure and foo.c, with an import clause for `"lib:foo"` at the
beginning of the Pure module. (You can also change the name of the Pure and C
output files using the [`-o`](#cmdoption-pure-gen-o) and
[`-c`](#cmdoption-pure-gen-c) options, respectively.)

The generated wrapper is just an ordinary C file which should be compiled to a
shared object (dll on Windows) as usual. E.g., using gcc on Linux:

    gcc -shared -o foo.so foo.c -lfoo

That's all. You should now be able to use the foo module by just putting the
declaration `using foo;` into your programs. The same approach also works with
the ffi interface if you replace the `-fc` option with `-fc-ffi`.

You can also adjust the C wrapper code to some extent by providing your own
template file, which has the following format:

``` {.sourceCode .c}
/* frontmatter here */
#include %h
%%

/* wrapper here */
%r %w(%p)
{
  return %n(%a);
}
```

Note that the code up to the symbol `%%` on a line by itself denotes
"frontmatter" which gets inserted at the beginning of the C file. (The
frontmatter section can also be empty or missing altogether if you don't need
it, but usually it will contain at least an `#include` for the input header
file.)

The rest of the template is the code for each wrapper function. Substitutions
of various syntactical fragments of the function definition is performed using
the following placeholders:

`%h` input header file

`%r` return type of the function

`%w` the name of the wrapper function

`%p` declaration of the formal parameters of the wrapper function

`%n` the real function name (i.e., the name of the target C function to be
called)

`%a` the arguments of the function call (formal parameters with types stripped
off)

`%%` escapes a literal %

A default template is provided if you don't specify one (which looks pretty
much like the template above, minus the comments). A custom template is
specified with the [`-t`](#cmdoption-pure-gen-t) option. (There's also a
[`-T`](#cmdoption-pure-gen-T) option to specify an "alternate" template for
dealing with routines returning struct values, see [Dealing with C
Structs](#dealing-with-c-structs).)

For instance, suppose that we place the sample template above into a file
foo.templ and invoke pure-gen on the foo.h header file as follows:

    pure-gen -fc -t foo.templ foo.h

Then in foo.c you'd get C output code like the following:

``` {.sourceCode .c}
/* frontmatter here */
#include "foo.h"

/* wrapper here */
void Pure_foo(int arg0, void* arg1)
{
  return foo(arg0, arg1);
}

/* wrapper here */
int Pure_bar(int arg0)
{
  return bar(arg0);
}
```

As indicated, the wrapper function names are usually stropped with the `Pure_`
prefix. You can change this with the [`-P`](#cmdoption-pure-gen-P) option.

This also works great to create boilerplate code for new modules. For this
purpose the following template will do the trick:

``` {.sourceCode .c}
/* Add #includes etc. here. */
%%

%r %n(%p)
{
  /* Enter code of %n here. */
}
```

Dealing with C Structs
----------------------

Modern C compilers allow you to pass C structs by value or return them as
results from a C function. This represents a problem, because Pure doesn't
provide any support for that in its extern declarations. Even Pure's libffi
interface only has limited support for C structs (no unions, no bit fields),
and at present pure-gen itself does not keep track of the internal structure
of C structs either.

Hence pure-gen will bark if you try to wrap an operation which passes or
returns a C struct, printing a warning message like the following which
indicates that the given function could not be wrapped:

    Warning: foo: struct argument or return type, try -fc-auto

What Pure *does* know is how to pass and return *pointers* to C structs in its
C interface. This makes it possible to deal with struct arguments and return
values in the C wrapper. To make this work, you need to create a C wrapper
module as explained in the previous section. However, as C wrappers are only
needed for functions which actually have struct arguments or return values,
you can also use the `-fc-auto` option (or `-fc-ffi-auto` if you prefer the
ffi interface) to only generate the C wrapper when required. This saves the
overhead of an extra function call if it's not actually needed.

Struct arguments in the original C function then become struct pointers in the
wrapper function. E.g., if the function is declared in the header as follows:

``` {.sourceCode .c}
typedef struct { double x, y; } point;
extern double foo(point p);
```

Then the generated wrapper code becomes:

``` {.sourceCode .c}
double Pure_foo(point* arg0)
{
  return foo(*arg0);
}
```

Which is declared in the Pure interface as:

    extern double Pure_foo(point*) = foo;

Struct return values are handled by returning a pointer to a static variable
holding the return value. E.g.,

``` {.sourceCode .c}
extern point bar(double x, double y);
```

becomes:

``` {.sourceCode .c}
point* Pure_bar(double arg0, double arg1)
{
  static point ret;
  ret = bar(arg0, arg1); return &ret;
}
```

Which is declared in the Pure interface as:

    extern point* Pure_bar(double, double) = bar;

(Note that the generated code in this case comes from an alternate template.
It's possible to configure the alternate template just like the normal one,
using the [`-T`](#cmdoption-pure-gen-T) option instead of
[`-t`](#cmdoption-pure-gen-t). See the [Generating C Code](#generating-c-code)
section above for details about code templates.)

In a Pure script you can now call `foo` and `bar` as:

    > foo (bar 0.0 1.0);

Note, however, that the pointer returned by `bar` points to static storage
which will be overwritten each time you invoke the `bar` function. Thus in the
following example *both* `u` and `v` will point to the same `point` struct,
namely that defined by the latter call to `bar`:

    > let u = bar 1.0 0.0; let v = bar 0.0 1.0;

Which most likely is *not* what you want. To avoid this, you'll have to take
dynamic copies of returned structs. It's possible to do this manually by
fiddling around with `malloc` and `memcpy`, but the most convenient way is to
employ the struct functions provided by Pure's ffi module:

    > using ffi;
    > let point_t = struct_t (double_t, double_t);
    > let u = copy_struct point_t (bar 1.0 0.0);
    > let v = copy_struct point_t (bar 0.0 1.0);

Now `u` and `v` point to different, malloc'd structs which even take care of
freeing themselves when they are no longer needed. Moreover, the ffi module
also allows you to access the members of the structs in a direct fashion.
Please refer to the [pure-ffi](pure-ffi.html) documentation for further
details.

Notes
-----

pure-gen currently requires gcc (`-E`) as the C preprocessor. It also needs a
version of gcc which understands the `-fdirectives-only` option, which means
gcc 4.3 or later. It will run with older versions of gcc, but then you'll get
an error message from gcc indicating that it doesn't understand the
`-fdirectives-only` option. pure-gen then won't be able to extract any
`#define`d constants from the header files.

pure-gen itself is written in Pure, but uses a C parser implemented in
Haskell, based on the Language.C library written by Manuel Chakravarty and
others.

pure-gen can only generate C bindings at this time. Other languages may have
their own calling conventions which make it hard or even impossible to call
them directly through Pure's extern interface. However, if your C compiler
knows how to call the other language, then it may be possible to interface to
modules written in that language by faking a C header for the module and
generating a C wrapper with a custom code template, as described in
[Generating C Code](#generating-c-code). In principle, this approach should
even work with behemoths like C++, although it might be easier to use
third-party tools like SWIG for that purpose.

In difference to SWIG and similar tools, pure-gen doesn't require you to write
any special "interface files", is controlled entirely by command line options,
and the amount of marshalling overhead in C wrappers is negligible. This is
possible since pure-gen targets only the Pure-C interface and Pure has good
support for interfacing to C built into the language already.

pure-gen usually works pretty well if the processed header files are written
in a fairly clean fashion. Nevertheless, some libraries defy fully automatic
wrapper generation and may thus require staged headers and/or manual editing
of the generated output to get a nice wrapper module.

In complex cases it may also be necessary to assemble the output of several
runs of pure-gen for different combinations of header files, symbol selections
and/or namespace/prefix settings. In such a situation it is usually possible
to just concatenate the various output files produced by pure-gen to
consolidate them into a single wrapper module. To make this easier, pure-gen
provides the [`-N`](#cmdoption-pure-gen-N) a.k.a.
[`--noclobber`](#cmdoption-pure-gen--noclobber) option which appends the
output to existing files instead of overwriting them. See the example below.

Example
-------

For the sake of a substantial, real-world example, here is how you can wrap
the entire GNU Scientific Library in a single Pure module mygsl.pure, with the
accompanying C module in mygsl.c:

``` {.sourceCode .sh}
rm -f mygsl.pure mygsl.c
DEFS=-DGSL_DISABLE_DEPRECATED
for x in /usr/include/gsl/gsl_*.h; do
  pure-gen $DEFS -N -fc-auto -s "$x::" $x -o mygsl.pure -c mygsl.c
done
```

The C module can then be compiled with:

``` {.sourceCode .sh}
gcc $DEFS -shared -o mygsl.so mygsl.c -lgsl
```

Note that the `GSL_DISABLE_DEPRECATED` symbol must be defined here to avoid
some botches with constants being defined in incompatible ways in different
GSL headers. Also, some GSL versions have broken headers lacking some system
includes which causes hiccups in pure-gen's C parser. Fixing those errors or
working around them through some appropriate cpp options should be a piece of
cake, though.

License
-------

BSD-like. See the accompanying COPYING file for details.

Authors
-------

Scott E. Dillard (University of California at Davis), Albert Graef (Johannes
Gutenberg University at Mainz, Germany).

See Also
--------

Language.C

:   A C parser written in Haskell by Manuel Chakravarty et al,
    <http://www.sivity.net/projects/language.c>.

SWIG

:   The Simplified Wrapper and Interface Generator, <http://www.swig.org>.


