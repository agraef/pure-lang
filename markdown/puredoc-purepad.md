<a name="doc-purepad"></a>

Using PurePad
=============

The following information is available:

-   [Running Pure on Windows](#running-pure-on-windows): important release
    notes that you should read first
-   [Getting Started](#getting-started): a brief overview of PurePad
-   [Editing Scripts](#editing-scripts): manage Pure scripts with PurePad
-   [Running Scripts](#running-scripts): run Pure scripts using the Pure
    interpreter
-   [Using the Log](#using-the-log): how to interact with the Pure interpreter
-   [Locating Source Lines](#locating-source-lines): quickly find source lines
    in error messages from the interpreter

Please also check the sidebar for other available documentation, including
[The Pure Manual](#the-pure-manual), the [Pure Library
Manual](#pure-library-manual) and information about various bundled modules.
(Note that at present only a subset of all available addon modules is included
in the Windows distribution.)

Getting Started
---------------

PurePad is a standard Windows application, with the menus, toolbar and status
line you are familiar with (see figure below). Like the standard Windows
editor from ancient times, PurePad is a single-document application, i.e.,
there is only one source file open at any time. The main window is divided
into two panes. The upper pane is the source pane which usually contains the
Pure script you are currently working with. The lower pane is the log pane in
which you interact with the interpreter (input expressions to be evaluated,
watch the interpreter's output, etc.). Editing operations, as well as the
`Font` and `Tab Stops` commands in the `View` menu and the `Print` and
`Print Preview` commands in the `File` menu, always apply to the currently
selected pane, i.e., the pane which contains the cursor.

![PurePad main window.](purepad.png)

To start up the interpreter you use `Script --> Run` (`F9`) and wait for the
interpreter's `>` prompt to appear. This command will run the Pure interpreter
with the script currently shown in the source pane (or an empty script if you
haven't yet loaded a script using the `File --> Open` (`Ctrl-O`) command). The
cursor will be placed in the log pane (see [Using the Log](#using-the-log))
and you can start typing in expressions and definitions to be evaluated. See
[Running Scripts](#running-scripts) for more information on this.

To begin a new script, use the `File --> New` (`Ctrl-N`) command. Probably the
next thing you want to do is to enter your first own little Pure script. For
instance, here is a version of the factorial function:

    fact n = if n>0 then n*fact (n-1) else 1;

Enter this equation into the source editor pane, save the script as
`fact.pure` with the `File --> Save` (`Ctrl-S`) command and press `F9`. The
cursor will be placed into the log pane and after a while the interpreter's
`>` prompt will appear. You can now type an expression like

    map fact (1.. 10);

and see what happens.

The currently selected pane (source or log) can be printed using the
`File --> Print` (`Ctrl-P`) command; you can also obtain a print preview with
the `Print Preview` command in the `File` menu. To change the font and
tabulator settings in the current pane, use the `Font` and `Tab Stops`
commands in the `View` menu. (Usually it is best to choose a fixed-width font
here, like `Fixedsys`, which is also the default. Tab stops are set to 8 by
default.)

Editing Scripts
---------------

The `File` menu contains the usual set of operations which let you create new
script files, open existing files, save a file that has been edited, preview
and print the current file and exit the application. The `View` menu allows
you to change tabulator settings (`Tab Stops` option) and the font used for
display and printing (`Font` option). Scripts are edited in the upper (source)
pane of the main window just as in the Windows Notepad editor. In the `Edit`
menu you find the common editing operations (`Undo`, `Cut`, `Copy`, `Paste`,
`Select All`, `Find`, `Replace`, and `Go To`, which allows you to jump to the
line number you specify). Many of these operations can also be accessed by
means of the familiar accelerator keys or the toolbar.

Running Scripts
---------------

Once you have entered your script and saved it in a file, you can run the
script using the Pure interpreter. The relevant commands can be found in the
`Script` menu:

-   `Run` runs your script with the Pure interpreter, in the directory where
    your source script is located. If the interpreter is already running, it
    is terminated first.
-   `Debug` invokes the interpreter with the built-in debugger. This allows
    you to trace the calculations ("reductions" in Pure parlance) performed by
    your script. Note that to actually debug a function, you must first set a
    breakpoint using the interpreter's `break` command; please see [The Pure
    Manual](#the-pure-manual) for details.
-   `Break` sends a `Ctrl-C` to the interpreter process. This allows you to
    interrupt the interpreter when it is doing a time-intensive evaluation, is
    producing output or is waiting for input.
-   `Quit` exits the interpreter. This usually has the same effect as typing
    `quit` at the interpreter's prompt and is performed automatically when
    PurePad is exited and the interpreter is still active. It also kills off
    the interpreter process if it does not terminate within a reasonable
    amount of time after it has been notified.

As soon as the script has been started, the cursor switches to the log (the
lower pane), the interpreter's prompt will appear and you can start typing
definitions and expressions, and watch the interpreter print the results. The
log pane is an edit control into which you can type text as usual. It also has
some special commands which allow you to access an input history and to
quickly locate positions in source files, see [Using the Log](#using-the-log)
for details.

If you run a script which has errors in it then the Pure compiler will display
a list of error messages. To quickly locate the source file positions listed
in the error messages, use the commands described in [Locating Source
Lines](#locating-source-lines).

You can check whether a script is running by taking a look at the title bar of
the PurePad window. The name of the previously started script is shown there
inside brackets. If it is preceded with the text `Terminated -`, then the
script is not currently running, either because the interpreter was exited in
a regular fashion, or because some other, unexpected event happened, like a
stack overflow.

There are a number of other items in the `Script` menu which deal with the
running script and the interpreter's configuration:

-   `Open` reopens the previously started script (the one whose name is shown
    inside brackets in the title bar) in the upper source pane. This operation
    is useful when you have opened other source files and now want to quickly
    reload your "main" script.
-   The `Prompt` option allows you to change the interpreter's prompt (this
    only becomes effective when the interpreter is started the next time using
    the `Run` or the `Debug` command).
-   `History File`, `History Size`. With these options you can set the name of
    the file used to store the input history, and the size of the history,
    respectively (see [Using the Log](#using-the-log) for details).
-   `Reset Log`. When this option is checked, the log is cleared any time a
    new script is run.

Using the Log
-------------

The log, the lower pane of the PurePad main window, is an edit control which
logs both your input to the interpreter and the interpreter's output. In the
log you can use all the usual editing operations, as well as the commands in
the `Edit` menu. Furthermore, printouts and print previews of the current
contents of the log can be obtained using the corresponding operations of the
`File` menu, and the `View` menu allows you to change tabulator settings and
the font used for display and printing.

The size (i.e. number of lines) of the log is limited. This limit (500 lines
by default, but you can change this manually with the Windows regedit utility)
is necessary because each evaluation can produce an arbitrary amount of
output, while the text size of a Windows edit control is usually limited to 64
KB.

When the interpreter is currently running, typing `Return` in the last line
(the current input line) of the log sends the line (with the prompt removed)
to the interpreter. In fact, any line in the log window, not only the last
one, can be edited and sent to the interpreter using `Ctrl-Return`. When using
this command, the line is copied down to the end of the log.

The log also maintains an input history for the (non-empty) lines you sent to
the interpreter. When positioned at the input line, you can browse through
this history using the `Ctrl-Up` and `Ctrl-Down` cursor keys (which recall and
insert the previous and the next input line, respectively). `Ctrl-PgUp` and
`Ctrl-PgDn` go to the first and the last line of the history, respectively.
Finally, you can search the history using the `Shift-Ctrl-Up` and
`Shift-Ctrl-Down` keys which look for the closest previous (resp. next)
history line which matches the text before the current cursor position. The
size of the input history (100 lines by default) can be set using the
`History Size` option of the `Script` menu. Furthermore, the history is stored
in a text file (`PurePadHistory` by default) when PurePad is exited. You can
switch to another history file using the `History File` command in the
`Script` menu.

Locating Source Lines
---------------------

To quickly locate the source line of an error message shown in the log pane,
PurePad provides some keyboard commands and toolbar buttons which can be
invoked from both the source and the log pane.

-   `F3` (`Next Error`, `>` button) finds and displays the position of the
    next source line reference in the log, starting below the current line in
    the log.
-   `Shift-F3` (`Previous Error`, `<` button) finds and displays the position
    of the previous source line reference in the log, starting above the
    current line in the log.
-   `Ctrl-F3` (`Last Error`, `>>` button) finds and displays the position of
    the last source line reference in a sequence of consecutive messages in
    the log, starting at or above the current line in the log.
-   `Shift-Ctrl-F3` (`First Error`, `<<` button) finds and displays the
    position of the first source line reference in a sequence of consecutive
    messages in the log, starting at or above the current line in the log.

Each of these commands opens the file indicated by the message (if it is not
open already) and sets the cursor to the corresponding line. Furthermore, it
marks the message in the log and also displays the source position in the
status line. Please note that since PurePad is a single-document application,
opening a new source file in this manner closes the file currently in the
source pane. If this file has been modified, you will be prompted to save the
file, just as if you used the `File --> Open` command.

Another useful command in the `View` menu, which is often used in conjunction
with the above operations is `To Input Line` (`Esc`) which quickly returns you
to the input (i.e., last) line in the log pane.
