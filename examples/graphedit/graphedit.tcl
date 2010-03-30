#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

# $Id: canvas-man.tcl,v 1.11 2004/09/23 19:50:04 baum Exp $

# ensure that "." is the decimal point
unset -nocomplain env(LC_ALL)
set ::env(LC_NUMERIC) "C"

package require Gnocl
package require GnoclCanvas

set box [gnocl::box -orientation vertical]

set filemenu [gnocl::menu]
$filemenu add [gnocl::menuItem -text "%#New" -onClicked {pure new_cb}]
$filemenu add [gnocl::menuItem -text "%#Open" -onClicked {pure open_cb}]
$filemenu add [gnocl::menuItem -text "%#Save" -onClicked {pure save_cb}]
$filemenu add [gnocl::menuItem -text "%#SaveAs" -onClicked {pure save_as_cb}]
$filemenu add [gnocl::menuSeparator]
$filemenu add [gnocl::menuItem -text "%#Quit" -onClicked {pure quit_cb $top}]

set undocmd [gnocl::menuItem -text "%#Undo" -accelerator "<control>Z" -onClicked {pure undo_cb}]
set redocmd [gnocl::menuItem -text "%#Redo" -accelerator "<control>Y" -onClicked {pure redo_cb}]
set cutcmd [gnocl::menuItem -text "%#Cut" -onClicked {pure cut_cb}]
set copycmd [gnocl::menuItem -text "%#Copy" -onClicked {pure copy_cb}]
set pastecmd [gnocl::menuItem -text "%#Paste" -onClicked {pure paste_cb}]
set dupcmd [gnocl::menuItem -text "%__Duplicate" -accelerator "<control>D" -onClicked {pure dup_cb}]
set deletecmd [gnocl::menuItem -text "%#Delete" -onClicked {pure delete_cb}]

set editmenu [gnocl::menu]
$editmenu add $undocmd
$editmenu add $redocmd
$editmenu add [gnocl::menuSeparator]
$editmenu add [gnocl::menuItem -text "%_Select _All" -accelerator "<control>A" -onClicked {pure select_all_cb}]
$editmenu add [gnocl::menuSeparator]
$editmenu add $cutcmd
$editmenu add $copycmd
$editmenu add $pastecmd
$editmenu add $dupcmd
$editmenu add [gnocl::menuSeparator]
$editmenu add $deletecmd

set align S

set topcmd [gnocl::menuRadioItem -text "%__Top" -variable align -onValue S -onToggled {pure align_cb %v}]
set bottomcmd [gnocl::menuRadioItem -text "%__Bottom" -variable align -onValue N -onToggled {pure align_cb %v}]
set leftcmd [gnocl::menuRadioItem -text "%__Left" -variable align -onValue E -onToggled {pure align_cb %v}]
set rightcmd [gnocl::menuRadioItem -text "%__Right" -variable align -onValue W -onToggled {pure align_cb %v}]
set topleftcmd [gnocl::menuRadioItem -text "Top Left" -variable align -onValue SE -onToggled {pure align_cb %v}]
set toprightcmd [gnocl::menuRadioItem -text "Top Right" -variable align -onValue SW -onToggled {pure align_cb %v}]
set bottomleftcmd [gnocl::menuRadioItem -text "Bottom Left" -variable align -onValue NE -onToggled {pure align_cb %v}]
set bottomrightcmd [gnocl::menuRadioItem -text "Bottom Right" -variable align -onValue NW -onToggled {pure align_cb %v}]
set centercmd [gnocl::menuRadioItem -text "%__Center" -variable align -onValue center -onToggled {pure align_cb %v}]

set alignmenu [gnocl::menu]
$alignmenu add $topcmd
$alignmenu add $bottomcmd
$alignmenu add $leftcmd
$alignmenu add $rightcmd
$alignmenu add [gnocl::menuSeparator]
$alignmenu add $topleftcmd
$alignmenu add $toprightcmd
$alignmenu add $bottomleftcmd
$alignmenu add $bottomrightcmd
$alignmenu add [gnocl::menuSeparator]
$alignmenu add $centercmd

set menubar [gnocl::menuBar]
$menubar add [gnocl::menuItem -text "%__File" -submenu $filemenu]
$menubar add [gnocl::menuItem -text "%__Edit" -submenu $editmenu]
$menubar add [gnocl::menuItem -text "%__Align" -submenu $alignmenu]

set toolbar [gnocl::toolBar]
$toolbar add item -text "%#New" -tooltip "Create a new graph" -onClicked {pure new_cb}
$toolbar add item -text "%#Open" -tooltip "Open an existing graph" -onClicked {pure open_cb}
$toolbar add item -text "%#Save" -tooltip "Save the current graph" -onClicked {pure save_cb}
#$toolbar add item -text "%#SaveAs" -tooltip "Save the current graph under a new name" -onClicked {pure save_as_cb}
$toolbar add space
set undotool [$toolbar add item -text "%#Undo" -tooltip "Undo the previous edit operation" -onClicked {pure undo_cb}]
set redotool [$toolbar add item -text "%#Redo" -tooltip "Redo the previously undone edit operation" -onClicked {pure redo_cb}]
$toolbar add space
set cuttool [$toolbar add item -text "%#Cut" -tooltip "Cut the selected subgraph to the clipboard" -onClicked {pure cut_cb}]
set copytool [$toolbar add item -text "%#Copy" -tooltip "Copy the selected subgraph to the clipboard" -onClicked {pure copy_cb}]
set pastetool [$toolbar add item -text "%#Paste" -tooltip "Paste a subgraph from the clipboard" -onClicked {pure paste_cb}]
$toolbar add space
set deletetool [$toolbar add item -text "%#Delete" -tooltip "Delete the selected subgraph" -onClicked {pure delete_cb}]
#$toolbar add space
#$toolbar add item -text "%#Quit" -tooltip "Quit graphedit" -onClicked {pure quit_cb $top}

set spin [gnocl::spinButton -digits 0 -stepInc 10 -pageInc 100 -onValueChanged {pure grid_cb %v} -tooltip "Select the grid size"]
set entry [gnocl::entry -onChanged {pure entry_cb %v} -variable entry_text -tooltip "Enter node and edge labels"]

$toolbar add space
$toolbar add widget $spin
$toolbar add space
$toolbar add widget $entry

set canv [gnocl::canvas -background white -antialiased 1]

set scrolled [gnocl::scrolledWindow -borderWidth 0 -child $canv]

set statusbar [gnocl::statusBar]
$statusbar push "Ready."

#$box add [list $menubar $toolbar]
$box add [list $menubar $toolbar]
$box add [list $scrolled] -expand 1 -fill 1
$box add [list $statusbar]

set top [gnocl::window -title "graphedit" -child $box -onDestroy {pure destroy_cb %w} -onDelete {pure deletew_cb %w} -defaultWidth 500 -defaultHeight 400]
$canv configure -hasFocus 1
