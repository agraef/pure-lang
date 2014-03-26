
package require vtk
package require vtkinteraction

vtkVersion vtkversion
set vtkversion [vtkversion GetVTKMajorVersion]
set os [lindex [array get tcl_platform os] 1]

# Set this to 1 to show the graph display in a separate VTK window. This is
# the default now, but you can also set this flag to 0 if you prefer to have
# the graph as an additional pane in the scale main window. NOTE: You *always*
# want to leave this option enabled on OS X, since there it is not possible to
# embed the VTK widget in a Gnocl application anyway.
set GRAPHWIN 1
if {$os == "Darwin"} { set GRAPHWIN 1 }

wm withdraw .

# Create a color map for the edge weights.
vtkLookupTable cmap
cmap SetHueRange 0.62 0.05
cmap Build

# These are just defaults which will be overwritten when calling reload_data
# below.
set wmin 0.0
set wmax 1.0
set fact 1.0

# Get the coordinates of the nodes in world coordinates.
proc get_points {} {
    set data [reader GetOutput]
    set n [$data GetNumberOfPoints]
    set res {}
    for {set i 0} {$i<$n} {incr i} {
	lappend res [$data GetPoint $i]
    }
    return $res
}

# Compute the bounding box of the given points.
proc bbox {points} {
    # Yeah, Tcl is clumsy. This would be a two-liner in Pure.
    set xmin 0.0; set ymin 0.0; set zmin 0.0
    set xmax 0.0; set ymax 0.0; set zmax 0.0
    foreach p $points {
	foreach {x y z} $p {
	    if {$x<$xmin} {set xmin $x}
	    if {$y<$ymin} {set ymin $y}
	    if {$z<$zmin} {set zmin $z}
	    if {$x>$xmax} {set xmax $x}
	    if {$y>$ymax} {set ymax $y}
	    if {$z>$zmax} {set zmax $z}
	}
    }
    return [list $xmin $xmax $ymin $ymax $zmin $zmax]
}

# Reset the camera.
proc reset_camera {} {
    # Looks like we have to reset the camera twice, don't ask me why.
    [ren1 GetActiveCamera] SetRoll 0.0
    ren1 ResetCamera
    set bb [bbox [get_points]]
    foreach {xmin xmax ymin ymax zmin zmax} $bb {
	set x [expr ($xmax+$xmin)/2]
	set y [expr ($ymax+$ymin)/2]
	set z [expr 10*$zmax+1]
	[ren1 GetActiveCamera] SetPosition $x $y $z
    }
    [ren1 GetActiveCamera] SetRoll 0.0
    ren1 ResetCamera
}

# Load new data and update display elements accordingly.
proc reload_data {reset} {
    global data cmap wmin wmax fact lbls title
    # reread the data
    reader SetInputString $data
    reader Modified
    reader Update
    glyph SetScaleFactor $fact
    txt SetInput $title
    if {$title != ""} {
	txt SetVisibility 1
    } else {
	txt SetVisibility 0
    }
    # update the color map
    graph_mapper SetScalarRange $wmin $wmax
    # update the node labels
    captions $lbls
    if {$reset} {reset_camera}
}

# Annotate the nodes with the given text labels.
set captions {}
proc captions {labels} {
    global captions
    set n [llength $captions]
    set m [llength $labels]
    if {$m > $n} {
	# Need some more captions.
	for {set i $n} {$i < $m} {incr i} {
	    set c [vtkCaptionActor2D New]
	    # There doesn't seem to be a direct way to set the actual font
	    # size, but at least we can set the height of the bounding box of
	    # the annotation (as a percentage of the viewport height?).
	    $c BorderOff
	    $c LeaderOff
	    $c SetPosition 0 0
	    [$c GetTextActor] SetTextScaleModeToNone
	    set tprop [$c GetCaptionTextProperty]
	    $tprop ShadowOff
#	    $tprop SetFontSize 12
	    ren1 AddActor2D $c
	    lappend captions $c
	}
    }
    # Create the new captions.
    foreach point [get_points] label $labels c $captions {
	if {$point != {} && $label != {} && $c != {}} {
	    foreach {x y z} $point {
		$c SetCaption $label
		$c SetAttachmentPoint $x $y $z
		$c SetVisibility 1
	    }
	} else {
	    $c SetVisibility 0
	}
    }
}

# Create the rendering pipeline.

set data ""

# Input data and the main actor.
vtkUnstructuredGridReader reader
    reader SetInputString $data
    reader ReadFromInputStringOn
vtkDataSetToDataObjectFilter weight_data
    weight_data SetInputConnection [reader GetOutputPort]
    weight_data CellDataOn
vtkDataSetMapper graph_mapper
    graph_mapper SetInputConnection [reader GetOutputPort]
    graph_mapper SetLookupTable cmap
    graph_mapper SetScalarRange $wmin $wmax
vtkActor graph
    graph SetMapper graph_mapper

# Node glyphs.
vtkSphereSource bullet
    bullet SetRadius 0.01
    bullet SetThetaResolution 50
    bullet SetPhiResolution 50
vtkGlyph3D glyph
    glyph SetInputConnection  [reader GetOutputPort]
    glyph SetSourceConnection [bullet GetOutputPort]
    glyph ScalingOn
vtkPolyDataMapper glyph_mapper
    glyph_mapper SetInputConnection [glyph GetOutputPort]
vtkActor node_glyphs
    node_glyphs SetMapper glyph_mapper
    [node_glyphs GetProperty] SetColor 0.0 0.79 0.34

# Automatic node labels. FIXME: Apparently VTK supports only the rendering of
# data in a direct fashion, so we do general text labels as captions, see
# above.
# vtkIdFilter node_ids
#     node_ids SetInputConnection [reader GetOutputPort]
#     node_ids PointIdsOn
# vtkLabeledDataMapper node_label_mapper
#     node_label_mapper SetInputConnection [node_ids GetOutputPort]
# #    node_label_mapper SetLabelFormat "%g"
# vtkActor2D node_labels
#     node_labels SetMapper node_label_mapper
#     node_labels SetVisibility 0

# Edge labels.
vtkCellCenters cc
    cc SetInputConnection [reader GetOutputPort]
vtkLabeledDataMapper edge_label_mapper
    edge_label_mapper SetInputConnection [cc GetOutputPort]
    edge_label_mapper SetLabelFormat "%5.4g"
    edge_label_mapper SetLabelModeToLabelScalars
    # Get rid of warnings about empty edge sets.
    edge_label_mapper GlobalWarningDisplayOff
vtkActor2D edge_labels
    edge_labels SetMapper edge_label_mapper    
    edge_labels SetVisibility 0

# Text display (used to display various information).
vtkTextActor txt
    [txt GetPositionCoordinate] \
                     SetCoordinateSystemToNormalizedViewport
    [txt GetPositionCoordinate] SetValue 0.1 0.95
    txt SetTextScaleModeToNone
    txt SetInput ""
    set txtprop [txt GetTextProperty]
    $txtprop BoldOn
    $txtprop SetFontSize 12
    txt SetVisibility 0

# Create the color legend for the edge weights.
vtkScalarBarActor scalarBar
    scalarBar SetLookupTable [graph_mapper GetLookupTable]
    # configure position, orientation and size
    [scalarBar GetPositionCoordinate] \
                     SetCoordinateSystemToNormalizedViewport
    [scalarBar GetPositionCoordinate] SetValue 0.1 0.01
    scalarBar SetOrientationToHorizontal
    scalarBar SetWidth 0.8
if {$vtkversion >= 6} {
# YMMV, but for me the title text on the color legend looks awfully huge with
# VTK 6.x. There doesn't seem to be an easy way to set the size manually, so
# we adjust the vertical size of the bar here to make it look a little nicer.
    scalarBar SetHeight 0.07
} else {
    scalarBar SetHeight 0.12
}

# Create the renderer and the render window. This needs to be done before we
# can create the axes (see below).
vtkRenderer ren1
  ren1 AddActor graph
  ren1 AddActor node_glyphs
#  ren1 AddActor node_labels
  ren1 AddActor edge_labels
  ren1 AddActor txt
  ren1 AddActor scalarBar
  ren1 SetBackground .1 .2 .4
vtkRenderWindow renWin
  renWin AddRenderer ren1

# Text property for axes titles and labels.
vtkTextProperty tprop
    tprop SetColor 1 1 1
    tprop ShadowOn

# Create the axes (initially invisible).
vtkCubeAxesActor2D axes
if {$vtkversion >= 6} {
    axes SetInputConnection [reader GetOutputPort]
} else {
    axes SetInput [reader GetOutput]
}
    axes SetCamera [ren1 GetActiveCamera]
    axes SetLabelFormat "%6.4g"
    axes SetFlyModeToOuterEdges
    axes SetFontFactor 0.8
    axes SetAxisTitleTextProperty tprop
    axes SetAxisLabelTextProperty tprop
    axes SetVisibility 0

# Add to the renderer.
ren1 AddViewProp axes

# GTK+ GUI. This requires Gnocl.
package require Gnocl

# Unfortunately, this addon package is missing in recent Gnocl versions. The
# version included in the 0.9.94e tarball seems to work ok with Gnocl 0.9.94g,
# however. If you don't have this package installed, we'll survive, but then
# the program won't be able to remember its window size.
set GCONF 1
if {$GCONF && [catch {package require GnoclGconf}]} { set GCONF 0 }

# Once Gnocl's WebKit package works, we can use it for browsing the
# documentation. Disabled for now.
set WEBKIT 0
if {$WEBKIT && [catch {package require GnoclWebKit}]} { set WEBKIT 0 }

# For the time being, set this variable to your favourite browser in order to
# read the documentation. If the program exists then it will be invoked on the
# html manual.
set browser "firefox"
if {$os == "Darwin"} { set browser "open" }

if {[info exists browser]} {
    set browser [auto_execok $browser]
}

set helpfile "scale.html"
if {[info exists argv0]} {
    # try to locate the help file
    set dir [file dirname $argv0]
    if {![file exists $argv0]} {
	set progname [auto_execok $argv0]
	if {$progname != ""} {
	    set dir [file dirname $progname]
	}
    }
    foreach d [list $dir [file join $dir "doc"] \
		   [file join $dir "../share/doc/scale"]] {
	set fname [file join $d "scale.html"]
	if {[file exists $fname]} {
	    set helpfile $fname
	    break
	}
    }
}
set helpfile [file normalize $helpfile]

set wd 500
set ht 400

proc error_dg {msg} {
    gnocl::dialog -title "Error" -type error -child \
	[gnocl::label -selectable 1 -wrap 1 -text $msg]
}

proc info_dg {msg} {
    gnocl::dialog -title "Information" -type info -child \
	[gnocl::label -selectable 1 -wrap 1 -text $msg]
}

proc question_dg {msg} {
    gnocl::dialog -title "Question" -type question -child \
	[gnocl::label -selectable 1 -wrap 1 -text $msg] \
	-buttons {"%#Yes" "%#No"} \
}

proc about_dg {} {
    global about about_text
    set about [gnocl::dialog -title "About" -type info -child \
		   [gnocl::label -wrap 1 -selectable 1 -text $about_text] \
		   -modal 0 -onResponse {$about delete}]
}

proc help_dg {} {
    global WEBKIT about about_text helpfile browser
    if {$WEBKIT && [file exists $helpfile]} {
	# Show the html manual using WebKit.
	set wk [gnocl::webKit -url "file:$helpfile"]
	set about [gnocl::window -title "Help" -child $wk \
		       -width 500 -height 500 -onDestroy {$about delete}]
    } elseif {[info exists browser] && $browser != ""} {
	# Show the html manual using an external browser.
	exec $browser $helpfile &
    } else {
	# Fallback: Show a simple About box.
	set about [gnocl::dialog -title "About" -type info -child \
		       [gnocl::label -wrap 1 -selectable 1 -text $about_text] \
		       -modal 0 -onResponse {$about delete}]
    }
}

set mts_realtime 1
set mts_encoding "2-byte"
set mts_basetone 9

proc mts_dg {} {
    global mts_realtime mts_encoding mts_basetone
    set box [gnocl::box -orientation vertical -spacing big]
    set cbox [gnocl::box -orientation horizontal -spacing big]
    $cbox add [gnocl::checkButton -text "%__Realtime" -variable mts_realtime \
		   -tooltip "Select realtime mode"] \
	-expand 1 -fill 0 -align center
    $cbox add [gnocl::comboBox -variable mts_encoding \
		   -items {"1-byte" "2-byte"} \
		   -tooltip "Pick the type of the encoding"] \
	-expand 1 -fill 0 -align center
    set base [gnocl::spinButton -variable mts_basetone \
		  -upper 11 -digits 0 \
		  -tooltip "Choose the reference tone of the scale"]
    # This doesn't seem to work in older gnocl versions.
    #$cbox add [gnocl::label -mnemonicText "_Base:" -mnemonicWidget $base]
    $cbox add [gnocl::label -text "Base:"]
    $cbox add $base -expand 1 -fill 0 -align center
    $box add [gnocl::label -widthChars 60 -wrap 1 -text \
"This dialog allows you to save the scale as a binary sysex file in\
one of the octave-based formats of the MIDI Tuning Standard.\
The available options are:\n\
- <b>Realtime</b>: tuning changes affect already sounding notes\n\
- <b>Encoding</b>: 1-byte or 2-byte format, affects range and precision\n\
- <b>Base</b>: reference tone of the tuning which will be at 0 ct \(0..11\)"]
    $box add $cbox
    gnocl::dialog -title "Choose MTS Format" -child $box \
	-buttons {"%#Save" "%#Cancel"} \
}

set lastfile ""
set filename ""
set dirname [pwd]

proc new_file {fname} {
    global lastfile dirname filename
    set lastfile $fname
    set filename [file tail $fname]
    if {$fname != ""} {set dirname [file dirname $fname]}
}

proc open_dg {} {
    global dirname
    return [gnocl::fileChooserDialog -title "Open Scale File" \
		-currentFolder $dirname -action open]
}

proc save_dg {} {
    global dirname filename
    return [gnocl::fileChooserDialog -title "Save Scale File" \
		-currentFolder $dirname -currentName $filename -action save]
    return $fname
}

proc cell_color {v} {
    global dmax
    if {$v > $dmax} {
	return "-foreground red"
    } else {
	return "-foreground black"
    }
}

proc cell_select {p c} {
    global list
    $list cellConfigure $p $c -value [expr ![$list get $p $c]]
}

proc init_list {} {
    global list sbox
    set list [gnocl::tree \
      -titles {"Select" "#" "Pitch" "Factors" "Cent" "Weight" "Note"
	  "Interval"} \
      -types {boolean integer string string float float string string} \
      -onRowCollapsed {pure collapse_cb %p} -onRowExpanded {pure expand_cb %p}]
    $list columnConfigure 0 -onToggled {cell_select %p 0}
    $list columnConfigure 1 -align right
    $list columnConfigure 2 -align right
    $list columnConfigure 3 -align left
    $list columnConfigure 4 -align right
    $list columnConfigure 5 -align right
# This breaks callbacks when returning to the View tab. Gnocl bug?
# $list columnConfigure 5 -onCellData {cell_color %v}
    $sbox addBegin $list -expand 1 -fill 1
}

proc reinit_list {} {
    global list
    $list delete
    init_list
}

proc fini {} {
    global GCONF top
    if {$GCONF} {
	set w [$top cget -width]
	set h [$top cget -height]
	gnocl::gconf set /apps/scale/geom [list $w $h]
    }
    pure fini_cb
    ::vtk::cb_exit
}

# check for saved gconf settings
set geom {}
if {$GCONF} {set geom [gnocl::gconf get /apps/scale/geom]}
if {$geom == {}} {
    # provide some reasonable defaults
    set mywd [expr $wd+10]; set myht [expr $ht+180]
} else {
    foreach {w h} $geom {
	set mywd $w; set myht $h
    }
}

set box [gnocl::box -orientation vertical -borderWidth 0]
set top [gnocl::window -title "Unnamed - scale" \
	     -width $mywd -height $myht -child $box \
	     -onKeyPress {pure key_cb %s %K} -onDestroy fini]
set notebook [gnocl::notebook]
$box add $notebook -expand 1 -fill 1
# If the $GRAPHWIN option is unset, we create an additional vbox here which
# holds both the socket for the graph display and the strip with the graph
# controls; these eventually go into a separate Graph tab in the notebook (see
# below).
if {!$GRAPHWIN} {
    set vbox [gnocl::box -orientation vertical -borderWidth 0]
    set socket [gnocl::socket]
    $vbox add $socket -expand 1 -fill 1
}
set cbox [gnocl::box]
#$cbox add [gnocl::checkButton -text "%__Trackball" -variable interactor \
#	       -value 1 -onToggled {pure interactor_cb %v} \
#	       -tooltip "Trackball/joystick mouse mode"] \
#    -expand 1 -fill 0 -align center
set drawbut [gnocl::button -text %__Draw -onClicked {pure draw_cb} \
		 -icon %#Refresh \
		 -tooltip "Draw scale and update scale information"]
$cbox add $drawbut -expand 1 -fill 0 -align center
$cbox add [gnocl::checkButton -text "%__Axes" -variable axes \
	       -onToggled {pure axes_cb %v} \
	       -tooltip "Show axes"] \
    -expand 1 -fill 0 -align center
$cbox add [gnocl::checkButton -text "%__Labels" -variable node_labels \
	       -value 1 -onToggled {pure node_labels_cb %v $label_type} \
	       -tooltip "Show node labels"] \
    -expand 1 -fill 0 -align center
$cbox add [gnocl::comboBox -variable label_type \
	       -items {Pitch Factors Cent Note Interval Ordinal} \
	       -onChanged {pure node_labels_cb $node_labels %v} \
	       -tooltip "Pick a node label type"] \
    -expand 1 -fill 0 -align center
$cbox add [gnocl::checkButton -text "%__Weights" -variable edge_labels \
	       -onToggled {pure edge_labels_cb %v} \
	       -tooltip "Show edge weights"] \
    -expand 1 -fill 0 -align center
$cbox add [gnocl::comboBox -variable pval \
	   -items {Barlow Euler "Barlow/2" "Euler/2" "Log Barlow" "Log Euler"} \
	   -onChanged {pure pval_cb %v} \
	   -tooltip "Pick an edge weight function"] \
    -expand 1 -fill 0 -align center
# If the $GRAPHWIN option is set, we simply add the graph controls to the main
# box, otherwise it goes into a separate Graph tab in the notebook.
if {$GRAPHWIN} {
    $box add $cbox
} else {
    $vbox add $cbox
    $notebook addPage $vbox "%__Graph"
}
set sbox [gnocl::box -orientation vertical -borderWidth 0]
init_list
set rbox [gnocl::box]
$rbox add [gnocl::button -text "%_Cl_ear" \
	       -onClicked {pure unselect_all_cb} \
	       -tooltip "Clear the selection"] \
    -expand 1 -fill 0 -align center
$rbox add [gnocl::button -text "%_Select _All" \
	       -onClicked {pure select_all_cb} \
	       -tooltip "Select all tuning alternatives"] \
    -expand 1 -fill 0 -align center
$rbox add [gnocl::button -text "%_Re_fresh" -onClicked {pure refresh_cb} \
	       -tooltip "Refresh scale information"] \
    -expand 1 -fill 0 -align center
$rbox add [gnocl::spinButton -variable nmax -value 3 -digits 0 \
	       -tooltip "Number of tuning alternatives"] \
    -expand 1 -fill 0 -align center
$rbox add [gnocl::spinButton -variable tol -value 50 -digits 2 \
	       -tooltip "Tuning tolerance (cent)"] \
    -expand 1 -fill 0 -align center
$rbox add [gnocl::comboBox -variable heur -items {First Best} \
	   -tooltip "Pick a rationalization heuristic"] \
    -expand 1 -fill 0 -align center
set ratbut [gnocl::button -text "%_Ra_tionalize" \
		-onClicked {pure rat_cb} \
	        -tooltip "Rationalize scale"]
$rbox add  $ratbut -expand 1 -fill 0 -align center
$rbox add [gnocl::button -text %__Update -onClicked {pure update_cb} \
	       -tooltip "Update and redraw the scale"] \
    -expand 1 -fill 0 -align center
$sbox addEnd $rbox -fill {1 0}
$notebook addPage $sbox "%_S_cale"
set maxcols 100
set titles [concat "#" "Pitch" [lrepeat $maxcols ""]]
set types  [concat "integer" "string" [lrepeat $maxcols "float"]]
set metric [gnocl::list -titles $titles -types $types]
$metric columnConfigure 0 -align right
for {set i 1} {$i <= $maxcols} {incr i} {
    $metric columnConfigure [expr $i+1] -align right -visible 0
# -onCellData {cell_color %v}
}
$notebook addPage $metric "%__Metric"
set tbox [gnocl::table -homogeneous 0 -rowSpacing small -columnSpacing small]
set scl [gnocl::entry -variable scale \
	       -tooltip "Enter the scale points, ratios and cent values"]
set des [gnocl::entry -variable descr \
	       -tooltip "Enter the scale description"]
$tbox add $scl 0 0 -expand {1 0} -fill {1 0}
$tbox add [gnocl::spinButton -variable dmax -digits 2 \
	       -onValueChanged {pure dmax_cb %v} \
	       -tooltip "Weight threshold"] 1 0 -expand 0
$tbox add [gnocl::button -text %#Help -onClicked {pure help_cb} \
	       -tooltip "Display the manual"] 2 0 -expand 0
$tbox add $des 0 1 -expand {1 0} -fill {1 0}
set progress [gnocl::progressBar]
$tbox add $progress 1 1 -expand 0 -columnSpan 2
$box add $tbox -fill {1 0}
set bbox [gnocl::box -buttonType 1]
$bbox add [gnocl::button -text %#New  -onClicked {pure new_cb} \
	       -tooltip "New scale"]
$bbox add [gnocl::button -text %#Open -onClicked {pure open_cb} \
	       -tooltip "Open scale file"]
$bbox add [gnocl::button -text %#Save -onClicked {pure save_cb} \
	       -tooltip "Save scale file"]
$bbox add [gnocl::button -text %#RevertToSaved -onClicked {pure revert_cb} \
	       -tooltip "Revert scale file"]
$bbox add [gnocl::button -text %_A_bout -onClicked {pure about_cb} \
	       -icon %#About -tooltip "About this program"]
$bbox add [gnocl::button -text %#Quit -onClicked fini \
	       -tooltip "Exit program"]
$box add $bbox
set status [gnocl::statusBar]
$box add $status
$status push "Ready"
gnocl::update

# If $GRAPHWIN is unset, we embed a Tk toplevel here which is used to host the
# VTK render window with the graph display. Otherwise we just create a
# separate toplevel for the display instead. (NOTE: The former doesn't work on
# OS X, so make sure you leave the $GRAPHWIN option enabled there.)
if {$GRAPHWIN} {
    toplevel .embed
    set wd $mywd
    set ht $myht
    wm title .embed "Graph (VTK Window)"
} else {
    toplevel .embed -use [format "0x%x" [$socket getID]]
}
wm minsize .embed $wd $ht
pack [frame .embed.f -width $wd -height $ht] -expand 1 -fill both
update

# The render window.
set vtkrw [vtkTkRenderWidget .embed.f.ren -width $wd -height $ht -rw renWin]
pack $vtkrw -expand 1 -fill both
::vtk::bind_tk_render_widget $vtkrw

# Update the camera so that everything is visible.
ren1 ResetCamera

# Set the interaction style.
[[renWin GetInteractor] GetInteractorStyle] SetCurrentStyleToTrackballCamera

# Render the image. This must come *after* the embedded render window has
# been created, since otherwise VTK will create its own toplevel instead.
renWin Render
