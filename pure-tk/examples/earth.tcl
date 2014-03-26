
# Tcl script used by earth.pure. This requires that you have the Tcl/Tk
# bindings of VTK (http://www.vtk.org/) installed.

# This script illustrates various points that might be useful when writing
# your own VTK applications with Pure and Tcl/Tk:

# - How to create a basic VTK rendering pipeline and use it in Pure.

# - How to use Tk and Gnocl to embed a VTK rendering window in a Tk or GTK+
#   application.

# - How to check whether Gnocl is available and to configure the GUI to use
#   either that or plain Tk as a fallback option.

package require vtk
package require vtkinteraction

# If you have Gnocl installed (http://www.gnocl.org/) then you'll get a
# somewhat fancier GTK+ GUI, otherwise you'll have to be content with plain
# Tk. Set GTK to 0 below if you always want the Tk GUI anyway. NOTE: We make
# Tk the default on OS X since there seems to be no way to actually embed the
# VTK rendering window in a Gnocl (X11) widget there.

set GTK 1
set os [lindex [array get tcl_platform os] 1]
if {$os == "Darwin"} { set GTK 0 }
if {$GTK && [catch {package require Gnocl}]} { set GTK 0 }

wm withdraw .

# Create the rendering pipeline, which renders an image of the Earth.
# This has been pilfered from the CameraAndSliderWidgets.tcl example in the
# VTK sources. The Earth image used as a texture here should be be in
# $VTK_DATA_ROOT/Data/earth.ppm. If you only see a solid red sphere instead
# then your installation lacks that file.

vtkPlaneSource plane
  plane SetOrigin 1.0 [expr 3.14159265359 - 0.0001] 0.0
  plane SetPoint1 1.0 [expr 3.14159265359 - 0.0001] 6.28318530719
  plane SetPoint2 1.0 0.0001                        0.0
  plane SetXResolution 19
  plane SetYResolution 9

vtkSphericalTransform transform

vtkTransformPolyDataFilter tpoly
  tpoly SetInputConnection [plane GetOutputPort]
  tpoly SetTransform transform

vtkPNMReader earth
  earth SetFileName "$VTK_DATA_ROOT/Data/earth.ppm"

vtkTexture texture
  texture SetInputConnection [earth GetOutputPort]
  texture InterpolateOn

vtkDataSetMapper mapper
  mapper SetInputConnection [tpoly GetOutputPort]

vtkActor world
  world SetMapper mapper
  world SetTexture texture

vtkRenderer ren1
vtkRenderWindow renWin
  renWin AddRenderer ren1
  ren1 AddActor world
  ren1 SetBackground .1 .2 .4

# Supply a minimal GUI.

if {!$GTK} {

# Plain Tk GUI.

# Callback to give feedback on state changes in the VTK window.
proc key_cb {shift keysym} {
    global wireframe actor interactor
    if {$shift & 8} {
	# These are handled in the GUI (Alt combinations).
    } else {
	# Update the button states.
	if {$keysym == "w"} { set wireframe 1
	} elseif {$keysym == "s"} { set wireframe 0
	} elseif {$keysym == "t"} { set interactor 1
	} elseif {$keysym == "j"} { set interactor 0
	}
    }
}

# The rendering window.
vtkTkRenderWidget .ren -width 400 -height 400 -rw renWin; pack .ren
::vtk::bind_tk_render_widget .ren

frame .box; pack .box
checkbutton .box.rotate -text Rotate -variable rotate \
    -command {pure render_cb $rotate}
checkbutton .box.wireframe -text Wireframe -variable wireframe \
    -command {pure wireframe_cb $wireframe}
checkbutton .box.interactor -text Trackball -variable interactor \
    -command {pure interactor_cb $interactor}
button .box.quit -text Quit -underline 0 -command ::vtk::cb_exit
pack .box.rotate .box.wireframe .box.interactor .box.quit -side left

bind . <Alt-Key-q> ::vtk::cb_exit
bind . <KeyPress> {key_cb %s %K}
wm protocol . WM_DELETE_WINDOW ::vtk::cb_exit
wm title . Earth

# Set some reasonable minimum window size.
wm minsize . 400 [expr 400+[winfo reqheight .box.quit]]

# Show the main window.
wm deiconify .

# Render the image. This must come *after* the embedded rendering window has
# been created, since otherwise VTK will create its own toplevel instead.
renWin Render

} else {

# GTK+ GUI. This requires Gnocl.

# Since we're using a Gnocl socket to embed the VTK rendering window here
# (which is a Tk window and hence can't be a direct child of a GTK+ window),
# we must forward the keyboard events ourselves. The following callbacks take
# care of this.

set mousex 0; set mousey 0

proc motion_cb {x y} {
    global mousex mousey
    set mousex $x; set mousey $y
}

proc key_cb {event shift keycode keysym} {
    global vtkrw mousex mousey wireframe actor interactor
    if {$shift & 8} {
	# These are handled in the GUI (Alt combinations).
    } else {
	::vtk::cb_vtkw_key_binding $vtkrw [$vtkrw GetRenderWindow] \
	    $mousex $mousey [expr $shift & 1] [expr $shift & 4] \
	    $event $keycode $keysym
	# Update the button states.
	if {$event == "Press"} {
	    if {$keysym == "w"} { set wireframe 1
	    } elseif {$keysym == "s"} {	set wireframe 0
	    } elseif {$keysym == "t"} {	set interactor 1
	    } elseif {$keysym == "j"} {	set interactor 0
	    }
	}
    }
}

set box [gnocl::box -orientation vertical -borderWidth 0]
set top [gnocl::window -title Earth -width 400 -height 430 -child $box \
	     -onDestroy ::vtk::cb_exit \
	     -onKeyPress {key_cb Press %s %k %K} \
	     -onKeyRelease {key_cb Release %s %k %K}]
set socket [gnocl::socket]
$box add $socket -expand 1 -fill 1
set bbox [gnocl::box -buttonType 1 -borderWidth 0]
$bbox add [gnocl::checkButton -text Rotate -variable rotate \
	       -onToggled {pure render_cb $rotate}]
$bbox add [gnocl::checkButton -text Wireframe -variable wireframe \
	       -onToggled {pure wireframe_cb %v}]
$bbox add [gnocl::checkButton -text Trackball -variable interactor \
	       -onToggled {pure interactor_cb %v}]
$bbox add [gnocl::button -text %#Quit -onClicked ::vtk::cb_exit]
$box add $bbox
gnocl::update

# Embed a Tk toplevel which is used to host the vtk rendering window.
toplevel .embed -use [format "0x%x" [$socket getID]]
wm minsize .embed 400 400
bind .embed <Motion> {motion_cb %x %y}
pack [frame .embed.f -width 400 -height 400]
update

# The rendering window.
set vtkrw [vtkTkRenderWidget .embed.f.ren -width 400 -height 400 -rw renWin]
pack $vtkrw
::vtk::bind_tk_render_widget $vtkrw

# Render the image. This must come *after* the embedded rendering window has
# been created, since otherwise VTK will create its own toplevel instead.
renWin Render

}
