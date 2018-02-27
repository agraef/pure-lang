
# Tcl script used by earth.pure. This requires that you have the Tcl/Tk
# bindings of VTK (http://www.vtk.org/) installed. This script illustrates:
# - How to create a basic VTK rendering pipeline and use it in Pure.
# - How to check whether Gnocl is available and to configure the GUI to use
#   either that or plain Tk as a fallback option.

package require vtk
package require vtkinteraction

# If you have Gnocl installed (http://www.gnocl.org/) then you'll get a
# somewhat fancier GTK+ GUI, otherwise you'll have to be content with plain
# Tk. Set GTK to 0 below if you always want the Tk GUI anyway.

set GTK 1
if {$GTK && [catch {package require Gnocl}]} { set GTK 0 }

wm withdraw .

# Create the rendering pipeline, which renders an image of the Earth.
# This has been pilfered from the CameraAndSliderWidgets.tcl example in the
# VTK sources. The Earth image used as a texture here (earth.ppm) should be in
# the current directory.

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
  earth SetFileName "earth.ppm"

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

vtkRenderWindowInteractor iren
  iren SetRenderWindow renWin

# Here's how you'd create an embedded rendering window. This only seems to
# work on Linux/X11, however, so this has been removed for the sake of
# cross-platform compatibility.

# vtkTkRenderWidget .ren -width 400 -height 400 -rw renWin; pack .ren
# ::vtk::bind_tk_render_widget .ren

# Supply a minimal GUI.

if {!$GTK} {

# Plain Tk GUI.

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
wm protocol . WM_DELETE_WINDOW ::vtk::cb_exit
wm title . Earth

# Set some reasonable minimum window size.
wm minsize . 400 [winfo reqheight .box.quit]

# Show the main window.
wm deiconify .

# Render the image. VTK will create this in its own toplevel.
renWin Render

} else {

# GTK+ GUI. This requires Gnocl.

set box [gnocl::box -orientation vertical -borderWidth 0]
set top [gnocl::window -title Earth -width 400 -height 30 -child $box \
	     -onDestroy ::vtk::cb_exit]
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

# Render the image. VTK will create this in its own toplevel.
renWin Render

}
