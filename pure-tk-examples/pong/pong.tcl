
# ensure that "." is the decimal point
unset -nocomplain env(LC_ALL)
set ::env(LC_NUMERIC) "C"

package require Gnocl
package require GnoclCanvas

##############################################################################

# Defaults which can be overridden in the Pure script, so you should mofify
# them there. The settings below are just to make this script self-contained.

if {[expr ![info exists ai_strength]]} {

# Default AI strength and ball speed.
set ai_strength 80
set ball_speed  50

# Paddle length and ball radius.
set l 40
set r 8

# Alternate keys for moving the paddle.
set upkey "u"
set downkey "d"

}

##############################################################################

set box [gnocl::box -orientation vertical -borderWidth 0]
set canv [gnocl::canvas -background darkgreen -antialiased 1 -onResize {canvasResize %w %W %H}]
set bbox [gnocl::box -spacing big]

proc canvasResize { canv w h } {
  $canv configure -width $w -height $h
  set w2 [expr $w/2]
  set h2 [expr $h/2]
  $canv itemConfigure cline -coords [list $w2 0 $w2 $h]
  $canv itemConfigure lscore -coords [list [expr $w2 - 35] [expr $h - 20]]
  $canv itemConfigure rscore -coords [list [expr $w2 + 35] [expr $h - 20]]
  $canv itemConfigure demotext -coords [list $w2 $h2]
  foreach {x1 y1 x2 y2} [$canv itemCget rpaddle -coords] {
    $canv itemConfigure rpaddle -coords [list $w $y1 $w $y2]
  }
  pure resize_cb $w $h
}

set bnew [gnocl::button -text "%__New" -onClicked {pure new_cb} -tooltip "Start a new game"]
set bpause [gnocl::button -text "%__Pause" -onClicked {pure pause_cb} -tooltip "Pause game"]
set lstrength [gnocl::label  -text "Difficulty:"]
set sstrength [gnocl::spinButton -digits 0 -variable ai_strength -onValueChanged {pure strength_cb %v} -tooltip "AI strength (%)"]
set lspeed [gnocl::label  -text "Speed:"]
set sspeed [gnocl::spinButton -digits 0 -variable ball_speed -onValueChanged {pure speed_cb %v} -tooltip "Ball speed"]
set bquit [gnocl::button -text "%#Quit" -onClicked {pure quit_cb $top} -tooltip "Exit program"]

$bbox add [list $bnew $bpause [gnocl::box -spacing big -children [list $lstrength $sstrength]] [gnocl::box -spacing big -children [list $lspeed $sspeed]] $bquit] -expand 1 -fill 1

$box add [list $canv] -expand 1 -fill 1
$box add [list $bbox] -fill 1

set top [gnocl::window -title "Pure Pong" -child $box -onDelete {gnocl::callback delete $timer} -onDestroy {pure destroy_cb} -defaultWidth 600 -defaultHeight 500 -onKeyPress { keydown %K} -onKeyRelease {keyup %K}]

set input 0

proc keydown { key } {
  global input upkey downkey
  # $canv configure -hasFocus 1
  if { $key == $downkey || $key == "KP_2" } {
    set new 1
  } elseif { $key == $upkey || $key == "KP_8" } {
    set new -1
  } else {
    set new $input
  }
  if { $input != $new } {
    set input $new
    pure input_cb $input
  }
}

proc keyup { key } {
  global input
  # $canv configure -hasFocus 1
  if { $input != 0 } {
    set input 0
    pure input_cb 0
  }
}

set w [$canv cget -width]
set h [$canv cget -height]
set w2 [expr $w/2]
set h2 [expr $h/2]
set y1 [expr $h2-$l/2]
set y2 [expr $h2+$l/2]

set Upkey [string toupper $upkey]
set Downkey [string toupper $downkey]

set demo_text "Demo running, press Alt+N to play\nAlt+P pauses or resumes the game\nKeys $Upkey and $Downkey move the paddle up and down."
set paused_demo_text "Demo is paused.\nPress Alt+P to resume."
set paused_game_text "Game is paused.\nPress Alt+P to resume."
set game_over_text "Game over.\nPress Alt+N for another one."

$canv create line -coords [list $w2 0 $w2 $h] -width 2 -fill white -tags cline
$canv create ellipse -coords [list $w2 $h2 $r] -fill white -tags ball
$canv create text -coords [list [expr $w2 - 35] [expr $h - 20]] -text 0 -fontSize 20 -fontWeight bold -fill white -tags lscore
$canv create text -coords [list [expr $w2 + 35] [expr $h - 20]] -text 0 -fontSize 20 -fontWeight bold -fill white -tags rscore
$canv create text -coords [list $w2 $h2] -text $demo_text -justify center -fontSize 16 -fill red -tags demotext
$canv create line -coords [list 0  $y1 0  $y2] -width 10 -fill white -tags lpaddle
$canv create line -coords [list $w $y1 $w $y2] -width 10 -fill white -tags rpaddle

$canv configure -hasFocus 1

set timer [gnocl::callback create {pure timer_cb} -interval 40 -priority -1]
#gnocl::mainLoop
