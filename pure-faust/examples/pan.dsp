
declare name "pan";
declare description "simple panning control";
declare author "Albert Graef";
declare version "1.0";

import("music.lib");

// control variables

vol = nentry("vol", 1.0, 0, 10, 0.01);	// %
pan = nentry("pan", 0.5, 0, 1, 0.01);	// %

// main program

process	= *(vol) : panner(pan);
