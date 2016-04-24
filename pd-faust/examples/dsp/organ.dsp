
declare name "organ";
declare description "a simple additive synth";
declare author "Albert Graef";
declare version "1.0";

// This declares that the module is an instrument with 16-voice polyphony.
declare nvoices "8";

import("music.lib");

// master controls (volume and stereo panning)
vol = hslider("/v:[1]/vol [style:knob] [midi:ctrl 7]", 0.3, 0, 1, 0.01);
pan = hslider("/v:[1]/pan [style:knob] [midi:ctrl 10]", 0.5, 0, 1, 0.01);

// relative amplitudes of the different partials
amp(1)	= hslider("/v:[2]/amp1 [style:knob]", 1.0, 0, 3, 0.01);
amp(2)	= hslider("/v:[2]/amp2 [style:knob]", 0.5, 0, 3, 0.01);
amp(3)	= hslider("/v:[2]/amp3 [style:knob]", 0.25, 0, 3, 0.01);

// adsr controls
attack	= hslider("/v:[3]/[1] attack", 0.01, 0, 1, 0.001);	// sec
decay	= hslider("/v:[3]/[2] decay", 0.3, 0, 1, 0.001);	// sec
sustain = hslider("/v:[3]/[3] sustain", 0.5, 0, 1, 0.01);	// %
release = hslider("/v:[3]/[4] release", 0.2, 0, 1, 0.001);	// sec

// voice controls
freq	= nentry("/freq", 440, 20, 20000, 1);	// Hz
gain	= nentry("/gain", 0.3, 0, 10, 0.01);	// %
gate	= button("/gate");			// 0/1

// additive synth: 3 sine oscillators with adsr envelop

partial(i) = amp(i+1)*osc((i+1)*freq);

// smoothing filter for vol/pan to avoid zipper noise
smooth(c) = *(1-c) : +~*(c);

process	= sum(i, 3, partial(i))
  * (gate : adsr(attack, decay, sustain, release))
  * gain : (*(vol:smooth(0.99)) : panner(pan:smooth(0.99)));
