
/* Emulate Faust's sum function. Actually, the following foldl macro provides
   a generic way to accumulate the results of any binary operation, so the
   same function can be used to emulate prod, seq, par and implement a bunch
   of other useful aggregate functions not built into Faust. */

fold(1,f,x) = x(0);
fold(n,f,x) = f(fold(n-1,f,x),x(n-1));

fsum(n) = fold(n,+);

/* Example: A simple additive synthesizer. */

import("math.lib");
import("music.lib");

// Control parameters.
freq	= nentry("freq", 440, 20, 20000, 1);	// Hz
gain	= nentry("gain", 0.3, 0, 10, 0.01);	// %
gate	= button("gate");			// 0/1
vol	= hslider("vol", 0.3, 0, 10, 0.01);	// %
pan	= hslider("pan", 0.5, 0, 1, 0.01);	// %
attack	= hslider("attack", 0.01, 0, 1, 0.001);	// sec
decay	= hslider("decay", 0.3, 0, 1, 0.001);	// sec
sustain = hslider("sustain", 0.5, 0, 1, 0.01);	// %
release = hslider("release", 0.2, 0, 1, 0.001);	// sec

vmeter(x) = attach(x, env(x) : vbargraph("dB", -96, 10));
env = abs : max(db2linear(-96)) : linear2db : min(10)  : max ~ -(96.0/SR);

// The relative amplitudes of the partials are also defined as a macro here.
a(0) = 1; a(1) = 0.5; a(2) = 0.3;

// The harmonics (sine oscillator scaled by the corresponding amplitude).
h(i) = a(i)*osc((i+1)*freq);

// The output signal.
process = fsum(3, h)
  * (gate : vgroup("Envelop", adsr(attack, decay, sustain, release)))
  * gain : vgroup("Master", *(vol) : panner(pan)) :
  vgroup("Meter", (vgroup("left", vmeter), vgroup("right", vmeter)));
