
/* Emulate Faust's sum function. Actually, the following foldl macro provides
   a generic way to accumulate the results of any binary operation, so the
   same function can be used to emulate prod, seq, par and implement a bunch
   of other useful aggregate functions not built into Faust. */

fold(1,f,x) = x(0);
fold(n,f,x) = f(fold(n-1,f,x),x(n-1));

fsum(n) = fold(n,+);

/* Example: A simple additive synthesizer. */

import("music.lib");

// The fundamental.
f0 = nentry("freq", 440, 20, 20000, 1);	// Hz

// The relative amplitudes of the partials are also defined as a macro here.
a(0) = 1; a(1) = 0.5; a(2) = 0.3;

// The harmonics (sine oscillator scaled by the corresponding amplitude).
h(i) = a(i)*osc((i+1)*f0);

// The output signal.
process = fsum(3, h);
