
declare name "subtractive";
declare description "saw wave filtered with resonant lowpass";
declare author "Albert Graef";
declare version "1.0";
declare nvoices "8";

import("music.lib");

// master volume and pan
vol = hslider("/v:[1]/vol [style:knob] [midi:ctrl 7]", 0.3, 0, 1, 0.01);
pan = hslider("/v:[1]/pan [style:knob] [midi:ctrl 8]", 0.5, 0, 1, 0.01);

// modulation (filter parameters)
res	= hslider("/v:[2]/res [unit:dB] [style:knob] [midi:ctrl 1]", 3, 0, 20, 0.1);
cutoff	= hslider("/v:[2]/cutoff [style:knob] [midi:ctrl 2]", 6, 1, 20, 0.1);

// ADSR envelop
attack	= hslider("/v:[3]/[1] attack", 0.01, 0, 1, 0.001);	// sec
decay	= hslider("/v:[3]/[2] decay", 0.3, 0, 1, 0.001);	// sec
sustain = hslider("/v:[3]/[3] sustain", 0.5, 0, 1, 0.01);	// %
release = hslider("/v:[3]/[4] release", 0.2, 0, 1, 0.001);	// sec

// voice parameters
freq	= nentry("/freq", 440, 20, 20000, 1);	// Hz
gain	= nentry("/gain", 1, 0, 10, 0.01);	// %
gate	= button("/gate");			// 0/1

// generic table-driven oscillator with phase modulation

// n	= the size of the table, must be a power of 2
// f	= the wave function, must be defined on the range [0,2*PI]
// freq	= the desired frequency in Hz
// mod	= the phase modulation signal, in radians

tblosc(n,f,freq,mod)	= (1-d)*rdtable(n,wave,i&(n-1)) +
			  d*rdtable(n,wave,(i+1)&(n-1))
with {
	wave	 	= time*(2.0*PI)/n : f;
	phase		= freq/SR : (+ : decimal) ~ _;
	modphase	= decimal(phase+mod/(2*PI))*n;
	i		= int(floor(modphase));
	d		= decimal(modphase);
};

// resonant lowpass

// This is a tweaked Butterworth filter by David Werner and Patrice Tarrabia,
// see http://www.musicdsp.org and http://www.experimentalscene.com for
// details.

// res = resonance in dB above DC gain
// freq = cutoff frequency

lowpass(res,freq)	= f : (+ ~ g) : *(a)
with {
	f(x)	= a0*x+a1*x'+a2*x'';
	g(y)	= 0-b1*y-b2*y';
	a	= 1/db2linear(0.5*res);

	c	= 1.0/tan(PI*(freq/SR));
	c2	= c*c;
	r	= 1/db2linear(2.0*res);
	q	= sqrt(2.0)*r;
	a0	= 1.0/(1.0+(q*c)+(c2));
	a1	= 2.0*a0;
	a2	= a0;
	b1	= 2.0*a0*(1.0-c2);
	b2	= a0*(1.0-q*c+c2);
};

// subtractive synth (saw wave passed through resonant lowpass)

saw(x)	= x/PI-1;

smooth(c) = *(1-c) : +~*(c);

process	= tblosc(1<<16, saw, freq, 0) : ((env,freq,_) : filter) :
	  *(env*gain) : (*(vol:smooth(0.99)) : panner(pan:smooth(0.99)))
with {
  env = gate : adsr(attack, decay, sustain, release);
  filter(env,freq)
      = lowpass(env*res, fmax(1/cutoff, env)*freq*cutoff);
};
