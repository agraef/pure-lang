
declare name "karplus";
declare description "Karplus-Strong string synth";
declare author "Yann Orlarey";
declare version "1.0";
declare nvoices "8";

import("music.lib");

// master volume and pan
vol	= hslider("/v:[1]/vol [style:knob] [midi:ctrl 7]", 0.3, 0, 1, 0.01);
pan	= hslider("/v:[1]/pan [style:knob] [midi:ctrl 8]", 0.5, 0, 1, 0.01);

// modulation (excitator and resonator parameters)
size	= hslider("/v:[2]/samples [style:knob]", 512, 1, 1024, 1); // #samples
dtime	= hslider("/v:[2]/decay time [style:knob]", 4, 0, 10, 0.01); // -60db decay time
bend	= hslider("/v:[3]/pitch bend", 0, -2, 2, 0.01); // pitch bend/semitones

// voice parameters
freq	= nentry("/freq", 440, 20, 20000, 1);	// Hz
gain	= nentry("/gain", 1, 0, 10, 0.01);	// %
gate	= button("/gate");			// 0/1

/* The excitator: */

upfront(x) 	= (x-x') > 0.0;
decay(n,x)	= x - (x>0)/n;
release(n)	= + ~ decay(n);
trigger(n) 	= upfront : release(n) : >(0.0) : +(leak);
leak		= 1.0/65536.0; // avoid denormals on Pentium
excitator	= trigger(size);

/* The resonator: */

average(x)	= (x+x')/2;
att(d,t)	= 1-1/pow(db2linear(60), d/(SR*t));
comb(d,a)	= (+ : fdelay(4096, d-1.5)) ~ (average : *(1.0-a));
resonator(d)	= comb(d,att(d,dtime));

/* DC blocker (see http://ccrma.stanford.edu/~jos/filters/DC_Blocker.html): */

dcblocker(x)	= (x-x') : (+ ~ *(0.995));

/* Karplus-Strong string synthesizer: */

smooth(c)	= *(1-c) : +~*(c);

process	= noise*gain : *(gate : excitator)
	: resonator(SR/(freq*pow(2,bend/12)))
	: dcblocker
	: (*(smooth(0.99, vol)) : panner(smooth(0.99, pan)));
