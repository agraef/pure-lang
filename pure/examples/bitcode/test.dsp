
/* A simple example to test the new Faust bitcode interface in Pure 0.44. The
   dsp just adds up its two input channels and applies a gain factor which can
   be set using a corresponding control. */

gain = nentry("gain", 0.3, 0, 10, 0.01);

process = + : *(gain);
