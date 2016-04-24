declare name "wahwah";
declare author "Julius Smith";
declare description "CryBaby effect from the Faust effect library";
declare version "1.0";

import("effect.lib"); 

drywet(fx) = _,_ <: *(d), *(d) , (fx : *(w), *(w)) :> _,_
with {
  d = (1-w);
  w = hslider("dry-wet[style:knob]", 0, 0, 1, 0.01);
};

wahwah = vgroup("", drywet(par(i,2, crybaby(wah))))
with {
  wah = hslider("wah[style:knob]",0.8,0,1,0.01) : automat(bps, 10, 0.0);
  // not sure what units these are...
  bps = hslider("tempo[style:knob]", 360, 0, 1920, 1);
};

process = wahwah;
