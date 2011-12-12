// simple linear crossfade for a stereo signal

import("math.lib");

switch = checkbox("switch");
time = nentry("time", 0.2, 0, 10, 0.01); // time to xfade to other signal

xfade(x,y) = (1-c)*x+c*y with {
  // time -> #samples, assume at least 1
  n = SR*time+(time==0.0);
  c = fade(n, switch);
  // fade into second signal for x=1, back to first signal otherwise
  fade(n,x) = abs (x-ramp(n,x));
  // ramp going from 1 to 0, triggered by any change in x
  ramp(n,x) = abs (x-x')*n : (+ ~ (-(1) : max(0))) : /(n);
};

process(x1,y1,x2,y2) = xfade(x1,x2), xfade(y1,y2);
