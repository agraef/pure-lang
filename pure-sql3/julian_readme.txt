A. General

Each Julian date, JD, is represented by a double, where each day has a
value of 1.0. Days are counted from noon of -4713-11-24. Thus, the
Gregorian value of JD 0.0 is -4713-11-24 12:00:00.000. This date
reflects the conventions discussed in julian.pure. [Add that stuff
here.]

B. Conversion functions

This is the guts of the the thing. Four functions are provided:

  str_to_jd, jd_to_str 

  and

  greg_to_jd, jd_to_greg

Here "jd" is a "JD", "str" is string of "YYYY-MM-DD[ hh:mm:ss]" and
"greg" is a "Gregorian tuple" of (Y,M,D [,h,m,s]).

These functions are polymorphic over the four combinations
of long form (i.e., with the h,m,s), short form, UTC time and local
time.

1. Convert UTC time from string to JD to Gregorian tuple and back

> let jd1 = str_to_jd "2010-03-27"; jd1;
2455282.5

> let gr1 = jd_to_greg jd1; gr1;
2010,3,27

> let jd2 = greg_to_jd gr1; jd2;
2455282.5

> jd_to_str jd2;
"2010-03-27"

2. Do the same UTC time conversions with "long" format. 

Note that str_to_jd and gret_to_jd automatically detect long
format. But jd_to_greg and jd_to_string need a second
parameter, "long".

> let jd1 = str_to_jd "2010-03-27 03:30:25"; jd1;
2455282.64612269

> let gr1 = jd_to_greg (jd1,"long"); gr1;
2010,3,27,3,30,25.0

> let jd4 = greg_to_jd gr1; jd2;
2455282.64612269

> jd_to_str (jd2,"lon");
"2010-03-27 03:30:25.000"


3. Short format local time conversions.

In the above, the greg tuples and greg strings are UTC. This is great
for manipulating data in the database, but not so good for interacting
with humans that live in other time zones. The "loc" parameter causes
the conversion functions to read and produce local Gregorian calendar
values (Y,M,D,h,m,s). The underlying JD is always a UTC value (by
definition and as implemented).

An example where the local time zone in Pacific Daylight (Calif)

> let jd2 = str_to_jd ("loc","2010-03-25 12:30:00"); jd2;
2455281.3125

> jd_to_str (jd2,"long");
"2010-03-25 19:30:00.000" //jd2 without "loc" ==> 19:30 in Greenwich

> jd_to_str ("loc", jd2,"long");
"2010-03-25 12:30:00.000" //jd2 with "loc" ==> 12:30 in Calif


Of course this works equally well for jd_to_greg and greg_to_jd, long
format or short format.

C. Other functions

These are self explanitory:

  jd_to_unixtime unixtime_to_jd

  current_jd weekday begmonth endmonth

D. TODO

Some clean up todo's in julianutil.c

Add a bump function. 

bump (Y,M,D,h,m,s) jd
bump (Y,M,D) jd
bump (Y,M) jd
bump Y jd

e.g. bump (0,0,1) is a function that bumps a jd by one day.
     bump (0,0,-1) bumps jds down by one day.

Test

Demo





