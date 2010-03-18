
# This must be set to the prefix under which g2 is installed.
g2prefix = /usr/local
libdir = $(g2prefix)/lib
includedir = $(g2prefix)/include
includefile = $(includedir)/g2.h

%: %.pure
	pure -L$(libdir) -c $^ -o $@

examples = g2_test g2_life

all: $(examples)

clean:
	rm -f $(examples) *.ps *.eps *.emf *.fig *.jpg *.png

# Run this to regenerate the wrapper.

generate:
	rm -f g2.pure
	pure-gen -s "$(includefile);" -o g2.tmp "$(includefile)" -llibg2
	for x in $(includedir)/g2_*.h; do pure-gen -s "$$x;" -o g2.tmp -N "$$x"; done
# This prototype needs cosmetic changes.
	sed -e 's/g2_open_X11X(int, int, int, int, char\*, char\*, char\*, int, int)/g2_open_X11X(int, int, int, int, char\*, char\*, void\*, int, int)/' < g2.tmp > g2.pure
	rm -f g2.tmp
