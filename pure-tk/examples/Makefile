
# GNU Makefile to compile the examples in this directory to native executables.

GTK_LIBS = $(shell pkg-config --libs gtk+-2.0)

progs = tk_hello tk_examp earth uiexample

all: $(progs)

# This one needs special treatment since we need to link in the GTK libs.
uiexample: uiexample.pure uiexample.glade
	pure -c $< -o $@ $(GTK_LIBS)

%: %.pure %.tcl
	pure -c $< -o $@

%: %.pure
	pure -c $< -o $@

clean:
	rm -f $(progs)
