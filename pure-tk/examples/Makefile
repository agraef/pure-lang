
# GNU Makefile to compile the examples in this directory to native executables.

progs = tk_hello tk_examp earth uiexample

all: $(progs)

uiexample: uiexample.pure
	pure -c $< -o $@ $(shell pkg-config --libs gtk+-2.0)

%: %.pure %.tcl
	pure -c $< -o $@

%: %.pure
	pure -c $< -o $@

clean:
	rm -f $(progs)
