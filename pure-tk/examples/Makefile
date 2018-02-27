
# GNU Makefile to compile the examples in this directory to native executables.

PIC = $(shell pkg-config pure --variable PIC)

progs = tk_hello tk_examp earth uiexample

all: $(progs)

uiexample: uiexample.pure
	pure $(PIC) -c $< -o $@ $(shell pkg-config --libs gtk+-2.0)

%: %.pure %.tcl
	pure $(PIC) -c $< -o $@

%: %.pure
	pure $(PIC) -c $< -o $@

clean:
	rm -f $(progs)
