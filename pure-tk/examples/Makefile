
# GNU Makefile to compile the examples in this directory to native executables.

progs = tk_hello tk_examp earth uiexample

all: $(progs)

%: %.pure %.tcl
	pure -c $< -o $@

%: %.pure
	pure -c $< -o $@

clean:
	rm -f $(progs)
