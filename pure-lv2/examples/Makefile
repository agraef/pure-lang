
sources = $(wildcard *.pure)
targets = $(patsubst %.pure, %.lv2, $(sources))

all: $(targets)

%.lv2: %.pure
	pure2lv2 $<

clean:
	rm -Rf $(targets)
