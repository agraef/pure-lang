
# Generic Makefile for Pure LV2 plugins.

# Use this variable to pass options to pure2lv2, such as -s or -u.
#PURE2LV2_FLAGS = -s

# Installation directory. Use /usr/lib/lv2 or /usr/local/lib/lv2 for
# system-wide, ~/.lv2 for personal installation.
prefix = /usr/local
lv2libdir = $(prefix)/lib/lv2

sources = $(wildcard *.pure)
targets = $(patsubst %.pure, %.lv2, $(sources))

all: $(targets)

%.lv2: %.pure
	pure2lv2 $(PURE2LV2_FLAGS) $<

clean:
	rm -Rf $(targets)

install: $(targets)
	rm -Rf $(addprefix $(DESTDIR)$(lv2libdir)/, $(targets))
	cp -R $(targets) $(DESTDIR)$(lv2libdir)

uninstall:
	rm -Rf $(addprefix $(DESTDIR)$(lv2libdir)/, $(targets))
