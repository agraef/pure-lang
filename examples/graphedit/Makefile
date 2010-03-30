
GTK_LIBS = $(shell pkg-config --libs gtk+-2.0)

all: graphedit

graphedit: graphedit.pure graphedit.tcl
	pure -c $< -o $@ $(GTK_LIBS)

html: graphedit.html

graphedit.html: README
	rst2html.py $< $@

clean:
	rm -f graphedit
