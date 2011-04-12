
GTK_LIBS = $(shell pkg-config --libs-only-L --libs-only-l gtk+-2.0)

all: pong

pong: pong.pure pong.tcl
	pure -c $< -o $@ $(GTK_LIBS)

html: pong.html

rst2html = $(shell which rst2html 2>/dev/null || which rst2html.py 2>/dev/null || echo rst2html)

pong.html: README
	$(rst2html) $< $@

clean:
	rm -f pong
