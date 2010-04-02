
GTK_LIBS = $(shell pkg-config --libs gtk+-2.0)

all: pong

pong: pong.pure pong.tcl
	pure -c $< -o $@ $(GTK_LIBS)

html: pong.html

pong.html: README
	rst2html.py $< $@

clean:
	rm -f pong
