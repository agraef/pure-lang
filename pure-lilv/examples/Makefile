
objects = synth
LIBS = $(shell pkg-config --libs lilv-0)

all: $(objects)

synth: synth.pure
	pure -c synth.pure -o synth $(LIBS) -lportaudio

clean:
	rm -f $(objects)
