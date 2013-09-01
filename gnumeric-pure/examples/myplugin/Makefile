
myplugin = myplugin.pure

all: plugin.xml

plugin.xml: $(myplugin)
	pure-gnm $< > $@

clean:
	rm -f plugin.xml

# This target lets you install the plugin.xml file into your personal Gnumeric
# plugin folder, so that the plugin will be recognized by Gnumeric without
# having to modify the plugin search path. Note that we use an absolute
# pathname for the plugin script here so that it is found no matter where in
# the file system it is located.

# NOTE: This requires GNU make and that you have the Gnumeric development
# files installed.
gnmversion=$(shell pkg-config --modversion libspreadsheet-1.12)
plugindir=$(HOME)/.gnumeric/$(gnmversion)/plugins/myplugin

install: $(myplugin)
	test -d $(plugindir) || mkdir -p $(plugindir)
	pure-gnm $(CURDIR)/$< > $(plugindir)/plugin.xml

# Remove the plugin directory created with 'make install' from your personal
# Gnumeric plugin folder.

uninstall:
	rm -rf $(plugindir)
