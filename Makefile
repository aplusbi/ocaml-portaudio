# $Id: Makefile.in 2660 2006-07-18 17:23:31Z dbaelde $

PROGNAME := ocaml-portaudio
DISTFILES := CHANGES COPYING Makefile.in README \
	bootstrap configure configure.ac \
	src/*.mli src/*.ml src/*.c src/Makefile.in src/META.in \
	src/OCamlMakefile \
	examples/*.ml examples/Makefile examples/OCamlMakefile
VERSION = 0.1.3

all clean install uninstall:
	$(MAKE) -C src $@

distclean: clean
	$(MAKE) -C examples clean

doc:
	$(MAKE) -C src htdoc
	mkdir -p doc
	rm -rf doc/html
	mv src/doc/portaudio/html doc
	rm -rf src/doc

dist:
	rm -rf $(PROGNAME)-$(VERSION)
	mkdir $(PROGNAME)-$(VERSION)
	cp -r --parents $(DISTFILES) $(PROGNAME)-$(VERSION)
	tar zcvf ../$(PROGNAME)-$(VERSION).tar.gz $(PROGNAME)-$(VERSION)
	rm -rf $(PROGNAME)-$(VERSION)

.PHONY: dist doc
