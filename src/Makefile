# Copyright (C) 2005-2006 Savonet team
# portaudio bindings for OCaml.
#
# by Samuel Mimram and Romain Beauxis

# $Id: Makefile.in 2383 2006-04-09 16:21:04Z smimram $

OCAMLMAKEFILE = OCamlMakefile

OCAMLFIND = /usr/local/bin/ocamlfind
OCAMLFIND_LDCONF = 
OCAMLC = /usr/local/bin/ocamlc.opt
OCAMLOPT = /usr/local/bin/ocamlopt.opt
BEST = byte opt
OCAMLMKTOP = @OCAMLMKTOP@
OCAMLCP = 
OCAMLDEP = /usr/local/bin/ocamldep
OCAMLLEX = @OCAMLLEX@
OCAMLYACC = @OCAMLYACC@
OCAMLDOC = /usr/local/bin/ocamldoc
LATEX = @LATEX@
DVIPS = @DVIPS@
PS2PDF = @PS2PDF@
OCAMLLIBPATH = /usr/local/lib/ocaml

SOURCES = portaudio_stubs.c portaudio.mli portaudio.ml
RESULT = portaudio
OCAMLDOCFLAGS = -stars
LIBINSTALL_FILES = $(wildcard *.mli *.cmi *.cma *.cmxa *.cmx *.a *.so)
ACLIBS =  -lportaudio -lasound -lm -lpthread  
LDFLAGS =   
CLIBS = $(ACLIBS:-l%=%)
LIBDIRS = $(LDFLAGS:-L%=%)
CC = gcc
CFLAGS = -g -O2 -pthread   -Wall -DCAML_NAME_SPACE
CPPFLAGS = 
NO_CUSTOM = yes
OCAMLFLAGS = 
THREADS = 1


all: $(BEST)

byte: byte-code-library

opt: native-code-library

native-code-library: byte-code-library

install: libinstall

uninstall: libuninstall

update: uninstall install

-include $(OCAMLMAKEFILE)
