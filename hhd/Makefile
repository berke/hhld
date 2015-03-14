export OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = hhd.ml

OTHERSOURCES = Makefile ../hhl/hhl.cmxa
INCDIRS = ../hhl +lablgtk2 
LIBS = str hhl lablgtk lablgnomecanvas
OCAMLDOCFLAGS = -t "Hidden Hidden Designer" -I ./ -sort
RESULT = hhd

all :  nc

include $(OCAMLMAKEFILE)
