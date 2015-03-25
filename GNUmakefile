OCAMLBUILD_FLAGS ?=
OCAMLBUILD_FLAGS += -cflags -w,+A-44
OCAMLBUILD_FLAGS += -docflags -charset,utf-8,-stars
OCAMLBUILD_FLAGS += -use-ocamlfind

.PHONY: all clean lib top

all: lib

clean:
	ocamlbuild ${OCAMLBUILD_FLAGS} -clean

lib:
	ocamlbuild ${OCAMLBUILD_FLAGS} ssfc.cma

top: lib
	utop -I _build/
