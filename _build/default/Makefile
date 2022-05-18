.PHONY: build

build:
	dune build
	test -e ./cli/bin || ln -s ./cli/bin

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean

doc:
	dune build @docx

zip:
	rm -f sectool.zip
	zip -r sectool.zip . -x@exclude.lst