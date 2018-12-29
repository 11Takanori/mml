.PHONY:	build

build:
	ocamllex src/lexer.mll
	ocamlyacc src/parser.mly
	cd src && dune build main.exe

execute:	build
	./_build/default/src/main.exe
 
clean:
	dune clean
	rm -f lexer.ml parser.ml parser.mli

test:
	dune runtest
