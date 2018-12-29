.PHONY:	build

build:
	ocamllex src/lexer.mll
	ocamlyacc src/parser.mly
	cd src && dune build main.exe

repl:	build
	./_build/default/src/main.exe

run:	build
	./_build/default/src/main.exe ${FILE_NAME}
 
clean:
	dune clean
	rm -f src/lexer.ml src/parser.ml src/parser.mli

test:
	dune runtest
