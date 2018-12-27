.PHONY:	build

build:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	dune build main.exe

execute:build
	./_build/default/main.exe
 
clean:
	dune clean
	rm -f lexer.ml parser.ml parser.mli