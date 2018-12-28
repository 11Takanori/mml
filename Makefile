.PHONY:	build

build:
	ocamllex lexer.mll
	ocamlyacc parser.mly
	dune build main.exe

execute:	build
	./_build/default/main.exe
 
clean:
	dune clean
	rm -f lexer.ml parser.ml parser.mli