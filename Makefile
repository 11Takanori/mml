.PHONY:	build

build:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	jbuilder build main.exe

execute:
	./_build/default/main.exe
 
clean:
	rm -rf _build
	rm -f .merlin lexer.ml parser.ml parser.mli