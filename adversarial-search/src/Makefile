all: CountStates minimax

minimax : tic_tac_toe.ml
	ocamlbuild minimax.native

CountStates: tic_tac_toe.ml
	ocamlbuild CountStates.native

clean: force_look
	-rm *\~
	-rm -r _build
	-rm *.native

force_look:
	@true
