all:
	ocamlopt -inline 10 unix.cmxa model.ml -o model