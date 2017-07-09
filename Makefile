compile:
	ocamlbuild -pkgs lablgtk2 gravity.byte && ./gravity.byte && ocamlbuild -clean
