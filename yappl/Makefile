yappl : 
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex scanner.mll
	ocamlc -c scanner.ml
	ocamlc -c translate.ml
	ocamlc -c yappl.ml
	ocamlc -o  yappl ast.cmo parser.cmo scanner.cmo translate.cmo yappl.cmo
clean :
	rm -f yappl parser.ml parser.mli scanner.ml \
	testall.log *.cmo *.cmi
