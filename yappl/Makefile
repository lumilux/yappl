yappl : 
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex scanner.mll
	ocamlc -c scanner.ml
	ocamlc -c translate.ml
clean :
	rm -f microc parser.ml parser.mli scanner.ml \
	testall.log *.cmo *.cmi
