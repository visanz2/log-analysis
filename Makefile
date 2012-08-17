all:
	ocamllex LogLexer.mll       
	ocamllex MapLexer.mll
	ocamlyacc LogParser.mly     
	ocamlyacc MapParser.mly     
	ocamlc -c MapAst.ml 
	ocamlc -c LogAst.ml 
	ocamlc -c LogParser.mli
	ocamlc -c MapParser.mli
	ocamlc -c LogLexer.ml
	ocamlc -c MapLexer.ml
	ocamlc -c MapParser.ml
	ocamlc -c LogParser.ml
	ocamlc -c LogC.ml
	ocamlc -o logc str.cma LogLexer.cmo MapLexer.cmo LogParser.cmo MapParser.cmo LogAst.cmo MapAst.cmo LogC.cmo