

all:: 
	cd parser; make ; cd ..

clean::
	rm -f parser/*.cm[iox] *~ .*~ #*#
	rm -f parser/*.cmi
	rm -f parser/*.cmo
	rm -f parser/*.mli
	rm -f parser/ArgParser.ml
	rm -f parser/MapParser.ml
	rm -f parser/LogParser.ml
	rm -f logC.byte
	rm -f logC.byte.opt 
