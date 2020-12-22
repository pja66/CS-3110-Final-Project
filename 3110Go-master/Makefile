MODULES= board visual command ai
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=testBoard.byte
MAIN = visual.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

install:
	opam update
	opam upgrade
	opam install cohttp-lwt-unix
	opam install lwt logs extlib
	opam install ANSITerminal

connect : 
	telnet localhost 3110

establish :
	ocamlfind ocamlopt -thread -package lwt,lwt.unix,logs,logs.lwt -linkpkg -o server ./server.ml
	./server
build:
	
	$(OCAMLBUILD) $(OBJECTS)

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)
		
test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

clean:
	ocamlbuild -clean
	rm server.o
	rm server.cmi
	rm server.cmx
	rm server

zip:
	zip goCaml.zip *.ml* _tags Makefile Install.txt LOC.txt

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
