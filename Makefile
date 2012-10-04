PACKAGE=vlmcs
INCLUDES=
OCAMLFIND=ocamlfind

LIB=vlmcs.cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

CMX_FILES=vlmc.cmx trie.cmx
CMO_FILES=$(CMX_FILES:.cmx=.cmo)
CMI_FILES=$(CMX_FILES:.cmx=.cmi)

all: opt byte

opt: $(LIB)
byte: $(LIB_BYTE)

$(LIB): $(CMI_FILES) $(CMX_FILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(CMX_FILES)

$(LIB_BYTE): $(CMI_FILES) $(CMO_FILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(CMO_FILES)

install: all
	$(OCAMLFIND) install $(PACKAGE) META \
	$(LIB) $(LIB:.cmxa=.a) $(CMI_FILES) $(LIB_BYTE)

uninstall: dummy
	$(OCAMLFIND) remove $(PACKAGE)


.suffixes: .ml .mli .cmx .cmi .cmo

%.cmi: %.mli
	$(OCAMLFIND) ocamlc $(INCLUDES) -annot -c $<

%.cmx: %.ml
	$(OCAMLFIND) ocamlopt -inline 10000 -verbose $(INCLUDES) -annot -c $<

%.cmo: %.ml
	$(OCAMLFIND) ocamlc -verbose $(INCLUDES) -annot -c $<

clean:
	rm -f *.cm* *.annot *.x *.byte *.o
