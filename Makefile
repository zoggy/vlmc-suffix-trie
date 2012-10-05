PACKAGE=vlmcs
INCLUDES=-I laws
OCAMLFIND=ocamlfind

LIB=vlmcs.cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

CMX_FILES=vlmc.cmx trie.cmx vlmc_zero_laws.cmx vlmc_laws.cmx
CMO_FILES=$(CMX_FILES:.cmx=.cmo)
CMI_FILES=$(CMX_FILES:.cmx=.cmi)

VLMC_EXP=vlmc_exp

TOOLS=$(VLMC_EXP)

all: opt byte $(TOOLS)

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

$(VLMC_EXP): $(LIB) vlmc_exp.cmx
	$(OCAMLFIND) ocamlopt -o $@ -linkall dynlink.cmxa $^

LAWS_CMXFILES=\
	laws/prob1.cmx \
	laws/comb_fact.cmx \
	laws/comb_log.cmx \
	laws/expo.cmx
LAWS_CMIFILES=$(LAWS_CMXFILES:.cmx=.cmi)
LAWS_CMXSFILES=$(LAWS_CMXFILES:.cmx=.cmxs)

laws: $(LAWS_CMXSFILES)

.suffixes: .ml .mli .cmx .cmi .cmo .cmxs

%.cmi: %.mli
	$(OCAMLFIND) ocamlc $(INCLUDES) -annot -c $<

%.cmx: %.ml
	$(OCAMLFIND) ocamlopt -inline 10000 -verbose $(INCLUDES) -annot -c $<

%.cmxs: %.ml
	$(OCAMLFIND) ocamlopt -inline 10000 -shared $(INCLUDES) -annot -o $@ $<

%.cmo: %.ml
	$(OCAMLFIND) ocamlc -verbose $(INCLUDES) -annot -c $<

clean:
	rm -f *.cm* *.annot $(TOOLS) *.o
	rm -f laws/*.cm* laws/*.o

.depend depend:
	ocamldep *.ml *.mli > .depend

include .depend