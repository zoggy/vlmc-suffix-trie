PACKAGE=vlmcs
INCLUDES=-I laws -I +functory
OCAMLFIND=ocamlfind
OCAMLFLAGS= -g $(INCLUDES) -annot
SYS_LIBS=unix.cmxa functory.cmxa dynlink.cmxa
SYS_LIBS_BYTE=$(SYS_LIBS:.cmxa=.cma)

LIB=vlmcs.cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

CMX_FILES=vlmc.cmx trie.cmx vlmc_zero_laws.cmx vlmc_laws.cmx
CMO_FILES=$(CMX_FILES:.cmx=.cmo)
CMI_FILES=$(CMX_FILES:.cmx=.cmi)

VLMC_EXP=vlmc-exp
VLMC_EXP_BYTE=$(VLMC_EXP).byte

TOOLS=$(VLMC_EXP) $(VLMC_EXP_BYTE)

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
	$(OCAMLFIND) ocamlopt $(OCAMLFLAGS)  -o $@ -linkall $(SYS_LIBS) $^

$(VLMC_EXP_BYTE): $(LIB_BYTE) vlmc_exp.cmo
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) -o $@ -linkall $(SYS_LIBS_BYTE) $^

LAWS_CMXFILES=\
	laws/prob1.cmx \
	laws/comb_fact.cmx \
	laws/comb_log.cmx \
	laws/expo.cmx
LAWS_CMIFILES=$(LAWS_CMXFILES:.cmx=.cmi)
LAWS_CMOFILES=$(LAWS_CMXFILES:.cmx=.cmo)
LAWS_CMXSFILES=$(LAWS_CMXFILES:.cmx=.cmxs)

laws: $(LAWS_CMXSFILES) $(LAWS_CMOFILES)

.suffixes: .ml .mli .cmx .cmi .cmo .cmxs

%.cmi: %.mli
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLFIND) ocamlopt $(OCAMLFLAGS) -inline 10000 -verbose -c $<

%.cmxs: %.ml
	$(OCAMLFIND) ocamlopt $(OCAMLFLAGS) -inline 10000 -shared -o $@ $<

%.cmo: %.ml
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS)  -verbose -c $<

clean:
	rm -f *.cm* *.annot $(TOOLS) *.o
	rm -f laws/*.cm* laws/*.o

.depend depend:
	ocamldep *.ml *.mli > .depend

include .depend