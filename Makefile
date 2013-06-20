#################################################################################
#                Vlmc-suffix-trie                                               #
#                                                                               #
#    Copyright (C) 2012 Institut National de Recherche en Informatique          #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License version 3             #
#    or later as published by the Free Software Foundation.                     #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU General Public License for more details.                               #
#                                                                               #
#    You should have received a copy of the GNU General Public License          #
#    along with this program; if not, write to the Free Software Foundation,    #
#    Inc., 59 Temple Place, Suite 330, Boston, MA                               #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#                                                                               #
#################################################################################

PACKAGE=vlmcs
INCLUDES=-I laws -I +functory
PACKAGES=unix,dynlink,functory
NETPACKAGES=nethttpd
OCAMLFIND=ocamlfind
OCAMLFLAGS= -g $(INCLUDES) -annot

LIB=vlmcs.cmxa
LIB_BYTE=$(LIB:.cmxa=.cma)

CMX_FILES=vlmc_misc.cmx vlmc.cmx trie.cmx vlmc_zero_laws.cmx vlmc_laws.cmx
CMO_FILES=$(CMX_FILES:.cmx=.cmo)
CMI_FILES=$(CMX_FILES:.cmx=.cmi)

VLMC_EXP=vlmc-exp
VLMC_EXP_BYTE=$(VLMC_EXP).byte

TRIE_DRAW=trie-draw
TRIE_DRAW_BYTE=$(TRIE_DRAW).byte

RWALK=random-walk
RWALK_BYTE=$(RWALK).byte

AUTOMATA=automata
AUTOMATA_BYTE=$(AUTOMATA).byte
AUTOMATA_HTTPD=automata-httpd

TOOLS=$(VLMC_EXP) $(TRIE_DRAW) $(RWALK) $(AUTOMATA) $(AUTOMATA_HTTPD)
TOOLS_BYTE=$(VLMC_EXP_BYTE) $(TRIE_DRAW_BYTE) $(RWALK_BYTE) $(AUTOMATA_BYTE)

all: opt byte

opt: $(LIB) $(TOOLS)
byte: $(LIB_BYTE) $(TOOLS_BYTE)

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
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) $(OCAMLFLAGS)  -o $@ -linkpkg -linkall $^

$(VLMC_EXP_BYTE): $(LIB_BYTE) vlmc_exp.cmo
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLFLAGS) -o $@ -linkpkg -linkall $^

$(TRIE_DRAW): $(LIB) trie_draw.cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) $(OCAMLFLAGS)  -o $@ -linkpkg -linkall $^

$(TRIE_DRAW_BYTE): $(LIB_BYTE) trie_draw.cmo
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLFLAGS) -o $@ -linkpkg -linkall $^

$(RWALK): $(LIB) random_walk.cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) $(OCAMLFLAGS)  -o $@ -linkpkg -linkall $^

$(RWALK_BYTE): $(LIB_BYTE) random_walk.cmo
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLFLAGS) -o $@ -linkpkg -linkall $^

$(AUTOMATA): automata.cmx automata_main.cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) $(OCAMLFLAGS)  -o $@ -linkpkg  $^

$(AUTOMATA_BYTE): automata.cmo automata_main.cmo
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(OCAMLFLAGS) -o $@ -linkpkg $^

$(AUTOMATA_HTTPD): automata.cmx automata_httpd.cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES),$(NETPACKAGES) $(OCAMLFLAGS) \
	-o $@ -linkpkg $^

automata_test: automata.cmx automata_test.cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) $(OCAMLFLAGS) \
	-o $@ -linkpkg $^

LAWS_CMXFILES=\
	laws/prob1.cmx \
	laws/comb_fact.cmx \
	laws/comb_log.cmx \
	laws/comb_log_1_2.cmx \
	laws/comb_log_1_4.cmx \
	laws/comb_log_1_8.cmx \
	laws/expo.cmx
LAWS_CMIFILES=$(LAWS_CMXFILES:.cmx=.cmi)
LAWS_CMOFILES=$(LAWS_CMXFILES:.cmx=.cmo)
LAWS_CMXSFILES=$(LAWS_CMXFILES:.cmx=.cmxs)

laws: $(LAWS_CMXSFILES) $(LAWS_CMOFILES)

.suffixes: .ml .mli .cmx .cmi .cmo .cmxs

%.cmi: %.mli
	$(OCAMLFIND) ocamlc -package $(PACKAGES),$(NETPACKAGES) $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLFIND) ocamlopt -package $(PACKAGES),$(NETPACKAGES) $(OCAMLFLAGS) -inline 10000 -verbose -c $<

%.cmxs: %.ml
	$(OCAMLFIND) ocamlopt -package $(PACKAGES),$(NETPACKAGES) $(OCAMLFLAGS) -inline 10000 -shared -o $@ $<

%.cmo: %.ml
	$(OCAMLFIND) ocamlc -package $(PACKAGES),$(NETPACKAGES) $(OCAMLFLAGS)  -verbose -c $<

clean:
	rm -f *.cm* *.annot $(TOOLS) *.o
	rm -f laws/*.cm* laws/*.o

.depend depend:
	ocamldep *.ml *.mli > .depend

include .depend

dummy:

## headers
HEADFILES= Makefile *.ml *.mli laws/*.ml
headers: dummy
	headache -h header.txt -c .headache_config `ls $(HEADFILES) `

noheaders: dummy
	headache -r -c .headache_config `ls $(HEADFILES) `

