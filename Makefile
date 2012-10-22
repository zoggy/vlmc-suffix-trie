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
OCAMLFIND=ocamlfind
OCAMLFLAGS= -g $(INCLUDES) -annot
SYS_LIBS=unix.cmxa functory.cmxa dynlink.cmxa
SYS_LIBS_BYTE=$(SYS_LIBS:.cmxa=.cma)

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

TOOLS=$(VLMC_EXP) $(VLMC_EXP_BYTE) $(TRIE_DRAW) $(TRIE_DRAW_BYTE) $(RWALK) $(RWALK_BYTE)

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

$(TRIE_DRAW): $(LIB) trie_draw.cmx
	$(OCAMLFIND) ocamlopt $(OCAMLFLAGS)  -o $@ -linkall $(SYS_LIBS) $^

$(TRIE_DRAW_BYTE): $(LIB_BYTE) trie_draw.cmo
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) -o $@ -linkall $(SYS_LIBS_BYTE) $^

$(RWALK): $(LIB) random_walk.cmx
	$(OCAMLFIND) ocamlopt $(OCAMLFLAGS)  -o $@ -linkall $(SYS_LIBS) $^

$(RWALK_BYTE): $(LIB_BYTE) random_walk.cmo
	$(OCAMLFIND) ocamlc $(OCAMLFLAGS) -o $@ -linkall $(SYS_LIBS_BYTE) $^

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

dummy:

## headers
HEADFILES= Makefile *.ml *.mli laws/*.ml
headers: dummy
	headache -h header.txt -c .headache_config `ls $(HEADFILES) `

noheaders: dummy
	headache -r -c .headache_config `ls $(HEADFILES) `

