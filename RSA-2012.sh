#!/bin/sh
./vlmc-exp -o rsa_2012.R -l 250000 -d 25 --cores 5 --measure-step 100 laws/comb_log.cmxs laws/expo.cmxs laws/comb_fact.cmxs
#./vlmc-exp -o rsa_2012_2.R -l 250000 -d 25 --cores 5 --measure-step 100 laws/comb_fact.cmxs laws/expo.cmxs