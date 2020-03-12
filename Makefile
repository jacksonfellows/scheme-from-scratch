bootstrap : bootstrap.c
	cc -g -Wall -o bootstrap bootstrap.c

%.result : %.scm bootstrap compiler.scm runtime.h runtime.c
	./bootstrap compiler.scm $*.scm | cc -xc - runtime.c && ./a.out > $*.result

TEST_CASES=$(wildcard tests/*.scm)
TEST_RESULTS=$(patsubst tests/%.scm, tests/%.result, $(TEST_CASES))

.PHONY : results
results : $(TEST_RESULTS)

SHELL=/bin/bash

.PHONY : test
test : results
	set -e; for f in $(TEST_RESULTS); do diff $$f $${f%.*}.expected; done
