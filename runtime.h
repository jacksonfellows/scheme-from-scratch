#ifndef __SCHEME
#define __SCHEME

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAGGED(val, mask, tag) ((val & mask) == tag)
#define TAG(val, shift, tag) ((val << shift) | tag)

#define immask 15

#define fxshift 1
#define fxmask  1
#define fxtag   1

#define bshift 4
#define bmask  immask
#define btag   6

#define t 22
#define f 6

#define cshift 4
#define cmask  immask
#define ctag   10

#define null 14

typedef size_t scm;

#define headershift 4
#define headermask  15

#define vectortag  0
#define symboltag  1
#define stringtag  2
#define pairtag    3
#define closuretag 4

typedef struct {
  scm header;
  scm data[];
} block;

#define CAR(pair) (((block*)pair)->data[0])
#define CDR(pair) (((block*)pair)->data[1])

#define IS_PAIR(x) (TAGGED(x,immask,0) && TAGGED((((block*)x))->header,headermask,pairtag))

scm allocvector(size_t len);
scm allocsymbol(char *name, size_t len);
scm allocstring(char *str, size_t len);
scm cons(scm car, scm cdr);
scm allocclosure(void *fp, size_t nfvs);

void print_scm_val(scm scm_val);

#endif
