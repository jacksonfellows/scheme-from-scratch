#ifndef __SCHEME
#define __SCHEME

#include <stdio.h>
#include <stdlib.h>

#define fxshift 1
#define fxmask 1
#define fxtag 1

#define bshift 4
#define bmask 15
#define btag 6

#define t 22
#define f 6

#define cshift 4
#define cmask 15
#define ctag 10

#define null 14

typedef size_t scm;

void print_scm_val(scm scm_val);

#endif
