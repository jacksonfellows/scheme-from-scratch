#include "runtime.h"

scm allocstring(char *str, size_t len)
{
  block *string = malloc(sizeof(scm) * (1 + len));
  string->header = TAG(string->header, headershift, stringtag);
  strncpy((char *)string->data, str, len);
  return (scm)string;
}

scm allocclosure(void *fp, size_t nfvs)
{
  block *closure = malloc(sizeof(scm) * (2 + nfvs));
  closure->header = closuretag;
  closure->data[0] = (scm)fp;
  return (scm)closure;
}

void print_block(block *scm_val)
{
  if (TAGGED(scm_val->header, headermask, stringtag))
    printf("\"%s\"", (char *)scm_val->data);
  else if (TAGGED(scm_val->header, headermask, closuretag))
    printf("#<procedure>");
  else
    printf("#<unknown block %p>", scm_val);
}

void print_scm_val(scm scm_val)
{
  if (TAGGED(scm_val, fxmask, fxtag))
    printf("%ld", (long)scm_val >> fxshift);
  else if (TAGGED(scm_val, bmask, btag))
    printf("#%c", scm_val >> bshift ? 't' : 'f');
  else if (TAGGED(scm_val, cmask, ctag))
    printf("#\\%c", (char)(scm_val >> cshift));
  else if (scm_val == null)
    printf("()");
  else if (TAGGED(scm_val, immask, 0))
    print_block((block *)scm_val);
  else
    printf("#<unknown immediate 0x%016zx>", scm_val);
  printf("\n");
}
