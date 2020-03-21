#include "runtime.h"

typedef struct _Node {
  block *scm_val;
  struct _Node *next;
} Node;

Node *INTERNED_SYMBOLS_LIST = NULL;

Node *allocnode(block *scm_val, Node *next)
{
  Node *node = malloc(sizeof(Node));
  node->scm_val = scm_val;
  node->next = next;
  return node;
}

scm allocsymbol(char *name, size_t len)
{
  for (Node *n = INTERNED_SYMBOLS_LIST; n != NULL; n = n->next)
    if (strncmp((char *)n->scm_val->data, name, len) == 0)
      return (scm)n->scm_val;

  block *symbol = malloc(sizeof(scm) * (len + 2));
  symbol->header = TAG(len, headershift, symboltag);
  strncpy((char *)symbol->data, name, len + 1);

  INTERNED_SYMBOLS_LIST = allocnode(symbol, INTERNED_SYMBOLS_LIST);

  return (scm)symbol;
}

scm allocstring(char *str, size_t len)
{
  block *string = malloc(sizeof(scm) * (len + 2));
  string->header = TAG(len, headershift, stringtag);
  strncpy((char *)string->data, str, len + 1);
  return (scm)string;
}

scm allocclosure(void *fp, size_t nfvs)
{
  block *closure = malloc(sizeof(scm) * (nfvs + 2));
  closure->header = closuretag;
  closure->data[0] = (scm)fp;
  return (scm)closure;
}

void print_block(block *scm_val)
{
  if (TAGGED(scm_val->header, headermask, symboltag))
    printf("%s", (char *)scm_val->data);
  else if (TAGGED(scm_val->header, headermask, stringtag))
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
