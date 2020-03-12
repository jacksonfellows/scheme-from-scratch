#include "runtime.h"

void print_scm_val(scm scm_val)
{
  if ((scm_val & fxmask) == fxtag)
    printf("%ld", (long)scm_val >> fxshift);
  else if ((scm_val & bmask) == btag)
    printf("#%c", scm_val >> bshift ? 't' : 'f');
  else if ((scm_val & cmask) == ctag)
    printf("#\\%c", (char)(scm_val >> cshift));
  else if (scm_val == null)
    printf("()");
  else
    printf("#<unknown 0x%016zx>", scm_val);
  printf("\n");
}
