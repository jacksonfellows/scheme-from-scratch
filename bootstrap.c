#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

typedef enum { INT, SYMBOL, PAIR, _NULL } Type;

typedef struct sObj {
  Type type;
  union {
    struct {
      int val;
    } fixnum;
    struct {
      char *name;
    } symbol;
    struct {
      struct sObj *car;
      struct sObj *cdr;
    } pair;
  } data;
} Obj;

Obj *allocobj()
{
  Obj *o = malloc(sizeof(Obj));
  return o;
}

Obj *makefixnum(int x)
{
  Obj *fixnum = allocobj();
  fixnum->type = INT;
  fixnum->data.fixnum.val = x;
  return fixnum;
}

Obj *makesymbol(char *buffer, int len) {
  Obj *symbol = allocobj();
  symbol->type = SYMBOL;
  symbol->data.symbol.name = malloc(len+1);
  strncpy(symbol->data.symbol.name, buffer, len+1);
  return symbol;
}

Obj *car(Obj *pair)
{
  return pair->data.pair.car;
}

Obj *cdr(Obj *pair)
{
  return pair->data.pair.cdr;
}

Obj *cons(Obj *car, Obj *cdr)
{
  Obj *pair = allocobj();
  pair->type = PAIR;
  pair->data.pair.car = car;
  pair->data.pair.cdr = cdr;
  return pair;
}

#define MAKE_CONSTANT_SYMBOL(str) makesymbol(str, sizeof(str))

#define DECLARE_CONSTANT(name) \
  Obj *the##name;\
  int is##name(Obj *o)\
  {\
  return o == the##name;\
  }

DECLARE_CONSTANT(null)
DECLARE_CONSTANT(quote)

void initconstants()
{
  thenull = allocobj();
  thenull->type = _NULL;

  thequote = MAKE_CONSTANT_SYMBOL("quote");
}

int peek()
{
  int c = getchar();
  ungetc(c, stdin);
  return c;
}

int isdelimiter(int c)
{
  return isspace(c) || c == '(' || c == ')';
}

void skipwhitespace()
{
  int c;
  while (isspace(c = getchar()));
  ungetc(c, stdin);
}

Obj *read();

Obj *readpair()
{
  skipwhitespace();
  if (peek() == ')') {
    getchar();
    return thenull;
  }

  Obj *pair = allocobj();
  pair->type = PAIR;
  pair->data.pair.car = read();

  skipwhitespace();
  if (peek() == '.') {
    getchar();
    pair->data.pair.cdr = read();
    skipwhitespace();
    if (getchar() != ')') {
      fprintf(stderr, "invalid use of .\n");
      exit(1);
    }
  } else
    pair->data.pair.cdr = readpair();
  return pair;
}

Obj *read()
{
#define BUFFER_LEN 1024
  char readbuffer[BUFFER_LEN];
  int i;

  if (scanf("%d", &i)) /* skips whitespace */
    return makefixnum(i);

  int c = getchar();
  switch (c) {
  case '(':
    return readpair();
  case ')':
    fprintf(stderr, "unbalanced parenthesis\n");
    exit(1);
  case '\'':
    return cons(thequote, read());
  default:
    ungetc(c, stdin);
    for (i = 0; !isdelimiter(c = getchar()); readbuffer[i++] = c);
    ungetc(c, stdin);
    readbuffer[i] = '\0';
    return makesymbol(readbuffer, i);
  }

  fprintf(stderr, "invalid input\n");
  exit(1);
}

Obj *eval(Obj *o)
{
  switch (o->type) {
  case INT:
    return o;
  case PAIR:
    if (isquote(car(o)))
      return cdr(o);
  default:
    fprintf(stderr, "cannot eval object\n");
    exit(1);
  }
}

void print(Obj *o);

void printpair(Obj *o)
{
  print(car(o));

  if (isnull(cdr(o)));
  else if (cdr(o)->type == PAIR) {
    printf(" ");
    printpair(cdr(o));
  } else {
    printf(" . ");
    print(cdr(o));
  }
}

void print(Obj *o)
{
  switch (o->type) {
  case INT:
    printf("%d", o->data.fixnum.val);
    break;
  case SYMBOL:
    printf("%s", o->data.symbol.name);
    break;
  case _NULL:
    printf("()");
    break;
  case PAIR:
    printf("(");
    printpair(o);
    printf(")");
    break;
  }
}

int main()
{
  initconstants();
  while (1) {
    printf("> ");
    print(eval(read()));
    printf("\n");
  }
  return 0;
}
