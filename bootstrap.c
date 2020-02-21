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

#define DECLARE_CONSTANT(name) \
  Obj *the##name;\
  int is##name(Obj *o)\
  {\
  return o == the##name;\
  }

DECLARE_CONSTANT(null)
DECLARE_CONSTANT(quote)
DECLARE_CONSTANT(define)
DECLARE_CONSTANT(ok)

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

Obj *car(Obj *pair)
{
  return pair->data.pair.car;
}

Obj *cdr(Obj *pair)
{
  return pair->data.pair.cdr;
}

#define caar(pair) car(car(pair))
#define cadr(pair) car(cdr(pair))
#define cdar(pair) cdr(car(pair))
#define cddr(pair) cdr(cdr(pair))

#define caaar(pair) car(car(car(pair)))
#define caadr(pair) car(car(cdr(pair)))
#define cadar(pair) car(cdr(car(pair)))
#define caddr(pair) car(cdr(cdr(pair)))
#define cdaar(pair) cdr(car(car(pair)))
#define cdadr(pair) cdr(car(cdr(pair)))
#define cddar(pair) cdr(cdr(car(pair)))
#define cdddr(pair) cdr(cdr(cdr(pair)))

Obj *cons(Obj *car, Obj *cdr)
{
  Obj *pair = allocobj();
  pair->type = PAIR;
  pair->data.pair.car = car;
  pair->data.pair.cdr = cdr;
  return pair;
}

#define PUSH(o, list) list = cons(o, list)

Obj *interned;

Obj *makesymbol(char *buffer, int len) {
  for (Obj *o = interned; !isnull(o); o = cdr(o))
    if (strncmp(car(o)->data.symbol.name, buffer, len) == 0)
      return car(o);

  Obj *symbol = allocobj();
  symbol->type = SYMBOL;
  symbol->data.symbol.name = malloc(len);
  strncpy(symbol->data.symbol.name, buffer, len);

  PUSH(symbol, interned);

  return symbol;
}

#define MAKE_CONSTANT_SYMBOL(str) makesymbol(str, sizeof(str))

Obj *globalenv;

void init()
{
  thenull = allocobj();
  thenull->type = _NULL;

  interned = thenull;
  globalenv = thenull;

  thequote = MAKE_CONSTANT_SYMBOL("quote");
  thedefine = MAKE_CONSTANT_SYMBOL("define");
  theok = MAKE_CONSTANT_SYMBOL("ok");
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
    return cons(thequote, cons(read(), thenull));
  default:
    ungetc(c, stdin);
    for (i = 0; !isdelimiter(c = getchar()); readbuffer[i++] = c);
    ungetc(c, stdin);
    readbuffer[i] = '\0';
    return makesymbol(readbuffer, i+1);
  }

  fprintf(stderr, "invalid input\n");
  exit(1);
}

Obj *lookup(Obj *sym, Obj *env)
{
  for (Obj *o = env; !isnull(o); o = cdr(o))
    if (caar(o) == sym)
      return cdar(o);

  fprintf(stderr, "unbound variable\n");
  exit(1);
}

Obj *eval(Obj *o)
{
  switch (o->type) {
  case INT:
    return o;
  case SYMBOL:
    return lookup(o, globalenv);
  case PAIR:
    if (isquote(car(o))) {
      if (isnull(cdr(o)) || !isnull(cddr(o))) {
	fprintf(stderr, "wrong # of args for quote\n");
	exit(1);
      }
      return cadr(o);
    }
    if (isdefine(car(o))) {
      PUSH(cons(cadr(o), caddr(o)), globalenv);
      return theok;
    }
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
    if (isquote(car(o))) {
      printf("'");
      print(cadr(o));
    } else {
      printf("(");
      printpair(o);
      printf(")");
    }
    break;
  }
}

int main()
{
  init();
  while (1) {
    printf("> ");
    print(eval(read()));
    printf("\n");
  }
  return 0;
}
