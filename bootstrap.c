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

Obj *THENULL;

int isnull(Obj *o)
{
  return o == THENULL;
}

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
    return THENULL;
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
  else if (peek() == '(') {
    getchar();
    return readpair();
  } else if (peek() == ')') {
    fprintf(stderr, "unbalanced parenthesis\n");
    exit(1);
  } else {
    int c;
    for (i = 0; !isdelimiter(c = getchar()); readbuffer[i++] = c);
    ungetc(c, stdin);
    readbuffer[i] = '\0';
    return makesymbol(readbuffer, i);
  }

  fprintf(stderr, "invalid input\n");
  exit(1);
}

void print(Obj *o);

void printpair(Obj *o)
{
  print(o->data.pair.car);

  if (isnull(o->data.pair.cdr));
  else if (o->data.pair.cdr->type == PAIR) {
    printf(" ");
    printpair(o->data.pair.cdr);
  } else {
    printf(" . ");
    print(o->data.pair.cdr);
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
  THENULL = allocobj();
  THENULL->type = _NULL;
  while (1) {
    printf("> ");
    print(read());
    printf("\n");
  }
  return 0;
}
