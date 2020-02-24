#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <setjmp.h>

jmp_buf errbuf;

#define ERROR(fmt, ...)				\
  do {						\
    fprintf(stderr, fmt, ##__VA_ARGS__);	\
    longjmp(errbuf, 1);				\
  } while (0);

typedef enum { NUMBER, SYMBOL, PAIR, _NULL, BOOLEAN, PRIM_PROC, COMP_PROC } Type;

typedef struct sObj {
  Type type;
  union {
    struct {
      int val;
    } fixnum;
    struct {
      int val;
    } boolean;
    struct {
      char *name;
    } symbol;
    struct {
      struct sObj *car;
      struct sObj *cdr;
    } pair;
    struct {
      struct sObj *(*proc)(struct sObj *args);
    } primproc;
    struct {
      struct sObj *formals;
      struct sObj *body;
      struct sObj *env;
    } compproc;
  } data;
} Obj;

#define DECLARE_CONSTANT(name)			\
  Obj *the##name;				\
  int is##name(Obj *o)				\
  {						\
    return o == the##name;			\
  }

DECLARE_CONSTANT(null);

DECLARE_CONSTANT(true);
DECLARE_CONSTANT(false);

DECLARE_CONSTANT(quote);
DECLARE_CONSTANT(define);
DECLARE_CONSTANT(ok);
DECLARE_CONSTANT(set);
DECLARE_CONSTANT(if);
DECLARE_CONSTANT(lambda);

Obj *allocobj()
{
  Obj *o = malloc(sizeof(Obj));
  return o;
}

Obj *makefixnum(int x)
{
  Obj *fixnum = allocobj();
  fixnum->type = NUMBER;
  fixnum->data.fixnum.val = x;
  return fixnum;
}

Obj *makeboolean(int x)
{
  Obj *boolean = allocobj();
  boolean->type = BOOLEAN;
  boolean->data.boolean.val = x;
  return boolean;
}

#define TOBOOLEAN(x) x ? thetrue : thefalse

Obj *car(Obj *pair)
{
  return pair->data.pair.car;
}

Obj *cdr(Obj *pair)
{
  return pair->data.pair.cdr;
}

void setcar(Obj *pair, Obj *o)
{
  pair->data.pair.car = o;
}

void setcdr(Obj *pair, Obj *o)
{
  pair->data.pair.cdr = o;
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

#define caaaar(pair) car(car(car(car(pair))))
#define caaadr(pair) car(car(car(cdr(pair))))
#define caadar(pair) car(car(cdr(car(pair))))
#define caaddr(pair) car(car(cdr(cdr(pair))))
#define cadaar(pair) car(cdr(car(car(pair))))
#define cadadr(pair) car(cdr(car(cdr(pair))))
#define caddar(pair) car(cdr(cdr(car(pair))))
#define cadddr(pair) car(cdr(cdr(cdr(pair))))
#define cdaaar(pair) cdr(car(car(car(pair))))
#define cdaadr(pair) cdr(car(car(cdr(pair))))
#define cdadar(pair) cdr(car(cdr(car(pair))))
#define cdaddr(pair) cdr(car(cdr(cdr(pair))))
#define cddaar(pair) cdr(cdr(car(car(pair))))
#define cddadr(pair) cdr(cdr(car(cdr(pair))))
#define cdddar(pair) cdr(cdr(cdr(car(pair))))
#define cddddr(pair) cdr(cdr(cdr(cdr(pair))))

Obj *cons(Obj *car, Obj *cdr)
{
  Obj *pair = allocobj();
  pair->type = PAIR;
  setcar(pair, car);
  setcdr(pair, cdr);
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

Obj *makeprimproc(Obj *(*proc)(Obj *args))
{
  Obj *primproc = allocobj();
  primproc->type = PRIM_PROC;
  primproc->data.primproc.proc = proc;
  return primproc;
}

Obj *makecompproc(Obj *formals, Obj *body, Obj *env)
{
  Obj *compproc = allocobj();
  compproc->type = COMP_PROC;
  compproc->data.compproc.formals = formals;
  compproc->data.compproc.body = body;
  compproc->data.compproc.env = env;
  return compproc;
}

int length(Obj *pair)
{
  int len = 0;
  for (Obj *o = pair; !isnull(o); o = cdr(o), ++len);
  return len;
}

Obj *add(Obj *args)
{
  int sum = 0;
  for (Obj *o = args; !isnull(o); o = cdr(o))
    sum += car(o)->data.fixnum.val;
  return makefixnum(sum);
}

Obj *sub(Obj *args)
{
  int sum;
  if (length(args) == 1)
    return makefixnum(-car(args)->data.fixnum.val);

  sum = car(args)->data.fixnum.val;
  for (Obj *o = cdr(args); !isnull(o); o = cdr(o))
    sum -= car(o)->data.fixnum.val;
  return makefixnum(sum);
}

Obj *mul(Obj *args)
{
  int product = 1;
  for (Obj *o = args; !isnull(o); o = cdr(o))
    product *= car(o)->data.fixnum.val;
  return makefixnum(product);
}

#define FIXNUM_TRUE_FOR_PAIRS(procname, name, op)			\
  Obj* procname(Obj *args)						\
  {									\
    Obj *last = car(args);						\
    for (Obj *o = cdr(args); !isnull(o); last = car(o), o = cdr(o))	\
      if (!(last->data.fixnum.val op car(o)->data.fixnum.val))		\
	return thefalse;						\
    return thetrue;							\
  }

FIXNUM_TRUE_FOR_PAIRS(fixnumeq, "=", ==);
FIXNUM_TRUE_FOR_PAIRS(fixnumgt, ">", >);
FIXNUM_TRUE_FOR_PAIRS(fixnumge, ">=", >=);
FIXNUM_TRUE_FOR_PAIRS(fixnumlt, "<", <);
FIXNUM_TRUE_FOR_PAIRS(fixnumle, "<=", <=);

#define TYPE_PREDICATE(name, _type)		\
  Obj* name##p(Obj *args)			\
  {						\
    return TOBOOLEAN(car(args)->type == _type);	\
  }

TYPE_PREDICATE(null, _NULL);
TYPE_PREDICATE(boolean, BOOLEAN);
TYPE_PREDICATE(symbol, SYMBOL);
TYPE_PREDICATE(number, NUMBER);
TYPE_PREDICATE(pair, PAIR);

Obj *procedurep(Obj *args)
{
  return TOBOOLEAN(car(args)->type == PRIM_PROC || car(args)->type == COMP_PROC);
}

Obj *lengthproc(Obj *args)
{
  return makefixnum(length(car(args)));
}

Obj *carproc(Obj *args)
{
  return caar(args);
}

Obj *cdrproc(Obj *args)
{
  return cdar(args);
}

Obj *setcarproc(Obj *args)
{
  setcar(car(args), cadr(args));
  return theok;
}

Obj *setcdrproc(Obj *args)
{
  setcdr(car(args), cadr(args));
  return theok;
}

Obj *consproc(Obj *args)
{
  return cons(car(args), cadr(args));
}

Obj *list(Obj *args)
{
  return args;
}

Obj *eq(Obj *args)
{
  return TOBOOLEAN(car(args) == cadr(args));
}

#define MAKE_CONSTANT_SYMBOL(str) makesymbol(str, sizeof(str))
#define INIT_CONSTANT_SYMBOL(name) the##name = MAKE_CONSTANT_SYMBOL(#name)
#define MAKE_PRIM_PROC(name, proc) PUSH(cons(MAKE_CONSTANT_SYMBOL(#name), makeprimproc(proc)), globalenv)

Obj *globalenv;

void init()
{
  thenull = allocobj();
  thenull->type = _NULL;

  thetrue = makeboolean(1);
  thefalse = makeboolean(0);

  interned = thenull;
  globalenv = thenull;

  INIT_CONSTANT_SYMBOL(quote);
  INIT_CONSTANT_SYMBOL(define);
  INIT_CONSTANT_SYMBOL(ok);
  theset = MAKE_CONSTANT_SYMBOL("set!");
  INIT_CONSTANT_SYMBOL(if);
  INIT_CONSTANT_SYMBOL(lambda);

  MAKE_PRIM_PROC(null?, nullp);
  MAKE_PRIM_PROC(boolean?, booleanp);
  MAKE_PRIM_PROC(symbol?, symbolp);
  MAKE_PRIM_PROC(number?, numberp);
  MAKE_PRIM_PROC(pair?, pairp);
  MAKE_PRIM_PROC(procedure?, procedurep);

  MAKE_PRIM_PROC(+, add);
  MAKE_PRIM_PROC(-, sub);
  MAKE_PRIM_PROC(*, mul);

  MAKE_PRIM_PROC(=, fixnumeq);
  MAKE_PRIM_PROC(<, fixnumlt);
  MAKE_PRIM_PROC(<=, fixnumle);
  MAKE_PRIM_PROC(>, fixnumgt);
  MAKE_PRIM_PROC(>=, fixnumge);

  MAKE_PRIM_PROC(car, carproc);
  MAKE_PRIM_PROC(cdr, cdrproc);
  MAKE_PRIM_PROC(set-car!, setcarproc);
  MAKE_PRIM_PROC(set-cdr!, setcdrproc);
  MAKE_PRIM_PROC(cons, consproc);
  MAKE_PRIM_PROC(list, list);

  MAKE_PRIM_PROC(length, lengthproc);

  MAKE_PRIM_PROC(eq?, eq);
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
  setcar(pair, read());

  skipwhitespace();
  if (peek() == '.') {
    getchar();
    setcdr(pair, read());
    skipwhitespace();
    if (getchar() != ')')
      ERROR("invalid use of .\n");
  } else
    setcdr(pair, readpair());
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
    ERROR("unbalanced parenthesis\n");
  case '\'':
    return cons(thequote, cons(read(), thenull));
  case '#':
    switch (getchar()) {
    case 't':
      return thetrue;
    case 'f':
      return thefalse;
    default:
      ERROR("# not followed by t or f\n");
    }
  default:
    ungetc(c, stdin);
    for (i = 0; !isdelimiter(c = getchar()); readbuffer[i++] = c);
    ungetc(c, stdin);
    readbuffer[i] = '\0';
    return makesymbol(readbuffer, i+1);
  }

  ERROR("invalid input\n");
}

#define ISTRUTHY !isfalse

Obj *eval(Obj *o, Obj *env);

Obj *evalall(Obj *list, Obj *env)
{
  if (isnull(list))
    return thenull;
  return cons(eval(car(list), env), evalall(cdr(list), env));
}

Obj *makealist(Obj *a, Obj *b)
{
  if (isnull(a))
    return thenull;
  return cons(cons(car(a), car(b)), makealist(cdr(a), cdr(b)));
}

Obj *framelookup(Obj *sym, Obj *frame)
{
  for (Obj *o = frame; !isnull(o); o = cdr(o))
    if (caar(o) == sym)
      return car(o);

  return thenull;
}

Obj *envlookup(Obj *sym, Obj *env)
{
  for (Obj *o = env; !isnull(o); o = cdr(o)) {
    Obj *binding = framelookup(sym, car(o));
    if (!isnull(binding))
      return binding;
  }
  ERROR("unbound variable\n");
}
void define(Obj *sym, Obj *val, Obj *env)
{
  for (Obj *o = env; !isnull(o); o = cdr(o)) {
    Obj *binding = framelookup(sym, car(o));
    if (!isnull(binding)) {
      setcdr(binding, val);
      return;
    }
  }
  Obj *o;
  for (o = car(env); !isnull(cdr(o)); o = cdr(o));
  setcdr(o, cons(cons(sym, val), thenull));
}

void print(Obj *o);

Obj *eval(Obj *o, Obj *env)
{
 tailcall:
  switch (o->type) {
  case NUMBER:
  case BOOLEAN:
    return o;
  case SYMBOL:
    return cdr(envlookup(o, env));
  case PAIR:
    if (isquote(car(o)))
      return cadr(o);
    if (isdefine(car(o))) {
      if (cadr(o)->type == PAIR)
	define(caadr(o), eval(cons(thelambda, cons(cdadr(o), cddr(o))), env), env);
      else
	define(cadr(o), eval(caddr(o), env), env);
      return theok;
    }
    if (isset(car(o))) {
      Obj *pair = envlookup(cadr(o), env);
      setcdr(pair, eval(caddr(o), env));
      return theok;
    }
    if (isif(car(o))) {
      o = ISTRUTHY(eval(cadr(o), env)) ? caddr(o) : (isnull(cdddr(o)) ? thefalse : cadddr(o));
      goto tailcall;
    }
    if (islambda(car(o)))
      return makecompproc(cadr(o), cddr(o), env);

    Obj *proc = eval(car(o), env);
    switch (proc->type) {
    case PRIM_PROC:
      return (*(proc->data.primproc.proc))(evalall(cdr(o), env));
    case COMP_PROC:
      env = cons(makealist(proc->data.compproc.formals, evalall(cdr(o), env)), proc->data.compproc.env);
      for (o = proc->data.compproc.body; !isnull(cdr(o)); o = cdr(o))
	eval(car(o), env);
      o = car(o);
      goto tailcall;
    default:
      ERROR("not a procedure\n");
    }
  default:
    ERROR("cannot eval object\n");
  }
}

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
  case NUMBER:
    printf("%d", o->data.fixnum.val);
    break;
  case BOOLEAN:
    printf("#%c", istrue(o) ? 't' : 'f');
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
  case PRIM_PROC:
  case COMP_PROC:
    printf("#<procedure>");
    break;
  }
}

int main()
{
  init();
  Obj *env = cons(globalenv, thenull);
  while (1) {
    setjmp(errbuf); /* should this be inside the while (1)? */
    printf("> ");
    print(eval(read(), env));
    printf("\n");
  }
  return 0;
}
