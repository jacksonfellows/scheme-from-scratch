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

typedef enum { NUMBER, BOOLEAN, CHAR, STRING, SYMBOL, PAIR, _NULL, PRIM_PROC, COMP_PROC, _EOF } Type;

typedef struct sObj {
  Type type;
  union {
    struct {
      long val;
    } fixnum;
    struct {
      int val;
    } boolean;
    struct {
      char val;
    } _char;
    struct {
      char *val;
    } string;
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

DECLARE_CONSTANT(eof);

DECLARE_CONSTANT(quote);
DECLARE_CONSTANT(define);
DECLARE_CONSTANT(ok);
DECLARE_CONSTANT(set);
DECLARE_CONSTANT(if);
DECLARE_CONSTANT(lambda);
DECLARE_CONSTANT(begin);
DECLARE_CONSTANT(cond);
DECLARE_CONSTANT(else);
DECLARE_CONSTANT(let);
DECLARE_CONSTANT(and);
DECLARE_CONSTANT(or);

DECLARE_CONSTANT(apply);
DECLARE_CONSTANT(eval);

Obj *allocobj()
{
  Obj *o = malloc(sizeof(Obj));
  return o;
}

Obj *makefixnum(long l)
{
  Obj *fixnum = allocobj();
  fixnum->type = NUMBER;
  fixnum->data.fixnum.val = l;
  return fixnum;
}

Obj *makeboolean(int x)
{
  Obj *boolean = allocobj();
  boolean->type = BOOLEAN;
  boolean->data.boolean.val = x;
  return boolean;
}

Obj *makechar(char c)
{
  Obj *_char = allocobj();
  _char->type = CHAR;
  _char->data._char.val = c;
  return _char;
}


Obj *makestring(char *buffer, int len)
{
  Obj *string = allocobj();
  string->type = STRING;
  string->data.string.val = malloc(len);
  strncpy(string->data.string.val, buffer, len);
  return string;
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

Obj *makesymbol(char *buffer, int len)
{
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

Obj *makeeof()
{
  Obj *eof = allocobj();
  eof->type = _EOF;
  return eof;
}

int length(Obj *pair)
{
  int len = 0;
  for (Obj *o = pair; !isnull(o); o = cdr(o), ++len);
  return len;
}

Obj *add(Obj *args)
{
  long sum = 0;
  for (Obj *o = args; !isnull(o); o = cdr(o))
    sum += car(o)->data.fixnum.val;
  return makefixnum(sum);
}

Obj *sub(Obj *args)
{
  long sum;
  if (length(args) == 1)
    return makefixnum(-car(args)->data.fixnum.val);

  sum = car(args)->data.fixnum.val;
  for (Obj *o = cdr(args); !isnull(o); o = cdr(o))
    sum -= car(o)->data.fixnum.val;
  return makefixnum(sum);
}

Obj *mul(Obj *args)
{
  long product = 1;
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

TYPE_PREDICATE(number, NUMBER);
TYPE_PREDICATE(boolean, BOOLEAN);
TYPE_PREDICATE(char, CHAR);
TYPE_PREDICATE(string, STRING);
TYPE_PREDICATE(symbol, SYMBOL);
TYPE_PREDICATE(pair, PAIR);
TYPE_PREDICATE(null, _NULL);
TYPE_PREDICATE(eof, _EOF);

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
#define MAKE_PRIM_PROC(env, name, proc) PUSH(cons(MAKE_CONSTANT_SYMBOL(#name), makeprimproc(proc)), env)

Obj *interactionenv;
Obj *predefinedenv;

Obj *interactionenvproc(Obj *args)
{
  return interactionenv;
}

Obj *nullenv(Obj *args)
{
  return cons(thenull, thenull);
}

Obj *initenv();

Obj *makeenv(Obj *args)
{
  Obj *env = nullenv(thenull);
  setcar(env, initenv());
  return env;
}

Obj *read();

Obj *readproc(Obj *args)
{
  Obj *o = read();
  return o == NULL ? theeof : o;
}

Obj *readcharproc(Obj *args)
{
  int c = getchar();
  return c == EOF ? theeof : makechar(c);
}

int peek();

Obj *peekcharproc(Obj *args)
{
  int c = peek();
  return c == EOF ? theeof : makechar(c);
}

void write(Obj *o);

Obj *writeproc(Obj *args)
{
  write(car(args));
  return theok;
}

Obj *eofobject(Obj *args)
{
  return theeof;
}

Obj *initenv()
{
  Obj *env = thenull;

  MAKE_PRIM_PROC(env, number?, numberp);
  MAKE_PRIM_PROC(env, boolean?, booleanp);
  MAKE_PRIM_PROC(env, char?, charp);
  MAKE_PRIM_PROC(env, string?, stringp);
  MAKE_PRIM_PROC(env, symbol?, symbolp);
  MAKE_PRIM_PROC(env, pair?, pairp);
  MAKE_PRIM_PROC(env, null?, nullp);
  MAKE_PRIM_PROC(env, procedure?, procedurep);
  MAKE_PRIM_PROC(env, eof-object?, eofp);

  MAKE_PRIM_PROC(env, +, add);
  MAKE_PRIM_PROC(env, -, sub);
  MAKE_PRIM_PROC(env, *, mul);

  MAKE_PRIM_PROC(env, =, fixnumeq);
  MAKE_PRIM_PROC(env, <, fixnumlt);
  MAKE_PRIM_PROC(env, <=, fixnumle);
  MAKE_PRIM_PROC(env, >, fixnumgt);
  MAKE_PRIM_PROC(env, >=, fixnumge);

  MAKE_PRIM_PROC(env, car, carproc);
  MAKE_PRIM_PROC(env, cdr, cdrproc);
  MAKE_PRIM_PROC(env, set-car!, setcarproc);
  MAKE_PRIM_PROC(env, set-cdr!, setcdrproc);
  MAKE_PRIM_PROC(env, cons, consproc);
  MAKE_PRIM_PROC(env, list, list);

  MAKE_PRIM_PROC(env, length, lengthproc);

  MAKE_PRIM_PROC(env, eq?, eq);

  MAKE_PRIM_PROC(env, apply, NULL);
  MAKE_PRIM_PROC(env, eval, NULL);

  MAKE_PRIM_PROC(env, interaction-environment, interactionenvproc);
  MAKE_PRIM_PROC(env, nullenv, nullenv);
  MAKE_PRIM_PROC(env, environment, makeenv);

  MAKE_PRIM_PROC(env, read, readproc);
  MAKE_PRIM_PROC(env, read-char, readcharproc);
  MAKE_PRIM_PROC(env, peek-char, peekcharproc);

  MAKE_PRIM_PROC(env, write, writeproc);

  MAKE_PRIM_PROC(env, eof-object, eofobject);

  return env;
}

void init()
{
  thenull = allocobj();
  thenull->type = _NULL;

  thetrue = makeboolean(1);
  thefalse = makeboolean(0);

  theeof = makeeof();

  interned = thenull;
  predefinedenv = thenull;

  INIT_CONSTANT_SYMBOL(quote);
  INIT_CONSTANT_SYMBOL(define);
  INIT_CONSTANT_SYMBOL(ok);
  theset = MAKE_CONSTANT_SYMBOL("set!");
  INIT_CONSTANT_SYMBOL(if);
  INIT_CONSTANT_SYMBOL(lambda);
  INIT_CONSTANT_SYMBOL(begin);
  INIT_CONSTANT_SYMBOL(cond);
  INIT_CONSTANT_SYMBOL(else);
  INIT_CONSTANT_SYMBOL(let);
  INIT_CONSTANT_SYMBOL(and);
  INIT_CONSTANT_SYMBOL(or);

  INIT_CONSTANT_SYMBOL(apply);
  INIT_CONSTANT_SYMBOL(eval);

  predefinedenv = initenv();
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

void eat(char *str)
{
  for (int i = 0; str[i] != '\0'; ++i)
    if (getchar() != str[i])
      ERROR("unexpected character\n");
}

void assertnextdelim()
{
  if (!isdelimiter(peek()))
    ERROR("expected delimiter\n");
}

Obj *read()
{
#define BUFFER_LEN 1024
  char readbuffer[BUFFER_LEN];
  int i = 0;
  long l;

  skipwhitespace();

  int c = getchar();
  switch (c) {
  case EOF:
    ungetc(' ', stdin); /* hack to remove EOF */
    return NULL;
  case '-': case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
    ungetc(c, stdin);
    scanf("%ld", &l);
    return makefixnum(l);
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
    case '\\':
      c = getchar();
      switch(c) {
      case 's':
	if (peek() == 'p') {
	  eat("pace");
	  assertnextdelim();
	  return makechar(' ');
	}
      case 'n':
	if (peek() == 'e') {
	  eat("ewline");
	  assertnextdelim();
	  return makechar('\n');
	}
      default:
	return makechar(c);
      }
    default:
      ERROR("# not followed by t, f, or \\\n");
    }
  case '"':
    while ((c = getchar()) != '"') {
      if (c == '\\')
	switch (getchar()) {
	case 'n':
	  readbuffer[i++] = '\n';
	  break;
	case '"':
	  readbuffer[i++] = '"';
	  break;
	case '\\':
	  readbuffer[i++] = '\\';
	  break;
	default:
	  ERROR("unrecognized escape sequence\n");
	}
      else
	readbuffer[i++] = c;
    }
    readbuffer[i++] = '\0';
    return makestring(readbuffer, i);
  default:
    ungetc(c, stdin);
    for (; !isdelimiter(c = getchar()); readbuffer[i++] = c);
    ungetc(c, stdin);
    readbuffer[i++] = '\0';
    return makesymbol(readbuffer, i);
  }

  ERROR("invalid input\n");
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
  if (isnull(car(env)))
    setcar(env, cons(cons(sym, val), thenull));
  else {
    Obj *o;
    for (o = car(env); !isnull(cdr(o)); o = cdr(o));
    setcdr(o, cons(cons(sym, val), thenull));
  }
}

Obj *eval(Obj *o, Obj *env);

Obj *evalall(Obj *list, Obj *env)
{
  if (isnull(list))
    return thenull;
  return cons(eval(car(list), env), evalall(cdr(list), env));
}

Obj *condtoif(Obj *condforms)
{
  if (isnull(condforms))
    return thefalse;
  if (iselse(caar(condforms)))
    return cons(thebegin, cdar(condforms));
  return cons(theif,
	      cons(caar(condforms),
		   cons(cons(thebegin, cdar(condforms)),
			cons(condtoif(cdr(condforms)), thenull))));
}

Obj *lettolambda(Obj *letforms)
{
  Obj *formals = thenull;
  Obj *values = thenull;
  for (Obj *o = car(letforms); !isnull(o); o = cdr(o)) {
    PUSH(caar(o), formals);
    PUSH(cadar(o), values);
  }
  return cons(cons(thelambda, cons(formals, cdr(letforms))), values);
}

#define ISTRUTHY !isfalse

Obj *eval(Obj *o, Obj *env)
{
 tailcall:
  switch (o->type) {
  case NUMBER:
  case BOOLEAN:
  case CHAR:
  case STRING:
  case _EOF:
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
    if (isbegin(car(o))) {
      for (o = cdr(o); !isnull(cdr(o)); o = cdr(o))
	eval(car(o), env);
      o = car(o);
      goto tailcall;
    }
    if (iscond(car(o))) {
      o = condtoif(cdr(o));
      goto tailcall;
    }
    if (islet(car(o))) {
      o = lettolambda(cdr(o));
      goto tailcall;
    }
    if (isand(car(o))) {
      if (isnull(cdr(o)))
	return thetrue;
      for (o = cdr(o); !isnull(cdr(o)); o = cdr(o))
	if (isfalse(eval(car(o), env)))
	  return thefalse;
      o = car(o);
      goto tailcall;
    }
    if (isor(car(o))) {
      if (isnull(cdr(o)))
	return thefalse;
      for (o = cdr(o); !isnull(cdr(o)); o = cdr(o)) {
	Obj *r = eval(car(o), env);
	if (ISTRUTHY(r))
	  return r;
      }
      o = car(o);
      goto tailcall;
    }
    if (isapply(car(o))) {
      o = cons(cadr(o), eval(caddr(o), env));
      goto tailcall;
    }
    if (iseval(car(o))) {
      Obj *newo = eval(cadr(o), env);
      env = eval(caddr(o), env);
      o = newo;
      goto tailcall;
    }

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

void writepair(Obj *o)
{
  write(car(o));

  if (isnull(cdr(o)));
  else if (cdr(o)->type == PAIR) {
    printf(" ");
    writepair(cdr(o));
  } else {
    printf(" . ");
    write(cdr(o));
  }
}

void write(Obj *o)
{
  switch (o->type) {
  case NUMBER:
    printf("%ld", o->data.fixnum.val);
    break;
  case BOOLEAN:
    printf("#%c", istrue(o) ? 't' : 'f');
    break;
  case CHAR:
    switch (o->data._char.val) {
    case '\n':
      printf("#\\newline");
      break;
    case ' ':
      printf("#\\space");
      break;
    default:
      printf("#\\%c", o->data._char.val);
    }
    break;
  case STRING:
    printf("\"");
    for (char *str = o->data.string.val; *str != '\0'; str++)
      switch(*str) {
      case '"':
	printf("\\\"");
	break;
      case '\n':
	printf("\\n");
	break;
      case '\\':
	printf("\\\\");
	break;
      default:
	printf("%c", *str);
      }
    printf("\"");
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
      write(cadr(o));
    } else {
      printf("(");
      writepair(o);
      printf(")");
    }
    break;
  case PRIM_PROC:
  case COMP_PROC:
    printf("#<procedure>");
    break;
  case _EOF:
    printf("#<eof>");
    break;
  }
}

int main()
{
  init();
  interactionenv = cons(predefinedenv, thenull);
  Obj *o;
  setjmp(errbuf); /* should this be inside the while (1)? */
  while (1) {
    printf("> ");
    o = read();
    if (o == NULL)
      break;
    write(eval(o, interactionenv));
    printf("\n");
  }
  return 0;
}
