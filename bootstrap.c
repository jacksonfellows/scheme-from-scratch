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

typedef enum { NUMBER, BOOLEAN, CHAR, STRING, SYMBOL, PAIR, _NULL, PRIM_PROC, COMP_PROC, _EOF, INPUT_PORT, OUTPUT_PORT } Type;

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
    struct {
      FILE *in;
    } inputport;
    struct {
      FILE *out;
    } outputport;
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

Obj *makeinputport(FILE* in)
{
  Obj *inputport = allocobj();
  inputport->type = INPUT_PORT;
  inputport->data.inputport.in = in;
  return inputport;
}

Obj *makeoutputport(FILE* out)
{
  Obj *outputport = allocobj();
  outputport->type = OUTPUT_PORT;
  outputport->data.outputport.out = out;
  return outputport;
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

Obj *lsh(Obj *args)
{
  return makefixnum(car(args)->data.fixnum.val << cadr(args)->data.fixnum.val);
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
TYPE_PREDICATE(inputport, INPUT_PORT);
TYPE_PREDICATE(outputport, OUTPUT_PORT);

Obj *procedurep(Obj *args)
{
  return TOBOOLEAN(car(args)->type == PRIM_PROC || car(args)->type == COMP_PROC);
}

Obj *numbertostring(Obj *args)
{
  long val = car(args)->data.fixnum.val;
  int len = snprintf(NULL, 0, "%ld", val) + 1;
  char *buffer = malloc(len);
  snprintf(buffer, len, "%ld", val);
  return makestring(buffer, len);
}

Obj *stringtosymbol(Obj *args)
{
  char *str = car(args)->data.string.val;
  return makesymbol(str, strlen(str) + 1);
}

Obj *stringlength(Obj *args)
{
  return makefixnum(strlen(car(args)->data.string.val));
}

Obj *stringappend(Obj *args)
{
  size_t len = 0;
  for (Obj *o = args; !isnull(o); o = cdr(o))
    len += strlen(car(o)->data.string.val);
  ++len;

  char *buffer = malloc(len);
  char *b = buffer;
  for (Obj *o = args; !isnull(o); o = cdr(o))
    b = stpcpy(b, car(o)->data.string.val);

  return makestring(buffer, len);
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

Obj *read(FILE *in);

#define GET_IN_PORT(args) isnull(args) ? stdin : car(args)->data.inputport.in

Obj *readproc(Obj *args)
{
  Obj *o = read(GET_IN_PORT(args));
  return o == NULL ? theeof : o;
}

Obj *readcharproc(Obj *args)
{
  int c = getc(GET_IN_PORT(args));
  return c == EOF ? theeof : makechar(c);
}

int peek(FILE *in);

Obj *peekcharproc(Obj *args)
{
  int c = peek(GET_IN_PORT(args));
  return c == EOF ? theeof : makechar(c);
}

Obj *openinputfile(Obj *args)
{
  return makeinputport(fopen(car(args)->data.string.val, "r"));
}

void write(FILE *out, Obj *o);

#define GET_OUT_PORT(args) isnull(args) ? stdout : car(args)->data.outputport.out

Obj *writeproc(Obj *args)
{
  write(GET_OUT_PORT(cdr(args)), car(args));
  return theok;
}

Obj *writecharproc(Obj *args)
{
  putc(car(args)->data._char.val, GET_OUT_PORT(cdr(args)));
  return theok;
}

Obj *openoutputfile(Obj *args)
{
  return makeoutputport(fopen(car(args)->data.string.val, "w"));
}

Obj *closeport(Obj *args)
{
  fclose(car(args)->data.inputport.in); /* union hacking */
  return theok;
}

void display(FILE* out, Obj *args);

/* TODO: make writepair accept a function pointer */
void displaypair(FILE *out, Obj *o)
{
  display(out, car(o));

  if (isnull(cdr(o)));
  else if (cdr(o)->type == PAIR) {
    fprintf(out, " ");
    displaypair(out, cdr(o));
  } else {
    fprintf(out, " . ");
    display(out, cdr(o));
  }
}

void display(FILE *out, Obj *o)
{
  switch (o->type) {
  case STRING:
    fprintf(out, "%s", o->data.string.val);
    break;
  case PAIR:
    fprintf(out, "(");
    displaypair(out, o);
    fprintf(out, ")");
    break;
  default:
    write(out, o);
  }
}

Obj *displayproc(Obj *args)
{
  display(GET_OUT_PORT(cdr(args)), car(args));
  return theok;
}

Obj *error(Obj *args)
{
  for (Obj *o = args; !isnull(o); o = cdr(o)) {
    switch (car(o)->type) {
    case STRING:
      fprintf(stderr, "%s", car(o)->data.string.val);
      break;
    default:
      write(stderr, car(o));
    }
    putc(' ', stderr);
  }
  ungetc(' ', stderr);
  ERROR("\n");
}


Obj *eval(Obj *o, Obj *env);

Obj *load(Obj *args)
{
  FILE *in = fopen(car(args)->data.string.val, "r");
  Obj *o;
  Obj *res;
  while ((o = read(in)) != NULL)
    res = eval(o, interactionenv);
  fclose(in);
  return res;
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
  MAKE_PRIM_PROC(env, input-port?, inputportp);
  MAKE_PRIM_PROC(env, output-port?, outputportp);

  MAKE_PRIM_PROC(env, number->string, numbertostring);
  MAKE_PRIM_PROC(env, string->symbol, stringtosymbol);

  MAKE_PRIM_PROC(env, string-length, stringlength);
  MAKE_PRIM_PROC(env, string-append, stringappend);

  MAKE_PRIM_PROC(env, +, add);
  MAKE_PRIM_PROC(env, -, sub);
  MAKE_PRIM_PROC(env, *, mul);

  MAKE_PRIM_PROC(env, lsh, lsh);

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

  MAKE_PRIM_PROC(env, load, load);

  MAKE_PRIM_PROC(env, read, readproc);
  MAKE_PRIM_PROC(env, read-char, readcharproc);
  MAKE_PRIM_PROC(env, peek-char, peekcharproc);
  MAKE_PRIM_PROC(env, open-input-file, openinputfile);

  MAKE_PRIM_PROC(env, write, writeproc);
  MAKE_PRIM_PROC(env, write-char, writecharproc);
  MAKE_PRIM_PROC(env, open-output-file, openoutputfile);

  MAKE_PRIM_PROC(env, close-port, closeport);

  MAKE_PRIM_PROC(env, display, displayproc);
  MAKE_PRIM_PROC(env, error, error);

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

int peek(FILE *in)
{
  int c = getc(in);
  ungetc(c, in);
  return c;
}

int isdelimiter(int c)
{
  return isspace(c) || c == '(' || c == ')';
}

void skipwhitespace(FILE *in)
{
  int c;
  while ((c = getc(in)) != EOF) {
    if (isspace(c))
      continue;
    if (c == ';') {
      while ((c = getc(in)) != '\n');
      continue;
    }
    ungetc(c, in);
    break;
  }
}

Obj *readpair(FILE *in)
{
  skipwhitespace(in);
  if (peek(in) == ')') {
    getc(in);
    return thenull;
  }

  Obj *pair = allocobj();
  pair->type = PAIR;
  setcar(pair, read(in));

  skipwhitespace(in);
  if (peek(in) == '.') {
    getc(in);
    setcdr(pair, read(in));
    skipwhitespace(in);
    if (getc(in) != ')')
      ERROR("invalid use of .\n");
  } else
    setcdr(pair, readpair(in));
  return pair;
}

void eat(FILE *in, char *str)
{
  for (int i = 0; str[i] != '\0'; ++i)
    if (getc(in) != str[i])
      ERROR("unexpected character\n");
}

void assertnextdelim(FILE *in)
{
  if (!isdelimiter(peek(in)))
    ERROR("expected delimiter\n");
}

Obj *read(FILE *in)
{
#define BUFFER_LEN 1024
  char readbuffer[BUFFER_LEN];
  int i = 0;
  long l;

  skipwhitespace(in);

  int c = getc(in);

  if (c == EOF) {
    ungetc(' ', in); /* hack to remove EOF */
    return NULL;
  } else if (isdigit(c) || (c == '-' && isdigit(peek(in)))) {
    ungetc(c, in);
    fscanf(in, "%ld", &l);
    return makefixnum(l);
  } else if (c == '(')
    return readpair(in);
  else if (c == ')') {
    ERROR("unbalanced parenthesis\n");
  } else if (c == '\'') {
    return cons(thequote, cons(read(in), thenull));
  } else if (c == '#') {
    switch (getc(in)) {
    case 't':
      return thetrue;
    case 'f':
      return thefalse;
    case '\\':
      c = getc(in);
      switch(c) {
      case 's':
	if (peek(in) == 'p') {
	  eat(in, "pace");
	  assertnextdelim(in);
	  return makechar(' ');
	}
      case 'n':
	if (peek(in) == 'e') {
	  eat(in, "ewline");
	  assertnextdelim(in);
	  return makechar('\n');
	}
      default:
	return makechar(c);
      }
    default:
      ERROR("# not followed by t, f, or \\\n");
    }
  } else if (c == '"') {
    while ((c = getc(in)) != '"') {
      if (c == '\\')
	switch (getc(in)) {
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
  }

  ungetc(c, in);
  for (; !isdelimiter(c = getc(in)); readbuffer[i++] = c);
  ungetc(c, in);
  readbuffer[i++] = '\0';
  return makesymbol(readbuffer, i);
}

Obj *makealist(Obj *a, Obj *b)
{
  if (isnull(a))
    return thenull;
  return cons(cons(car(a), car(b)), makealist(cdr(a), cdr(b)));
}

Obj *bindformals(Obj *formals, Obj *args)
{
  if (formals->type == PAIR)
    return makealist(formals, args);
  return cons(cons(formals, args), thenull);
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
  fprintf(stderr, "unbound variable: ");
  write(stderr, sym);
  ERROR("\n");
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
  Obj *args;
 tailcall:
  switch (o->type) {
  case NUMBER:
  case BOOLEAN:
  case CHAR:
  case STRING:
  case _EOF:
  case INPUT_PORT:
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
      Obj *evaled = evalall(cddr(o), env);

      if (isnull(evaled));
      else if (isnull(cdr(evaled)))
	evaled = car(evaled);
      else {
	Obj *e;
	for (e = evaled; !isnull(cddr(e)); e = cdr(e));
	setcdr(e, cadr(e));
      }

      o = cons(cadr(o), thenull);
      args = evaled;
      goto apply;
    }
    if (iseval(car(o))) {
      Obj *newo = eval(cadr(o), env);
      env = eval(caddr(o), env);
      o = newo;
      goto tailcall;
    }

    args = evalall(cdr(o), env);
  apply:
    do {
      Obj *proc = eval(car(o), env);
      switch (proc->type) {
      case PRIM_PROC:
	return (*(proc->data.primproc.proc))(args);
      case COMP_PROC:
	env = cons(bindformals(proc->data.compproc.formals, args), proc->data.compproc.env);
	for (o = proc->data.compproc.body; !isnull(cdr(o)); o = cdr(o))
	  eval(car(o), env);
	o = car(o);
	goto tailcall;
      default:
	fprintf(stderr, "not a procedure: ");
	write(stderr, proc);
	ERROR("\n");
      }
    } while (0);
  default:
    fprintf(stderr, "cannot eval object: ");
    write(stderr, o);
    ERROR("\n");
  }
}

void writepair(FILE *out, Obj *o)
{
  write(out, car(o));

  if (isnull(cdr(o)));
  else if (cdr(o)->type == PAIR) {
    fprintf(out, " ");
    writepair(out, cdr(o));
  } else {
    fprintf(out, " . ");
    write(out, cdr(o));
  }
}

void write(FILE *out, Obj *o)
{
  switch (o->type) {
  case NUMBER:
    fprintf(out, "%ld", o->data.fixnum.val);
    break;
  case BOOLEAN:
    fprintf(out, "#%c", istrue(o) ? 't' : 'f');
    break;
  case CHAR:
    switch (o->data._char.val) {
    case '\n':
      fprintf(out, "#\\newline");
      break;
    case ' ':
      fprintf(out, "#\\space");
      break;
    default:
      fprintf(out, "#\\%c", o->data._char.val);
    }
    break;
  case STRING:
    fprintf(out, "\"");
    for (char *str = o->data.string.val; *str != '\0'; str++)
      switch(*str) {
      case '"':
	fprintf(out, "\\\"");
	break;
      case '\n':
	fprintf(out, "\\n");
	break;
      case '\\':
	fprintf(out, "\\\\");
	break;
      default:
	fprintf(out, "%c", *str);
      }
    fprintf(out, "\"");
    break;
  case SYMBOL:
    fprintf(out, "%s", o->data.symbol.name);
    break;
  case _NULL:
    fprintf(out, "()");
    break;
  case PAIR:
    if (isquote(car(o))) {
      fprintf(out, "'");
      write(out, cadr(o));
    } else {
      fprintf(out, "(");
      writepair(out, o);
      fprintf(out, ")");
    }
    break;
  case PRIM_PROC:
  case COMP_PROC:
    fprintf(out, "#<procedure>");
    break;
  case _EOF:
    fprintf(out, "#<eof>");
    break;
  case INPUT_PORT:
    fprintf(out, "#<input-port>");
    break;
  case OUTPUT_PORT:
    fprintf(out, "#<output-port>");
    break;
  }
}

Obj *makeargslist(int argc, char *argv[], int i)
{
  if (i < argc)
    return cons(makestring(argv[i], strlen(argv[i])+1), makeargslist(argc, argv, i+1));
  return thenull;
}

int main(int argc, char *argv[])
{
  init();
  interactionenv = cons(predefinedenv, thenull);

  if (argc == 1) {
    Obj *o;
    setjmp(errbuf);
    while (1) {
      printf("> ");
      o = read(stdin);
      if (o == NULL)
	break;
      write(stdout, eval(o, interactionenv));
      printf("\n");
    }
  } else {
    if (setjmp(errbuf))
      return 1;
    load(cons(makestring(argv[1], strlen(argv[1])+1), thenull));
    Obj *argslist = makeargslist(argc, argv, 2);
    Obj *cmd = cons(MAKE_CONSTANT_SYMBOL("main"), cons(cons(thequote, cons(argslist, thenull)), thenull));
    eval(cmd, interactionenv);
  }
  return 0;
}
