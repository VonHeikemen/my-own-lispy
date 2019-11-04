/* compile: gcc -std=c99 -Wall lispy.c mpc.c -ledit -lm -o lispy */ 

#include "mpc.h"

#include <stdio.h>
#include <stdlib.h>

#include <editline/readline.h>
#include <histedit.h>

/* Forward declarations */
struct lisp_val;
struct lisp_env;
typedef struct lisp_val lisp_val;
typedef struct lisp_env lisp_env;

lisp_env* lisp_env_new(void);
void lisp_env_del(lisp_env* env);
lisp_env* lisp_env_copy(lisp_env* env);
lisp_val* lisp_read(mpc_ast_t* t);

mpc_parser_t* Number;
mpc_parser_t* Symbol;
mpc_parser_t* String;
mpc_parser_t* Comment;
mpc_parser_t* Sexpr;
mpc_parser_t* Qexpr;
mpc_parser_t* Expr;
mpc_parser_t* Lispy;

/* Possible lisp value types */
enum {
  LISP_VAL_ERR, 
  LISP_VAL_NUM,
  LISP_VAL_SYM,
  LISP_VAL_STR,
  LISP_VAL_FN,
  LISP_VAL_SEXPR,
  LISP_VAL_QEXPR
};

typedef lisp_val*(*lisp_builtin)(lisp_env*, lisp_val*);

struct lisp_val {
  int type;

  /* Basic */
  long num;
  char* err;
  char* sym;
  char* str;

  /* Function */
  lisp_builtin builtin;
  lisp_env* env;
  lisp_val* formals;
  lisp_val* body;

  /* Expression */
  int count;
  lisp_val** cell;
};

/* Possible error types */
enum { LISP_ERR_DIV_ZERO, LISP_ERR_BAD_OP, LISP_ERR_BAD_NUM };

/* Forward declarations */
lisp_val* lisp_eval(lisp_env* env, lisp_val* v);
void print_val(lisp_val* v);

/* 
 * lisp_val constructors 
 */
lisp_val* lisp_num(long x) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_NUM;
  v->num = x;

  return v;
}

lisp_val* lisp_err(char* fmt, ...) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_ERR;

  /* Create a va list */
  va_list va;
  va_start(va, fmt);

  /* Allocate 512 bytes for the message */
  v->err = malloc(512);

  /* print the error string with a max of 511 characters */
  vsnprintf(v->err, 511, fmt, va);

  /* Reallocate to number of bytes actually used */
  v->err = realloc(v->err, strlen(v->err) + 1);

  /* Cleanup va list */
  va_end(va);

  return v;
}

lisp_val* lisp_sym(char* str) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_SYM;
  v->sym = malloc(strlen(str) + 1);
  strcpy(v->sym, str);

  return v;
}

lisp_val* lisp_str(char* str) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_STR;
  v->str = malloc(strlen(str) + 1);
  strcpy(v->str, str);

  return v;
}

lisp_val* lisp_fn(lisp_builtin fn) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_FN;
  v->builtin = fn;

  return v;
}

lisp_val* lisp_sexpr(void) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_SEXPR;
  v->count = 0;
  v->cell = NULL;

  return v;
}

lisp_val* lisp_qexpr(void) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_QEXPR;
  v->count = 0;
  v->cell = NULL;

  return v;
}

lisp_val* lisp_lambda(lisp_val* formals, lisp_val* body) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_FN;

  v->builtin = NULL;
  v->env = lisp_env_new();
  v->formals = formals;
  v->body = body;

  return v;
}

/* lisp_val destructor */
void lisp_del(lisp_val* v) {
  switch(v->type) {
    /* Do nothing */
    case LISP_VAL_NUM: break;

    /* User defined functions */
    case LISP_VAL_FN:
      if(!v->builtin) {
        lisp_env_del(v->env);
        lisp_del(v->formals);
        lisp_del(v->body);
      }
    break;

    /* For error or symbol free the string data */
    case LISP_VAL_ERR: free(v->err); break;
    case LISP_VAL_SYM: free(v->sym); break;
    case LISP_VAL_STR: free(v->str); break;

     /* Delete all elements inside the cell */
    case LISP_VAL_QEXPR:
    case LISP_VAL_SEXPR:
      for(int i = 0; i < v->count; i++) {
        lisp_del(v->cell[i]);
      }

      /* Free the memory for the cell */
      free(v->cell);
    break;
  }
  
  /* Free the memory for the struct itself */
  free(v);
}

/* Copy values */
lisp_val* lisp_val_copy(lisp_val* v) {
  lisp_val* target = malloc(sizeof(lisp_val));
  target->type = v->type;

  switch(v->type) {
    /* Copy functions and numbers directly */
    case LISP_VAL_NUM: target->num = v->num; break;

    case LISP_VAL_FN:
      if(v->builtin) {
        target->builtin = v->builtin;
      } else {
        target->builtin = NULL;
        target->env = lisp_env_copy(v->env);
        target->formals = lisp_val_copy(v->formals);
        target->body = lisp_val_copy(v->body);
      }
    break;

    /* Copy strings */
    case LISP_VAL_ERR:
      target->err = malloc(strlen(v->err) + 1);
      strcpy(target->err, v->err);
    break;

    case LISP_VAL_SYM:
      target->sym = malloc(strlen(v->sym) + 1);
      strcpy(target->sym, v->sym);
    break;

    case LISP_VAL_STR:
      target->str = malloc(strlen(v->str) + 1);
      strcpy(target->str, v->str);
    break;

    /* Copy lists */
    case LISP_VAL_SEXPR:
    case LISP_VAL_QEXPR:
      target->count = v->count;
      target->cell = malloc(sizeof(lisp_val*) * target->count);
      for(int i = 0; i < target->count; i++) {
        target->cell[i] = lisp_val_copy(v->cell[i]);
      }
    break;
  }

  return target;
}

/* 
 * Utility functions 
 */
lisp_val* lisp_add(lisp_val* v, lisp_val* x) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lisp_val*) * v->count);
  v->cell[v->count - 1] = x;

  return v;
}

lisp_val* lisp_pop(lisp_val* v, int index) {
  /* Find the item a index */
  lisp_val* item = v->cell[index];

  /* Shift memory after the index over the top */
  memmove(
    &v->cell[index],
    &v->cell[index + 1],
    sizeof(lisp_val*) * (v->count - index - 1)
  );

  /* Decrease the count of children in the list */
  v->count--;

  /* Reallocate the memory used */
  v->cell = realloc(v->cell, sizeof(lisp_val*) * v->count);

  return item;
}

lisp_val* lisp_take(lisp_val* v, int index) {
  lisp_val* item = lisp_pop(v, index);
  lisp_del(v);

  return item;
}

lisp_val* lisp_join(lisp_val* x, lisp_val* y) {
  /* For each cell in 'y' add it to 'x' */
  for(int i = 0; i < y->count; i ++) {
    x = lisp_add(x, y->cell[i]);
  }

  /* Delete the empty 'y' and return 'x' */
  free(y->cell);
  free(y);

  return x;
}

int lisp_eq(lisp_val* x, lisp_val* y) {
  /* Different types are always unequal */
  if(x->type != y->type) { return 0; }

  /* Compare based upon type */
  switch(x->type) {
    /* Compare number values */
    case LISP_VAL_NUM: return (x->num == y->num);

    /* Compare string values */
    case LISP_VAL_ERR: return (strcmp(x->err, y->err) == 0);
    case LISP_VAL_SYM: return (strcmp(x->sym, y->sym) == 0);
    case LISP_VAL_STR: return (strcmp(x->str, y->str) == 0);

    /* Compare functions */
    case LISP_VAL_FN:
      if(x->builtin || y->builtin) {
        return x->builtin == y->builtin;
      } else {
        return lisp_eq(x->formals, y->formals) 
          && lisp_eq(x->body, y->body);
      }

    /* If expression, compare every element */
    case LISP_VAL_QEXPR:
    case LISP_VAL_SEXPR:
      if(x->count != y->count) { return 0; }
      for(int i = 0; i < x->count; i++) {
        /* If one element is different then is not equal*/
        if(!lisp_eq(x->cell[i], y->cell[i])) { return 0; }
      }
  
      /* Otherwise lists must be equal */
      return 1;
      break;
  }
  return 0;
}

/* 
 * Print functions 
 */
void print_expr(lisp_val* v, char open, char close) {
  putchar(open);

  for(int i = 0; i < v->count; i++) {
    print_val(v->cell[i]);

    /* Don't print trailing space if last element */
    if(i != (v->count - 1)) {
      putchar(' ');
    }
  }

  putchar(close);
}

void print_str(lisp_val* v) {
  /* Make a copy of the string */
  char* escaped = malloc(strlen(v->str) + 1);
  strcpy(escaped, v->str);

  /* Pass it through the escape function */
  escaped = mpcf_escape(escaped);

  /* Print it between " character */
  printf("\"%s\"", escaped);

  /* Free copied string */
  free(escaped);
}

void print_val(lisp_val* v) {
  switch(v->type) {
    case LISP_VAL_FN:
      if(v->builtin) {
        printf("<builtin>");
      } else {
        printf("(-> "); 
        print_val(v->formals);
        putchar(' ');
        print_val(v->body);
        putchar(')');
      }
    break;

    case LISP_VAL_NUM:
      printf("%li", v->num);
    break;

    case LISP_VAL_ERR:
      printf("Error: %s", v->err);
    break;

    case LISP_VAL_SYM:
      printf("%s", v->sym);
    break;

    case LISP_VAL_STR:
      print_str(v);
    break;

    case LISP_VAL_SEXPR:
      print_expr(v, '(', ')');
    break;

    case LISP_VAL_QEXPR:
      print_expr(v, '{', '}');
    break;
  }
}

void print_line(lisp_val* v) {
  print_val(v);
  putchar('\n');
}

char* lisp_type_name(int t) {
  switch(t) {
    case LISP_VAL_FN: return "Function";
    case LISP_VAL_NUM: return "Number";
    case LISP_VAL_ERR: return "Error";
    case LISP_VAL_SYM: return "Symbol";
    case LISP_VAL_STR: return "String";
    case LISP_VAL_SEXPR: return "S-Expression";
    case LISP_VAL_QEXPR: return "Q-Expression";
    default: return "Unknown";
  }
}

/* 
 * Lisp environment 
 */
struct lisp_env {
  lisp_env* parent;
  int count;
  char** syms;
  lisp_val** vals;
};

/* Constructor */
lisp_env* lisp_env_new(void) {
  lisp_env* env = malloc(sizeof(lisp_env));
  env->parent = NULL;
  env->count = 0;
  env->syms = NULL;
  env->vals = NULL;

  return env;
}

/* Destructor */
void lisp_env_del(lisp_env* env) {
  for(int i = 0; i < env->count; i++) {
    free(env->syms[i]);
    lisp_del(env->vals[i]);
  }
  free(env->syms);
  free(env->vals);
  free(env);
}

/* 
 * Env utilities 
 */
lisp_val* lisp_env_get(lisp_env* env, lisp_val* key) {
  /* Iterate over all items in the environment */
  for(int i = 0; i < env->count; i++) {
    /* Check it the stored string matches the symbol string */
    /* if it does, return a copy of the value */
    if(strcmp(env->syms[i], key->sym) == 0) {
      return lisp_val_copy(env->vals[i]);
    }
  }

  /* If no symbol is found check in parent */
  if(env->parent) {
    return lisp_env_get(env->parent, key);
  }

  return lisp_err("Unbound symbol '%s'", key->sym);
}

void lisp_env_put(lisp_env* env, lisp_val* key, lisp_val* val) {
  /* Check if variable already exists */
  for(int i = 0; i < env->count; i++) {
    /* If variable is found replace the item with user supplied input */
    if(strcmp(env->syms[i], key->sym) == 0) {
      lisp_del(env->vals[i]);
      env->vals[i] = lisp_val_copy(val);

      return;
    }
  }

  /* If variable does not exists allocate space for new entry */
  env->count++;
  env->vals = realloc(env->vals, sizeof(lisp_val*) * env->count);
  env->syms = realloc(env->syms, sizeof(char*) * env->count);

  /* Copy content of lisp_val and symbol into new location */
  env->vals[env->count - 1] = lisp_val_copy(val);
  env->syms[env->count - 1] = malloc(strlen(key->sym) + 1);
  strcpy(env->syms[env->count - 1], key->sym);
}

void lisp_env_def(lisp_env* env, lisp_val* key, lisp_val* val) {
  /* Iterate till env has no parent */
  while(env->parent) { env = env->parent; }

  /* Put value in 'env' */
  lisp_env_put(env, key, val);
}

lisp_env* lisp_env_copy(lisp_env* env) {
  lisp_env* target = malloc(sizeof(lisp_env));

  target->parent = env->parent;
  target->count = env->count;
  target->syms = malloc(sizeof(char*) * target->count);
  target->vals = malloc(sizeof(lisp_val*) * target->count);

  for(int i = 0; i < env->count; i++) {
    target->syms[i] = malloc(strlen(env->syms[i]) + 1);
    strcpy(target->syms[i], env->syms[i]);
    target->vals[i] = lisp_val_copy(env->vals[i]);
  }

  return target;
}

/* 
 * Builtin functions 
 */
#define LISP_ASSERT(args, condition, err_msg, ...) \
  if(!(condition)) { \
    lisp_val* err = lisp_err(err_msg, ##__VA_ARGS__); \
    lisp_del(args); \
    return err; \
  }

#define LISP_ASSERT_TYPE(fun, args, index, expect) \
  LISP_ASSERT( \
    args, \
    args->cell[index]->type == expect, \
    "Function '%s' passed incorrect type for argument %i. " \
    "Got %s, expected %s", \
    fun, \
    index, \
    lisp_type_name(args->cell[index]->type), \
    lisp_type_name(expect) \
  )

#define LISP_ASSERT_NUM(fun, args, num) \
  LISP_ASSERT( \
    args, \
    args->count == num, \
    "Function '%s' passed incorrect number if arguments. " \
    "Got %i, expected %i", \
    fun, \
    arg->count, \
    num \
  )

#define LISP_ASSERT_NOT_EMPTY(fun, args, index) \
  LISP_ASSERT( \
    args, \
    args->cell[index]->count != 0, \
    "Function '%s' passed {} for argument %i. " \
    "Got %i, expected %i", \
    fun, \
    index \
  )

lisp_val* builtin_list(lisp_env* env, lisp_val* arg) {
  arg->type = LISP_VAL_QEXPR;

  return arg;
}

lisp_val* builtin_head(lisp_env* env, lisp_val* arg) {
  LISP_ASSERT_NUM("head", arg, 1);
  LISP_ASSERT_TYPE("head", arg, 0, LISP_VAL_QEXPR);
  LISP_ASSERT_NOT_EMPTY("head", arg, 0);

  lisp_val* v = lisp_take(arg, 0);
  while(v->count > 1) {
    lisp_del(lisp_pop(v, 1));
  }

  return v;
}

lisp_val* builtin_tail(lisp_env* env, lisp_val* arg) {
  LISP_ASSERT_NUM("tail", arg, 1);
  LISP_ASSERT_TYPE("tail", arg, 0, LISP_VAL_QEXPR);
  LISP_ASSERT_NOT_EMPTY("tail", arg, 0);

  lisp_val* v = lisp_take(arg, 0);
  lisp_del(lisp_pop(v, 0));

  return v;
}


lisp_val* builtin_eval(lisp_env* env, lisp_val* arg) {
  LISP_ASSERT_NUM("eval", arg, 1);
  LISP_ASSERT_TYPE("eval", arg, 0, LISP_VAL_QEXPR);

  lisp_val* expr = lisp_take(arg, 0);
  expr->type = LISP_VAL_SEXPR;

  return lisp_eval(env, expr);
}

lisp_val* builtin_join(lisp_env* env, lisp_val* arg) {
  for(int i = 0; i < arg->count; i++) {
    LISP_ASSERT_TYPE("join", arg, i, LISP_VAL_QEXPR);
  }

  lisp_val* expr = lisp_pop(arg, 0);

  while(arg->count) {
    lisp_val* next = lisp_pop(arg, 0); 
    expr = lisp_join(expr, next);
  }

  lisp_del(arg);

  return expr;
}

lisp_val* builtin_op(lisp_env* env, lisp_val* input, char* op) {
  /* Ensure all arguments are numbers */
  for(int i = 0; i < input->count; i++) {
    if(input->cell[i]->type != LISP_VAL_NUM) {
      lisp_del(input);
      return lisp_err("Cannot operate on non-number");
    }
  }

  /* Get the first element */
  lisp_val* x = lisp_pop(input, 0); 

  /* If no arguments and sub then perform unary negation */
  if((strcmp(op, "-") == 0) && input->count == 0) {
    x->num = -x->num;
  }

  while(input->count > 0) {
    /* Pop the next element */
    lisp_val* y = lisp_pop(input, 0);

    /* Evaluate operator */
    if(strcmp(op, "+") == 0) { x->num += y->num; }
    if(strcmp(op, "-") == 0) { x->num -= y->num; }
    if(strcmp(op, "*") == 0) { x->num *= y->num; }
    if(strcmp(op, "/") == 0) {
      if(y->num == 0) {
        lisp_del(x);
        lisp_del(y);

        x = lisp_err("Division by zero");
        break;
      } 
      
      x->num /= y->num; 
    }

    lisp_del(y);
  }

  lisp_del(input);

  return x;
}

lisp_val* builtin_add(lisp_env* env, lisp_val* arg) {
  return builtin_op(env, arg, "+");
}

lisp_val* builtin_sub(lisp_env* env, lisp_val* arg) {
  return builtin_op(env, arg, "-");
}

lisp_val* builtin_mul(lisp_env* env, lisp_val* arg) {
  return builtin_op(env, arg, "*");
}

lisp_val* builtin_div(lisp_env* env, lisp_val* arg) {
  return builtin_op(env, arg, "/");
}

lisp_val* builtin_lambda(lisp_env* env, lisp_val* arg) {
  /* Check that the first two arguments are Q-Expressions */
  LISP_ASSERT_NUM("->", arg, 2);
  LISP_ASSERT_TYPE("->", arg, 0, LISP_VAL_QEXPR);
  LISP_ASSERT_TYPE("->", arg, 1, LISP_VAL_QEXPR);

  /* Check first Q-Expression contains only symbols */
  for(int i = 0; i < arg->cell[0]->count; i++) {
    LISP_ASSERT(
      arg,
      (arg->cell[0]->cell[i]->type == LISP_VAL_SYM),
      "Cannot define non-symbol. Got %s, expected %s",
      lisp_type_name(arg->cell[0]->cell[i]->type),
      lisp_type_name(LISP_VAL_SYM)
    );
  }

  /* Pop the first two arguments and pass them to lisp_lambda */
  lisp_val* formals = lisp_pop(arg, 0);
  lisp_val* body = lisp_pop(arg, 0);
  lisp_del(arg);

  return lisp_lambda(formals, body);
}

lisp_val* lisp_call(lisp_env* env, lisp_val* formals, lisp_val* args) {
  /* If is builtin then apply it */
  if(formals->builtin) { return formals->builtin(env, args); }

  /* Argument count */
  int given = args->count;
  int total = formals->formals->count;

  /* While arguments still remain to be processed */
  while(args->count) {
    /* If we've ran out of formal arguments to bind */
    if(formals->formals->count == 0) {
      lisp_del(args);
      return lisp_err(
        "Function passed too many arguments. "
        "Got %i, expected %i.",
        given,
        total
      );
    }

    /* Pop first symbol from formals */
    lisp_val* sym = lisp_pop(formals->formals, 0);

    /* Special case to deal with '&' */
    if(strcmp(sym->sym, "&") == 0) {
      /* Ensure '&' is followed by another symbol */
      if(formals->formals->count != 1) {
        lisp_del(args);
        return lisp_err(
          "Function format invalid. "
          "Symbol '&' not followed by single symbol."
        );
      }

      /* Next formal should be bound to remaining arguments */
      lisp_val* next_sym = lisp_pop(formals->formals, 0);
      lisp_env_put(formals->env, next_sym, builtin_list(env, args));
      lisp_del(sym);
      lisp_del(next_sym);
      break;
    }

    /* Pop next argument from the list */
    lisp_val* val = lisp_pop(args, 0);

    /* Bind a copy into the function's environment */
    lisp_env_put(formals->env, sym, val);

    /* Delete symbol and value */
    lisp_del(sym);
    lisp_del(val);
  }

  /* Arguments list can be cleaned up */
  lisp_del(args);

  /* If '&' remains in formal list bind to empty list */
  if(formals->formals->count > 0 &&
    strcmp(formals->formals->cell[0]->sym, "&") == 0
  ) {
    /* Check to ensure that & is not passed invalidly */
    if(formals->formals->count != 2) {
      return lisp_err(
        "Function format invalid. "
        "Symbol '&' not followed by single symbol."
      );
    }

    /* Pop and delete '&' symbol */
    lisp_del(lisp_pop(formals->formals, 0));

    /* Pop next symbol and create empty list */
    lisp_val* sym = lisp_pop(formals->formals, 0);
    lisp_val* val = lisp_qexpr();

    /* Bind to environment and delete */
    lisp_env_put(formals->env, sym, val);
    lisp_del(sym);
    lisp_del(val);
  }

  /* If all formals have been bound then evaluate */
  if(formals->formals->count == 0) {
    /* Set parent environment */
    formals->env->parent = env;

    /* Evaluate and return */
    return builtin_eval(
      formals->env,
      lisp_add(lisp_sexpr(), lisp_val_copy(formals->body))
    );
  }

  /* return partially evaluated function */
  return lisp_val_copy(formals);
}

lisp_val* builtin_var(lisp_env* env, lisp_val* arg, char* fn_name) {
  LISP_ASSERT_TYPE(fn_name, arg, 0, LISP_VAL_QEXPR);

  lisp_val* syms = arg->cell[0];
  for(int i = 0; i < syms->count; i++) {
    LISP_ASSERT(
      arg,
      (syms->cell[i]->type == LISP_VAL_SYM),
      "Function '%s', cannot define non-symbol. "
      "Got %s, expected %s",
      fn_name,
      lisp_type_name(syms->cell[i]->type),
      lisp_type_name(LISP_VAL_SYM)
    );
  }

  LISP_ASSERT(
    arg,
    (syms->count == arg->count - 1),
    "Function '%s' passed too many arguments for symbols. "
    "Got %i, expected %i.",
    fn_name,
    syms->count,
    arg->count - 1
  );

  for(int i = 0; i < syms->count; i++) {
    /* if 'def' define in global env */
    if(strcmp(fn_name, "def") == 0) {
      lisp_env_def(env, syms->cell[i], arg->cell[i + 1]);
    }

    /* if 'put' define in local env */
    else if(strcmp(fn_name, "=") == 0) {
      lisp_env_put(env, syms->cell[i], arg->cell[i + 1]);
    }
  }

  lisp_del(arg);

  return lisp_sexpr();
}

lisp_val* builtin_def(lisp_env* env, lisp_val* arg) {
  return builtin_var(env, arg, "def");
}

lisp_val* builtin_put(lisp_env* env, lisp_val* arg) {
  return builtin_var(env, arg, "=");
}

lisp_val* builtin_fun(lisp_env* env, lisp_val* arg) {
  /* test case fn {add x y} {+ x y} */
  /* Check that the first two arguments are Q-Expressions */
  LISP_ASSERT_NUM("fn", arg, 2);
  LISP_ASSERT_TYPE("fn", arg, 0, LISP_VAL_QEXPR);
  LISP_ASSERT_TYPE("fn", arg, 1, LISP_VAL_QEXPR);

  /* Check first Q-Expression contains only symbols */
  for(int i = 0; i < arg->cell[0]->count; i++) {
    LISP_ASSERT(
      arg,
      (arg->cell[0]->cell[i]->type == LISP_VAL_SYM),
      "Cannot define non-symbol. Got %s, expected %s",
      lisp_type_name(arg->cell[0]->cell[i]->type),
      lisp_type_name(LISP_VAL_SYM)
    );
  }

  lisp_val* fn_args = lisp_pop(arg, 0); 
  lisp_val* fn_name = lisp_pop(fn_args, 0);
  lisp_val* fn_body = lisp_pop(arg, 0);

  lisp_val* fn = lisp_lambda(fn_args, fn_body);
  lisp_env_def(env, fn_name, fn);

  lisp_del(fn_name);

  return fn;
}

lisp_val* builtin_ord(lisp_env* env, lisp_val* arg, char* op) {
  LISP_ASSERT_NUM(op, arg, 2);
  LISP_ASSERT_TYPE(op, arg, 0, LISP_VAL_NUM);
  LISP_ASSERT_TYPE(op, arg, 1, LISP_VAL_NUM);

  int res;

  if(strcmp(op, ">") == 0) {
    res = (arg->cell[0]->num > arg->cell[1]->num);
  }

  if(strcmp(op, "<") == 0) {
    res = (arg->cell[0]->num < arg->cell[1]->num);
  }
 
  if(strcmp(op, ">=") == 0) {
    res = (arg->cell[0]->num >= arg->cell[1]->num);
  }

  if(strcmp(op, "<=") == 0) {
    res = (arg->cell[0]->num <= arg->cell[1]->num);
  }

  lisp_del(arg);
  return lisp_num(res);
}

lisp_val* builtin_gt(lisp_env* env, lisp_val* arg) {
  return builtin_ord(env, arg, ">");
}

lisp_val* builtin_lt(lisp_env* env, lisp_val* arg) {
  return builtin_ord(env, arg, "<");
}

lisp_val* builtin_gte(lisp_env* env, lisp_val* arg) {
  return builtin_ord(env, arg, ">=");
}

lisp_val* builtin_lte(lisp_env* env, lisp_val* arg) {
  return builtin_ord(env, arg, "<=");
}

lisp_val* builtin_cmp(lisp_env* env, lisp_val* arg, char* op) {
  LISP_ASSERT_NUM(op, arg, 2);

  int res;

  if(strcmp(op, "==") == 0) {
    res = lisp_eq(arg->cell[0], arg->cell[1]);
  }

  if(strcmp(op, "!=") == 0) {
    res = !lisp_eq(arg->cell[0], arg->cell[1]);
  }

  lisp_del(arg);
  return lisp_num(res);
}

lisp_val* builtin_eq(lisp_env* env, lisp_val* arg) {
  return builtin_cmp(env, arg, "==");
}

lisp_val* builtin_ne(lisp_env* env, lisp_val* arg) {
  return builtin_cmp(env, arg, "!=");
}

lisp_val* builtin_if(lisp_env* env, lisp_val* arg) {
  LISP_ASSERT_NUM("if", arg, 3);
  LISP_ASSERT_TYPE("if", arg, 0, LISP_VAL_NUM);
  LISP_ASSERT_TYPE("if", arg, 1, LISP_VAL_QEXPR);
  LISP_ASSERT_TYPE("if", arg, 2, LISP_VAL_QEXPR);

  /* Mark both expressions as evaluable */
  lisp_val* x;
  arg->cell[1]->type = LISP_VAL_SEXPR;
  arg->cell[2]->type = LISP_VAL_SEXPR;

  if(arg->cell[0]->num) {
    /* If true evaluate first expression */
    x = lisp_eval(env, lisp_pop(arg, 1));
  } else {
    x = lisp_eval(env, lisp_pop(arg, 2));
  }

  lisp_del(arg);
  return x;
}

lisp_val* builtin_load(lisp_env* env, lisp_val* arg) {
  LISP_ASSERT_NUM("load", arg, 1);
  LISP_ASSERT_TYPE("load", arg, 0, LISP_VAL_STR);

  /* Parse file given by string name */
  mpc_result_t result;

  if(mpc_parse_contents(arg->cell[0]->str, Lispy, &result)) {
    /* Read contents */
    lisp_val* expr = lisp_read(result.output);
    mpc_ast_delete(result.output);

    /* Evaluate each expression */
    while(expr->count) {
      lisp_val* curr_expr = lisp_eval(env, lisp_pop(expr, 0));

      /* If evaluation leads to an error, print it */
      if(curr_expr->type == LISP_VAL_ERR) { print_line(curr_expr); }
      lisp_del(curr_expr);
    }

    /* Delete expressions and arguments */
    lisp_del(expr);
    lisp_del(arg);

    /* Return empty list */
    return lisp_sexpr();
  } else {
    /* Get parse error as string */
    char* err_msg = mpc_err_string(result.error);
    mpc_err_delete(result.error);

    /* Create new error message using it */
    lisp_val* err = lisp_err("Could not load library %s", err_msg);
    free(err_msg);
    lisp_del(arg);

    return err;
  }
}

lisp_val* builtin_print(lisp_env* env, lisp_val* arg) {
  /* Print each argument followed by a space */
  for(int i = 0; i < arg->count; i++) {
    print_val(arg->cell[i]);
    putchar(' ');
  }

  putchar('\n');
  lisp_del(arg);

  return lisp_sexpr();
}

lisp_val* builtin_error(lisp_env* env, lisp_val* arg) {
  LISP_ASSERT_NUM("error", arg, 1);
  LISP_ASSERT_TYPE("error", arg, 0, LISP_VAL_STR);

  /* Create error from first argument */
  lisp_val* err = lisp_err(arg->cell[0]->str);

  lisp_del(arg);

  return err;
}

void lisp_env_add_builtin(lisp_env* env, char* name, lisp_builtin fn) {
  lisp_val* key = lisp_sym(name);
  lisp_val* val = lisp_fn(fn);
  lisp_env_put(env, key, val);

  lisp_del(key);
  lisp_del(val);
}

void lisp_env_add_builtins(lisp_env* env) {
  /* Variable functions */
  lisp_env_add_builtin(env, "def", builtin_def);
  lisp_env_add_builtin(env, "=", builtin_put);
  lisp_env_add_builtin(env, "->", builtin_lambda);
  lisp_env_add_builtin(env, "fn", builtin_fun);

  /* List functions */
  lisp_env_add_builtin(env, "list", builtin_list);
  lisp_env_add_builtin(env, "head", builtin_head);
  lisp_env_add_builtin(env, "tail", builtin_tail);
  lisp_env_add_builtin(env, "eval", builtin_eval);
  lisp_env_add_builtin(env, "join", builtin_join);

  /* Mathematical functions */
  lisp_env_add_builtin(env, "+", builtin_add);
  lisp_env_add_builtin(env, "-", builtin_sub);
  lisp_env_add_builtin(env, "*", builtin_mul);
  lisp_env_add_builtin(env, "/", builtin_div);

  /* Comparison functions */
  lisp_env_add_builtin(env, "if", builtin_if);
  lisp_env_add_builtin(env, "==", builtin_eq);
  lisp_env_add_builtin(env, "!=", builtin_ne);
  lisp_env_add_builtin(env, ">", builtin_gt);
  lisp_env_add_builtin(env, "<", builtin_lt);
  lisp_env_add_builtin(env, ">=", builtin_gte);
  lisp_env_add_builtin(env, "<=", builtin_lte);

  /* String functions */
  lisp_env_add_builtin(env, "load", builtin_load);
  lisp_env_add_builtin(env, "error", builtin_error);
  lisp_env_add_builtin(env, "print", builtin_print);
}


/* 
 * Evaluation functions 
 */

lisp_val* lisp_eval_sexpr(lisp_env* env, lisp_val* v) {
  /* Evaluate children */
  for(int i = 0; i < v->count; i++) {
    v->cell[i] = lisp_eval(env, v->cell[i]);
  }

  /* Error check */
  for(int i = 0; i < v->count; i++) {
    if(v->cell[i]->type == LISP_VAL_ERR) { return lisp_take(v, i); }
  }

  /* Empty expression */
  if(v->count == 0) { return v; }

  /* Single expression */
  if(v->count == 1) { return lisp_take(v, 0); }

  /* Ensure first element is a function after evaluation */
  lisp_val* first = lisp_pop(v, 0);
  if(first->type != LISP_VAL_FN) {
    lisp_val* err = lisp_err(
      "S-Expression starts with incorrect type. "
      "Got %s, expected %s.",
      lisp_type_name(first->type),
      lisp_type_name(LISP_VAL_FN)
    );

    lisp_del(v);
    lisp_del(first);

    return err;
  }

  /* Call function to get result */
  lisp_val* result = lisp_call(env, first, v);
  lisp_del(first);

  return result;
}

lisp_val* lisp_eval(lisp_env* env, lisp_val* v) {
  if(v->type == LISP_VAL_SYM) {
    lisp_val* x = lisp_env_get(env, v);
    lisp_del(v);
    return x;
  }

  /* Evaluate S-expressions */
  if(v->type == LISP_VAL_SEXPR) { return lisp_eval_sexpr(env, v); }

  /* All other lisp_val remain the same */
  return v;
}

/* 
 * Reading functions 
 */
lisp_val* lisp_read_num(mpc_ast_t* t) {
  errno = 0;
  long x = strtol(t->contents, NULL, 10);

  return errno != ERANGE
    ? lisp_num(x)
    : lisp_err("Invalid number");
}

lisp_val* lisp_read_str(mpc_ast_t* t) {
  /* Cut off the final quote character */
  t->contents[strlen(t->contents) - 1] = '\0';

  /* Copy the string missing out the quote character */
  char* unescaped = malloc(strlen(t->contents + 1) + 1);
  strcpy(unescaped, t->contents + 1);

  /* Pass through the unescape function */
  unescaped = mpcf_unescape(unescaped);

  /* Create a new lisp_val using the string */
  lisp_val* str = lisp_str(unescaped);

  /* Free the string and return */
  free(unescaped);

  return str;
}

lisp_val* lisp_read(mpc_ast_t* t) {
  /* If symbol or number return the conversion */
  if(strstr(t->tag, "number")) { return lisp_read_num(t); }
  if(strstr(t->tag, "string")) { return lisp_read_str(t); }
  if(strstr(t->tag, "symbol")) { return lisp_sym(t->contents); }

  /* If root (>) or sexpr then create empty list */
  lisp_val* list = NULL;
  if(strcmp(t->tag, ">") == 0) { list = lisp_sexpr(); }
  if(strstr(t->tag, "sexpr")) { list = lisp_sexpr(); }
  if(strstr(t->tag, "qexpr")) { list = lisp_qexpr(); }

  /* Fill the list with any valid expression */
  for(int i = 0; i < t->children_num; i++) {
    if(strcmp(t->children[i]->contents, "(") == 0) { continue; }
    if(strcmp(t->children[i]->contents, ")") == 0) { continue; }
    if(strcmp(t->children[i]->contents, "{") == 0) { continue; }
    if(strcmp(t->children[i]->contents, "}") == 0) { continue; }
    if(strcmp(t->children[i]->tag, "regex") == 0) { continue; }
    if(strstr(t->children[i]->tag, "comment")) { continue; }

    list = lisp_add(list, lisp_read(t->children[i]));
  }

  return list;
}

int main(int argc, char** argv) {
  /* Create some parsers */
  Number   = mpc_new("number");
  Symbol   = mpc_new("symbol");
  String   = mpc_new("string");
  Comment  = mpc_new("comment");
  Sexpr    = mpc_new("sexpr");
  Qexpr    = mpc_new("qexpr");
  Expr     = mpc_new("expr");
  Lispy    = mpc_new("lispy");

  /* Define language 'grammar' */
  mpca_lang(
    MPCA_LANG_DEFAULT,
    "                                                     \
      number  : /-?[0-9]+/ ;                              \
      symbol  : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;        \
      string  : /\"(\\\\.|[^\"])*\"/ ;                    \
      comment : /;[^\\r\\n]*/ ;                           \
      sexpr   : '(' <expr>* ')' ;                         \
      qexpr   : '{' <expr>* '}' ;                         \
      expr    : <number>  | <symbol> | <string>           \
              | <comment> | <sexpr>  | <qexpr> ;          \
      lispy   : /^/ <expr>* /$/ ;                         \
    ",
    Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lispy
  );

  lisp_env* env = lisp_env_new();
  lisp_env_add_builtins(env);

  /* Interactive prompt */
  if(argc == 1) {
    /* Print information */
    puts("Lispy Version 0.7.0");
    puts("Press Ctrl+c to exit \n");

    while(1) {
      /* Show prompt */
      char* input = readline("lispy> ");

      /* Add to command history */
      add_history(input);

      /* Parse input */
      mpc_result_t parser_res;
      if(mpc_parse("<stdin>", input, Lispy, &parser_res)) {
        lisp_val* result = lisp_eval(env, lisp_read(parser_res.output));
        print_line(result);
        lisp_del(result);
        mpc_ast_delete(parser_res.output);
      } else {
        /* Print error */
        mpc_err_print(parser_res.error);
        mpc_err_delete(parser_res.error);
      }

      free(input);
    }
  }

  /* Supplied with list of files */
  if(argc >= 2) {
    /* Loop over each filename */
    for(int i = 1; i < argc; i++) {
      /* Argument list with a single argument, the filename */
      lisp_val* args = lisp_add(lisp_sexpr(), lisp_str(argv[i]));

      /* Pass to builtin load */
      lisp_val* res = builtin_load(env, args);

      /* If result is an error, print it */
      if(res->type == LISP_VAL_ERR) { print_line(res); }

      lisp_del(res);
    }
  }

  /* Cleanup environment */
  lisp_env_del(env);

  /* Cleanup parsers */
  mpc_cleanup(8,
    Number, Symbol, String, Comment,
    Sexpr, Qexpr, Expr, Lispy
  );

  return 0;
}

