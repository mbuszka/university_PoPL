#ifndef INTERP_H
#define INTERP_H

#include "util.h"
#include "data-structures.h"

/* Bounce type */

typedef enum {
  val_bnc,
  thunk_bnc
} bounce_tag;

typedef struct {
  bounce_tag tag;
  expval_t val;
  cont_t* cont;
  env_t* env;
  expr_t* exp;
} bounce_t;

/* ********************** */

bounce_t value_of(expr_t* exp, env_t* env, cont_t* cont);
bounce_t apply_cont(cont_t* cont, expval_t val);
expval_t trampoline(bounce_t bounce);
expval_t eval(expr_t* exp);

#endif
