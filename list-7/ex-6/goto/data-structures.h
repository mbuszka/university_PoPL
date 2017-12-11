#ifndef DATA_STRUCTURES_H
#define DATA_STRUCTURES_H

#include "ast.h"

/* Procedure type */

struct environment;

typedef struct procedure {
  char* var;
  expr_t* body;
  struct environment* env;
} proc_t;

/* ******************* */

/* Expressed values */

typedef enum {
  num_val,
  bool_val,
  proc_val
} expval_tag;

typedef struct {
  expval_tag tag;
  int v_num;
  int v_bool;
  proc_t v_proc;
} expval_t;

expval_t inj_num(int n);
expval_t inj_bool(int b);
expval_t inj_proc(proc_t proc);

/* ******************** */

/* Environment */

typedef struct environment {
  char* var;
  expval_t val;
  struct environment *next;
} env_t;

expval_t apply_env(env_t* env, char* var);
env_t* extend_env(env_t* env, char*var, expval_t val);
env_t* empty_env();

/* ******************** */

/* Continuations */

typedef enum {
  end_cont,
  zero_cont,
  let_cont,
  if_cont,
  diff_1_cont,
  diff_2_cont,
  rator_cont,
  rand_cont
} cont_tag;

typedef struct continuation {
  cont_tag tag;

  struct continuation* cont;
  env_t*   env;
  char*    var;
  expval_t val;
  expr_t*  body;
  expr_t*  then_e;
  expr_t*  else_e;
  expr_t*  exp;
} cont_t;

cont_t* make_cont(cont_tag tag);

/* ************************ */

#endif
