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
  union {
    int v_num;
    int v_bool;
    proc_t v_proc;
  } data;
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
  union {
    struct {
      struct continuation* cont;
    } c_zero;

    struct {
      char* var;
      expr_t* body;
      env_t* env;
      struct continuation* cont;
    } c_let;

    struct {
      expr_t* then_e;
      expr_t* else_e;
      env_t* env;
      struct continuation* cont;
    } c_if;

    struct bin_op_1 {
      expr_t* exp;
      env_t* env;
      struct continuation* cont;
    } c_diff_1;

    struct bin_op_2 {
      expval_t val;
      struct continuation* cont;
    } c_diff_2;

    struct bin_op_1 c_rator;

    struct bin_op_2 c_rand;

  } data;
} cont_t;

cont_t* make_cont(cont_tag tag);

/* ************************ */

#endif
