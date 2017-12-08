#include "util.h"
#include "data-structures.h"

/* Bounce type */

typedef enum {
  val_bnc,
  thunk_bnc
} bounce_tag;

typedef struct {
  bounce_tag tag;
  union {
    expval_t val;

    struct {
      cont_t* cont;
      env_t* env;
    } thunk;
  } data;
} bounce_t;

/* ********************** */



bounce_t apply_cont(cont_t* cont, expval_t val);

bounce_t value_of(expr_t* exp, env_t* env, cont_t* cont) {
  switch (exp->tag) {
  case const_exp:
    return apply_cont(cont, inj_num(exp->data.e_const));

  case var_exp:
    return apply_cont(cont, apply_env(env, exp->data.e_var));

  case diff_exp: {
    cont_t* new_cont = make_cont(diff_1_cont);
    new_cont->data.c_diff_1.exp = exp->data.e_diff.rhs;
    new_cont->data.c_diff_1.env = env;
    new_cont->data.c_diff_1.cont = cont;
    return value_of(exp->data.e_diff.lhs, env, new_cont);
  }

  case zero_exp: {
    cont_t* new_cont = make_cont(zero_cont);
    new_cont->data.c_zero.cont = cont;
    return value_of(exp->data.e_zero, env, cont);
  }

  case if_exp: {
    cont_t* new_cont = make_cont(if_cont);
    new_cont->data.c_if.then_e = exp->data.e_if.then_e;
    new_cont->data.c_if.else_e = exp->data.e_if.else_e;
    new_cont->data.c_if.env = env;
    new_cont->data.c_if.cont = cont;
    return value_of(exp->data.e_if.cond, env, new_cont);
  }

  case let_exp: {
    cont_t* new_cont = make_cont(let_cont);
    new_cont->data.c_let.var = exp->data.e_let.var;
    new_cont->data.c_let.body = exp->data.e_let.body;
    new_cont->data.c_let.env = env;
    new_cont->data.c_let.cont = cont;
    return value_of(exp->data.e_let.exp, env, new_cont);
  }

  case proc_exp: {
    proc_t proc = { exp->data.e_proc.var, exp->data.e_proc.body, env};
    return apply_cont(cont, inj_proc(proc));
  }

  case call_exp: {
    cont_t* new_cont = make_cont(rator_cont);
    new_cont->data.c_rator.exp = exp->data.e_diff.rhs;
    new_cont->data.c_rator.env = env;
    new_cont->data.c_rator.cont = cont;
    return value_of(exp->data.e_call.lhs, env, new_cont);
  }

  case letrec_exp: {
    env_t* new_env = extend_env(env, exp->data.e_letrec.p_name, inj_num(42));
    proc_t proc = { exp->data.e_letrec.var, exp->data.e_letrec.body, new_env };
    new_env->val = inj_proc(proc);
    return value_of(exp->data.e_letrec.exp, new_env, cont);
  }
  }
}

bounce_t apply_cont(cont_t* cont, expval_t val) {
  switch (cont->tag) {
  case end_cont: {
    printf("Finished computation.\n");
    bounce_t ret;
    ret.tag = val_bnc;
    ret.data.val = val;
    return ret;
  }

  case zero_cont: {
    if (val.tag != num_val) abort("Expected numerical value in zero?");
    return apply_cont(cont->data.c_zero.cont, inj_bool(val.data.v_num == 0 ? 1 : 0));
  }

  case let_cont: {
    env_t* new_env = extend_env(cont->data.c_let.env, cont->data.c_let.var, val);
    return value_of(cont->data.c_let.body, new_env, cont->data.c_let.cont);
  }

  case if_cont: {
    if (val.tag != bool_val) abort("Expected boolean value in if condition");
    if (val.data.v_bool) {
      return value_of(cont->data.c_if.then_e, cont->data.c_if.env, cont->data.c_if.cont);
    } else {
      return value_of(cont->data.c_if.else_e, cont->data.c_if.env, cont->data.c_if.cont);
    }
  }

  case diff_1_cont: {
    cont_t* new_cont = make_cont(diff_2_cont);
    new_cont->data.c_diff_2.cont = cont->data.c_diff_1.cont;
    new_cont->data.c_diff_2.val = val;
    return value_of(cont->data.c_diff_1.exp, cont->data.c_diff_1.env, new_cont);
  }

  case diff_2_cont: {

  }

  case rator_cont: {

  }

  case rand_cont: {

  }
  }
}

