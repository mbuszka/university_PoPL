#include "util.h"
#include "data-structures.h"
#include "interp.h"

expr_t* expr;
env_t* env;
cont_t* cont;
expval_t val;

void value_of() {
  switch (expr->tag) {
  case const_exp:
    val = inj_num(expr->num);
    apply_cont();
    break;

  case var_exp:
    val = apply_env(env, expr->var);
    apply_cont();
    break;

  case diff_exp: {
    cont_t* new_cont = make_cont(diff_1_cont);
    new_cont->exp = expr->rhs;
    new_cont->env = env;
    new_cont->cont = cont;
    cont = new_cont;
    expr = expr->lhs;
    value_of();
    break;
  }

  case zero_exp: {
    cont_t* new_cont = make_cont(zero_cont);
    new_cont->cont = cont;
    cont = new_cont;
    expr = expr->exp;
    value_of();
    break;
  }

  case if_exp: {
    cont_t* new_cont = make_cont(if_cont);
    new_cont->then_e = expr->then_e;
    new_cont->else_e = expr->else_e;
    new_cont->env = env;
    new_cont->cont = cont;
    cont = new_cont;
    expr = expr->exp;
    value_of();
    break;
  }

  case let_exp: {
    cont_t* new_cont = make_cont(let_cont);
    new_cont->var = expr->var;
    new_cont->body = expr->body;
    new_cont->env = env;
    new_cont->cont = cont;
    cont = new_cont;
    expr = expr->exp;
    value_of();
    break;
  }

  case proc_exp: {
    proc_t proc = { expr->var, expr->body, env };
    val = inj_proc(proc);
    apply_cont();
    break;
  }

  case call_exp: {
    cont_t* new_cont = make_cont(rator_cont);
    new_cont->exp = expr->rhs;
    new_cont->env = env;
    new_cont->cont = cont;
    cont = new_cont;
    expr = expr->lhs;
    value_of();
    break;
  }

  case letrec_exp: {
    env_t* new_env = extend_env(env, expr->p_name, inj_num(42));
    proc_t proc = { expr->var, expr->body, new_env };
    new_env->val = inj_proc(proc);
    env = new_env;
    expr = expr->exp;
    value_of();
    break;
  }
  }
}

void apply_cont() {
  switch (cont->tag) {
  case end_cont: {
    printf("Finished computation.\n");
    break;
  }

  case zero_cont: {
    if (val.tag != num_val) abort("Expected numerical value in zero?");
    cont = cont->cont;
    val = inj_bool(val.v_num == 0 ? 1 : 0);
    apply_cont();
    break;
  }

  case let_cont: {
    env_t* new_env = extend_env(cont->env, cont->var, val);
    env = new_env;
    expr = cont->body;
    cont = cont->cont;
    value_of();
    break;
  }

  case if_cont: {
    if (val.tag != bool_val) abort("Expected boolean value in if condition");
    env = cont->env;
    if (val.v_bool) {
      expr = cont->then_e;
    } else {
      expr = cont->else_e;
    }
    cont = cont->cont;
    value_of();
    break;
  }

  case diff_1_cont: {
    cont_t* new_cont = make_cont(diff_2_cont);
    new_cont->cont = cont->cont;
    new_cont->val = val;
    expr = cont->exp;
    env = cont->env;
    cont = new_cont;
    value_of();
    break;
  }

  case diff_2_cont: {
    if (cont->val.tag != num_val || val.tag != num_val)
      abort("Expected both operands of - to be num");
    val = inj_num(cont->val.v_num - val.v_num);
    cont = cont->cont;
    apply_cont();
    break;
  }

  case rator_cont: {
    cont_t* new_cont = make_cont(rand_cont);
    new_cont->cont = cont->cont;
    new_cont->val = val;
    expr = cont->exp;
    env = cont->env;
    cont = new_cont;
    value_of();
    break;
  }

  case rand_cont: {
    if (cont->val.tag != proc_val) {
      printf("Operator is %d\n", cont->val.tag);
      abort("Expected operator to be a procedure");
    }
    proc_t proc = cont->val.v_proc;
    env_t* new_env = extend_env(proc.env, proc.var, val);
    env = new_env;
    cont = cont->cont;
    expr = proc.body;
    value_of();
    break;
  }
  }
}

expval_t eval(expr_t* exp) {
  expr = exp;
  cont = make_cont(end_cont);
  env = empty_env();
  value_of();
  return val;
}

