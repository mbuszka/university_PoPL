#include "util.h"
#include "data-structures.h"
#include "interp.h"

bounce_t value_of(expr_t* exp, env_t* env, cont_t* cont) {
  switch (exp->tag) {
  case const_exp:
    return apply_cont(cont, inj_num(exp->num));

  case var_exp:
    return apply_cont(cont, apply_env(env, exp->var));

  case diff_exp: {
    cont_t* new_cont = make_cont(diff_1_cont);
    new_cont->exp = exp->rhs;
    new_cont->env = env;
    new_cont->cont = cont;
    return value_of(exp->lhs, env, new_cont);
  }

  case zero_exp: {
    cont_t* new_cont = make_cont(zero_cont);
    new_cont->cont = cont;
    return value_of(exp->exp, env, new_cont);
  }

  case if_exp: {
    cont_t* new_cont = make_cont(if_cont);
    new_cont->then_e = exp->then_e;
    new_cont->else_e = exp->else_e;
    new_cont->env = env;
    new_cont->cont = cont;
    return value_of(exp->exp, env, new_cont);
  }

  case let_exp: {
    cont_t* new_cont = make_cont(let_cont);
    new_cont->var = exp->var;
    new_cont->body = exp->body;
    new_cont->env = env;
    new_cont->cont = cont;
    return value_of(exp->exp, env, new_cont);
  }

  case proc_exp: {
    proc_t proc = { exp->var, exp->body, env};
    return apply_cont(cont, inj_proc(proc));
  }

  case call_exp: {
    cont_t* new_cont = make_cont(rator_cont);
    new_cont->exp = exp->rhs;
    new_cont->env = env;
    new_cont->cont = cont;
    return value_of(exp->lhs, env, new_cont);
  }

  case letrec_exp: {
    env_t* new_env = extend_env(env, exp->p_name, inj_num(42));
    proc_t proc = { exp->var, exp->body, new_env };
    new_env->val = inj_proc(proc);
    return value_of(exp->exp, new_env, cont);
  }
  }
}

bounce_t apply_cont(cont_t* cont, expval_t val) {
  switch (cont->tag) {
  case end_cont: {
    printf("Finished computation.\n");
    bounce_t ret;
    ret.tag = val_bnc;
    ret.val = val;
    return ret;
  }

  case zero_cont: {
    if (val.tag != num_val) abort("Expected numerical value in zero?");
    return apply_cont(cont->cont, inj_bool(val.v_num == 0 ? 1 : 0));
  }

  case let_cont: {
    env_t* new_env = extend_env(cont->env, cont->var, val);
    return value_of(cont->body, new_env, cont->cont);
  }

  case if_cont: {
    if (val.tag != bool_val) abort("Expected boolean value in if condition");
    if (val.v_bool) {
      return value_of(cont->then_e, cont->env, cont->cont);
    } else {
      return value_of(cont->else_e, cont->env, cont->cont);
    }
  }

  case diff_1_cont: {
    cont_t* new_cont = make_cont(diff_2_cont);
    new_cont->cont = cont->cont;
    new_cont->val = val;
    return value_of(cont->exp, cont->env, new_cont);
  }

  case diff_2_cont: {
    if (cont->val.tag != num_val || val.tag != num_val)
      abort("Expected both operands of - to be num");

    return apply_cont(cont->cont, inj_num(cont->val.v_num - val.v_num));
  }

  case rator_cont: {
    cont_t* new_cont = make_cont(rand_cont);
    new_cont->cont = cont->cont;
    new_cont->val = val;
    return value_of(cont->exp, cont->env, new_cont);
  }

  case rand_cont: {
    if (cont->val.tag != proc_val) {
      printf("Operator is %d\n", cont->val.tag);
      abort("Expected operator to be a procedure");
    }
    proc_t proc = cont->val.v_proc;
    env_t* new_env = extend_env(proc.env, proc.var, val);
    bounce_t thunk;
    thunk.tag = thunk_bnc;
    thunk.exp = proc.body;
    thunk.env = new_env;
    thunk.cont = cont->cont;
    return thunk;
  }
  }
}

expval_t trampoline(bounce_t bounce) {
  while (bounce.tag != val_bnc) {
    printf("Trampolining!\n");
    bounce = value_of(bounce.exp, bounce.env, bounce.cont);
  }

  return bounce.val;
}

expval_t eval(expr_t* exp) {
  return trampoline(value_of(exp, empty_env(), make_cont(end_cont)));
}

