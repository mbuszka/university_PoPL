#include <stdlib.h>
#include <string.h>

#include "data-structures.h"
#include "util.h"

expval_t inj_num(int n) {
  expval_t v;
  v.tag = num_val;
  v.v_num = n;
  return v;
}

expval_t inj_bool(int n) {
  expval_t v;
  v.tag = bool_val;
  v.v_bool = n;
  return v;
}

expval_t inj_proc(proc_t proc) {
  expval_t v;
  v.tag = proc_val;
  v.v_proc = proc;
  return v;
}

env_t* empty_env() {
  return NULL;
}

expval_t apply_env(env_t* env, char* var) {
  if (env == NULL) {
    abort("No value in environment");
  }

  if (strcmp(var, env->var) == 0) {
    return env->val;
  } else {
    return apply_env(env->next, var);
  }
}

env_t* extend_env(env_t* env, char* var, expval_t val) {
  env_t* new_env = malloc(sizeof(env_t));
  new_env->next = env;
  new_env->var = var;
  new_env->val = val;
  return new_env;
}

cont_t* make_cont(cont_tag tag) {
  cont_t* cont = malloc(sizeof(cont_t));
  cont->tag = tag;
  return cont;
}
