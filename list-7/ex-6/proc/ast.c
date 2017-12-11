
#include <stdlib.h>

#include "ast.h"

expr_t* make_const(int n) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = const_exp;
  ptr->num = n;
  return ptr;
}

expr_t* make_var(char *n) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = var_exp;
  ptr->var = n;
  return ptr;
}

expr_t* make_diff(expr_t* l, expr_t* r) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = diff_exp;
  ptr->lhs = l;
  ptr->rhs = r;
  return ptr;
}

expr_t* make_let(char* var, expr_t* exp, expr_t* body) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = let_exp;
  ptr->var = var;
  ptr->exp = exp;
  ptr->body = body;
  return ptr;
}

expr_t* make_call(expr_t* rator, expr_t* rand) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = call_exp;
  ptr->lhs = rator;
  ptr->rhs = rand;
  return ptr;
}

expr_t* make_letrec(char* p_name, char* var, expr_t* body, expr_t* exp) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = letrec_exp;
  ptr->p_name = p_name;
  ptr->var = var;
  ptr->body = body;
  ptr->exp = exp;
  return ptr;
}

expr_t* make_proc(char* var, expr_t* body) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = proc_exp;
  ptr->var = var;
  ptr->body = body;
  return ptr;
}

expr_t* make_if(expr_t* cond, expr_t* then_e, expr_t* else_e) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = if_exp;
  ptr->exp = cond;
  ptr->then_e = then_e;
  ptr->else_e = else_e;
  return ptr;
}

expr_t* make_zero(expr_t* exp) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->tag = zero_exp;
  ptr->exp = exp;
  return ptr;
}

