
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

