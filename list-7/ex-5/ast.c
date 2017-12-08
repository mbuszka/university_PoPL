
#include <stdlib.h>

#include "ast.h"

expr_t* make_const(int n) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->e_type = const_exp;
  ptr->data.e_const = n;
  return ptr;
}

expr_t* make_var(char *n) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->e_type = var_exp;
  ptr->data.e_var = n;
  return ptr;
}

expr_t* make_diff(expr_t* l, expr_t* r) {
  expr_t* ptr = malloc(sizeof(expr_t));
  ptr->e_type = var_exp;
  ptr->data.e_diff.lhs = l;
  ptr->data.e_diff.rhs = r;
  return ptr;
}

int main() {
  expr_t* expr = malloc(sizeof(expr_t));
  expr->e_type = const_exp;
  expr->data.e_const = 15;
  return 0;
}
