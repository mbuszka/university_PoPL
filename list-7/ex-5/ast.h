#ifndef AST_H
#define AST_H

typedef enum {
  const_exp,
  var_exp,
  diff_exp,
  zero_exp,
  if_exp,
  let_exp,
  proc_exp,
  call_exp,
  letrec_exp
} expr_tag;

typedef struct expression {
  expr_tag tag;
  int num;
  char* var;

  struct expression* lhs;
  struct expression* rhs;

  struct expression* then_e;
  struct expression* else_e;

  struct expression* exp;
  struct expression* body;

  char* p_name;
} expr_t;

expr_t* make_const(int n);
expr_t* make_var(char *n);
expr_t* make_diff(expr_t* l, expr_t* r);
expr_t* make_let(char* var, expr_t* exp, expr_t* body);
expr_t* make_call(expr_t* rator, expr_t* rand);
expr_t* make_letrec(char* p_name, char* var, expr_t* body, expr_t* exp);
expr_t* make_proc(char* var, expr_t* body);
expr_t* make_if(expr_t* cond, expr_t* then_e, expr_t* else_e);
expr_t* make_zero(expr_t* exp);

#endif
