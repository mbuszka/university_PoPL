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
  union {
    int e_const;

    char* e_var;

    struct {
      struct expression* lhs;
      struct expression* rhs;
    } e_diff;

    struct expression* e_zero;

    struct {
      struct expression* cond;
      struct expression* then_e;
      struct expression* else_e;
    } e_if;

    struct {
      char *var;
      struct expression* exp;
      struct expression* body;
    } e_let;

    struct {
      char* var;
      struct expression* body;
      struct expression* exp;
    } e_proc;

    struct {
      struct expression* lhs;
      struct expression* rhs;
    } e_call;

    struct {
      char* p_name;
      char* var;
      struct expression* body;
      struct expression* exp;
    } e_letrec;
  } data;
} expr_t;

#endif
