#include "interp.h"
#include "ast.h"

#define test(_msg, _res, _expected) if (_res != _expected) { printf("Test failed: %s", _msg); }

void test_1() {
  expr_t* exp = make_const(42);
  expval_t res = eval(exp);
  test("Should be number", res.tag, num_val);
  test("Should have value 42", res.v_num, 42);
}

void test_2() {
  expr_t* d1 = make_diff(make_const(4), make_const(7));
  expr_t* let1 = make_let("x", d1, make_diff(make_var("x"), make_const(7)));
  expval_t res = eval(let1);
  test("Should be number", res.tag, num_val);
  test("Should have value -10", res.v_num, -10)
}

void test_3() {
  expr_t* top =
    make_let("mul",
             make_proc("x",
                       make_letrec("inner",
                                   "y",
                                   make_if(make_zero(make_var("y")),
                                           make_const(0),
                                           make_diff(make_call(make_var("inner"),
                                                               make_diff(make_var("y"),
                                                                         make_const(1))),
                                                     make_var("x"))),
                                   make_var("inner"))),
             make_call(make_call(make_var("mul"),
                                 make_const(4)),
                       make_const(500)));
  expval_t res = eval(top);
  test("Should be number", res.tag, num_val);
  test("Should have value -20", res.v_num, -2000);
}

int main() {
  test_1();
  test_2();
  test_3();
  return 0;
}
