#include "interp.h"
#include "ast.h"

#define test(_msg, _res, _expected) if (_res != _expected) { printf("Test failed: %s", _msg); }

void test_1 () {
  expr_t* exp = make_const(42);
  expval_t res = eval(exp);
  test("Should be number", res.tag, num_val);
  test("Should have value 42", res.v_num, 42);
}

int main() {
  test_1();
  return 0;
}
