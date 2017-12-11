#ifndef INTERP_H
#define INTERP_H

#include "util.h"
#include "data-structures.h"

void value_of();
void apply_cont();
expval_t eval(expr_t* exp);

#endif
