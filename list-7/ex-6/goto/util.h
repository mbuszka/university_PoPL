#include <stdlib.h>
#include <stdio.h>

#define abort(_msg) do { printf("%s\n", _msg); exit(1); } while(0)
