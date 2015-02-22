#include <stdio.h>

#include "foo.h"
#include "bar.h"

#define HELLO_WORLD  42

int main(int argc, char** argv)
{
  Foo f = Bar();

  Foo2().bar();

  double a = Foo2().abc;
  printf("%g\n", a);

  printf("%g\n", Foo2().abc);

  return 0;
}
