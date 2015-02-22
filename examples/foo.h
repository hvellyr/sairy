#include "bar.h"

class Foo
{
public:
  Foo();
  Foo(Bar b);

  //--------

  Bar mB;
};


class Foo2
{
public:
  Foo2();

  virtual Bar bar() const;

  double abc;
};
