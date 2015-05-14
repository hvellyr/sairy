/*! @doc Foo's doc */
class Foo {
public:
  /*! @doc public inner */
  class Inner {
  public:
    static int bar;
  };

private:
  /*! @doc private inner */
  class Other {
  };
};

/*! @doc The magic number */
int Foo::Inner::bar = 42;
