/*! @doc This is Foo's docu */
class Foo {
public:
  /*! @doc bar() inline */
  int bar();

protected:
  int moo();

private:
  int gaz();
};

/*! @doc bar() out-of-line */
int Foo::bar()
{
}

/*! @doc Moo out-of-line */
int Foo::moo()
{
}

/*! @doc gaz() should not show up */
int Foo::gaz() { }
