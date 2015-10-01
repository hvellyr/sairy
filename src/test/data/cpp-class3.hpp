/*! @doc This is Foo's docu */
class Foo {
public:
  /*! @doc ctor() inline */
  Foo();

  Foo(int);

  ~Foo();
};

/*! @doc ctor() out-of-line */
Foo::Foo() { }

/*! @doc ctor(int) out-of-line */
Foo::Foo(int) { }

/*! @doc dtor() out-of-line */
Foo::~Foo() { }
