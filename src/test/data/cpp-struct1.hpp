/*! @doc A struct */
struct Foo {
  int bar;
  Foo();

  Foo(int b);

  ~Foo();

  int get_bar() const;

private:
  char b;
};
