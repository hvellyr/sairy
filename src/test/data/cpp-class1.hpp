/*! @doc A struct */
class Foo {
  int bar;
  Foo();

  Foo(int b);

  ~Foo();

  virtual char* get_foo() const;
  virtual char* get_foo2() = 0;

protected:
  int get_bar();
  int get_bar() const;
  static char* set_moo(char* moo);

private:
  bool get_icon();

private:
  char b;
};
