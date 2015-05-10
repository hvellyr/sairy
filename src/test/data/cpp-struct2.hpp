/*! @doc Access visibility in structs */
struct Foo {
  int a;
  static int a2;
protected:
  int b;
  static int b2;
private:
  int c;
  static int c2;
};
