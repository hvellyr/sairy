namespace maa {
/*! @doc A base struct */
struct Foo {
  virtual void bar();
};
}

using namespace maa;

/*! @doc A struct */
struct Foo2 : public Foo {
  void bar() override;
};
