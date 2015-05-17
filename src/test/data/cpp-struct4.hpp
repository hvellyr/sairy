/*! @doc Foo's doc */
struct Foo {
  /*! @doc Inner U */
  union U {
    int i;
  };

  enum {
    k_a,
    k_b,
  };

  U u;
};
