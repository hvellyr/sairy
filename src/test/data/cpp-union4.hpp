namespace foo {

  /*! @doc A union */
  union Abc {
    enum Boo {
      k_a,
      k_b,
    };
    Boo boo;

    union U {
      int i;
      //! @doc the bar handle
      struct Bar {
        float f;
      };
      Bar f;
    };
    U u;
  };
}
