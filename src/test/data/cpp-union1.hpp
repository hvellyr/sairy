namespace foo {

  struct Xyz { };

  /*! @doc A union */
  union Abc {
    int bar;    //!< @doc the bar handle
    double gaz;
    Xyz xyz;    //!< @doc unknown
  };
}
