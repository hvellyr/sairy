namespace foo {

  /*! @doc A union */
  union Abc {
    //! @doc the bar handle
    struct Bar {
      float f;
      const char* name;
    };
    Bar bar;

    class Mno {
    public:
      int ixwick;  //!< @doc unknown
      double raa;
    } mno;
  };
}
