/*! @doc Foo's doc */
class Foo {
public:
  /*! @doc public inner */
  struct Xyz {
    /*! @doc The value */
    using value_type = int;
  };

  /*! @doc Value type */
  using value_type = Xyz::value_type;
};
