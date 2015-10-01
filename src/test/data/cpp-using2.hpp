/*! @doc Foo's doc */
class Foo {
public:
  class Xyz { };

  /*! @doc Dummy type */
  class Dummy { };

  /*! @doc Value type */
  using value_type = Xyz;
  /*! @doc Value2 type */
  using value2_type = Dummy;
};
