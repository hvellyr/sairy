/*! @doc Foo's doc */
class Foo {
public:
  class Xyz { };

  /*! @doc Dummy type */
  class Dummy { };

  /*! @doc Value type */
  typedef Xyz value_type;
  /*! @doc Value2 type */
  typedef Dummy value2_type;
};
