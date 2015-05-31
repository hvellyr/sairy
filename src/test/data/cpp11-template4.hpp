/*! @doc Foo is a wrapper for a T */
template <typename T, int U = 42>
class Foo
{
public:
  /*! @doc Constructor */
  Foo(T t);

  /*! @doc The wrapped value */
  T _value;
};
