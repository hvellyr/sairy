/*! @doc Foo is a wrapper for a T */
template <typename T, typename U>
class Foo
{
public:
  /*! @doc Constructor */
  Foo(T t);

  /*! @doc Constructor2 */
  Foo(U&& u);

  /*! @doc The wrapped value */
  T _value;
};
