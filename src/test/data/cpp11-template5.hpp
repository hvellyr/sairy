/*! @doc Foo is a wrapper for a T */
template <typename T, template <typename> class Cont>
class Foo
{
public:
  /*! @doc Constructor */
  Foo(T t);

  /*! @doc The wrapped value */
  Cont<T> _value;
};
