#include <memory>

/*! @doc Foo is a wrapper for a T */
template <typename T, typename A = void, typename B = void>
class Foo
{
public:
  /*! @doc The wrapped type */
  using Type = T;
};


/*! @doc Foo is a specialized version for Foo */
template <typename T>
class Foo<std::shared_ptr<T>, float>
{
public:
  /*! @doc The wrapped type 2 */
  using Type = std::shared_ptr<T>;
};
