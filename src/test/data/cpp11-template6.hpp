#include <memory>

namespace abc {

/*! @doc Foo is a wrapper for a T */
template <typename T, typename _ = void>
class Foo
{
public:
  /*! @doc The wrapped type */
  using Type = T;
};


/*! @doc Foo is a specialized version for Foo */
template <typename T>
class Foo<std::shared_ptr<T>>
{
public:
  /*! @doc The wrapped type 2 */
  using Type = std::shared_ptr<T>;
};

}
