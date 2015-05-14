#include <vector>

class Xyz { };

/*! @doc Dummy type */
class Dummy { };

namespace moo {

/*! @doc A number */
using Foo = int;

/*! @doc A container */
using Bar = std::vector<int>;

/*! @doc My type */
using MyXyz = Xyz;

/*! @doc Another type */
using MyDummy = Dummy;
}
