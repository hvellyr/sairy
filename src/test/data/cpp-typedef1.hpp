#include <vector>

class Xyz { };

/*! @doc Dummy type */
class Dummy { };

namespace moo {

/*! @doc A number */
typedef int Foo;

/*! @doc A container */
typedef std::vector<int> Bar;

/*! @doc My type */
typedef Xyz MyXyz;

/*! @doc Another type */
typedef Dummy MyDummy;
}
