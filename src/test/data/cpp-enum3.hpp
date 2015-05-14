/*! @doc Foo's doc */
class Foo {
public:
  enum {
    k_hello //!< @doc hello
  };

protected:
  enum {
    k_world //!< @doc world!
  };

private:
  enum {
    k_private
  };
};
