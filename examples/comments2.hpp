/*! @doc Foo is the toplevel namespace for all bar code */
namespace foo {
namespace bar {

//! @doc A comment
//!
//! with an example
//!
//! @example
//!   let a = boo()
//! @end example
//!
//! @admon{Note} which is a note admonition.@end{admon}
//!
const char* boo();

/*! @doc Returns the name of the app */
extern const char* app_name;
}}

// /* @doc boo boo */
// const char* foo::bar::boo() { }
