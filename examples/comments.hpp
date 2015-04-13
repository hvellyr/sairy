/*! @doc A Qt style comment */
int foo(int a, char* b);

/** @doc A java style comment */
int foo1();


/*! @doc A comment
 *
 * This comment spans multiple lines, includes some markup like @prm{this} or
 * @val{that}, and shows some other structures:
 * 
 * @itemize
 * @item a list item
 * @item another one
 * @end itemize
 * 
 * @admon{Note} Note which is a note admonition.
 * @end admon
 */
const char* boo();

/*! A comment
 *
 * Some more */
const char* boo23();


//! Comment ignored, because not linked to a declaration

//! Comment in doxygen_style
void doxygen_style();


/// Tripple slash comment with multiple lines.
/// Where the first line should be a autobrief line (because of the fullstop
/// ending its first setence.
void gaz();


//! Comment which
// is not continued
// properly
int foo2();

//!   A comment
//!
//! with an example
//!
//! @example
//!   let a = boo()
//! @end example
//!
//! @admon Note which is a note admonition.
//!
const char* boox();
