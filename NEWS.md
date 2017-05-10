
# rlang 0.1.9000

* `env_child()`'s first argument is now `.parent` instead of `parent`.

* `env()` and `env_child()` can now get arguments starting with `.`.
  Prior to this fix, these arguments were partial-matching on
  `env_bind()`'s `.env` argument.

* `mut_` setters like `mut_attrs()` and environment helpers like
  `env_bind()` and `env_unbind()` now return their (modified) input
  invisibly. This follows the tidyverse convention that functions
  called primarily for their side effects should return their input
  invisibly.

* The internal `replace_na()` symbol was renamed to avoid a collision
  with an exported function in tidyverse. This solves an issue
  occurring in old versions of R prior to 3.3.2 (#133).

* The `new_environment()` constructor creates a child of the empty
  environment and takes an optional named list of data to populate it.
  Compared to `env()` and `child_env()`, it is meant to create
  environments as data structures rather than as part of a scope
  hierarchy.

* The `new_language()` constructor creates calls from a callable
  object and a pairlist of arguments. It is useful to avoid costly
  internal coercions between lists and pairlists of arguments.

* `is_pairlist()` now returns `TRUE` for `NULL`. We added `is_node()`
  to test for actual pairlist nodes. In other words, `is_pairlist()`
  tests for the data structure while `is_node()` tests for the type.

* Bare formulas are now evaluated in the correct environment in
  tidyeval functions.


# rlang 0.1

Initial release.
