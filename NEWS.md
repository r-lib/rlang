
# rlang 0.1.2.9000

* `new_cnd()` is now `cnd()` for consistency with other constructors.
  Also, `cnd_error()`, `cnd_warning()` and `cnd_message()` are now
  `error_cnd()`, `warning_cnd()` and `message_cnd()` to follow our
  naming scheme according to which the type of output is a suffix
  rather than a prefix.

* Condition signallers such as `cnd_signal()` and `abort()` now accept
  a call depth as `call` arguments. This allows plucking a call from
  further up the call stack (#30).

* `cnd_signal()` now returns invisibly.

* `cnd_signal()` and `cnd_abort()` now accept character vectors to
  create typed conditions with several S3 subclasses.

* New `env_set()` function to set a value in an environment or a
  scope. If the `create` argument is `FALSE`, it only overwrites
  existing bindings and issues an error otherwise (#162).

* New `fn_fmls<-` and `fn_fmls_names<-` setters.

* Calling `enquo()` at top-level is now an alias for `quo()` instead
  of throwing an error. This is intended to make it easier to teach
  tidyeval. Similarly, `enexpr()` at top-level is an alias for
  `expr()`.

* `ensym()` is a new variant of `enexpr()` that expects a symbol.

* New function `chr_translate_unicode()` for turning characters
  serialised to unicode point form (e.g. `<U+xxxx>`) to UTF-8. In
  addition, `as_utf8_character()` now translates those as well.

* `eval_tidy()` no longer maps over lists but returns them literally.
  This behaviour is an overlook from past refactorings and was never
  documented.

* When nested quosures are evaluated with `eval_tidy()`, the `.env`
  pronoun now correctly refers to the current quosure under evaluation
  (#174). Previously it would always refer to the environment of the
  outermost quosure.


# rlang 0.1.2

This hotfix release makes rlang compatible with the R 3.1 branch.


# rlang 0.1.1

This release includes two important fixes for tidy evaluation:

* Bare formulas are now evaluated in the correct environment in
  tidyeval functions.

* `enquo()` now works properly within compiled functions. Before this
  release, constants optimised by the bytecode compiler couldn't be
  enquoted.


## New functions:

* The `new_environment()` constructor creates a child of the empty
  environment and takes an optional named list of data to populate it.
  Compared to `env()` and `child_env()`, it is meant to create
  environments as data structures rather than as part of a scope
  hierarchy.

* The `new_language()` constructor creates calls out of a callable
  object (a function or an expression) and a pairlist of arguments. It
  is useful to avoid costly internal coercions between lists and
  pairlists of arguments.


## UI improvements:

* `env_child()`'s first argument is now `.parent` instead of `parent`.

* `mut_` setters like `mut_attrs()` and environment helpers like
  `env_bind()` and `env_unbind()` now return their (modified) input
  invisibly. This follows the tidyverse convention that functions
  called primarily for their side effects should return their input
  invisibly.

* `is_pairlist()` now returns `TRUE` for `NULL`. We added `is_node()`
  to test for actual pairlist nodes. In other words, `is_pairlist()`
  tests for the data structure while `is_node()` tests for the type.


## Bugfixes:

* `env()` and `env_child()` can now get arguments whose names start
  with `.`.  Prior to this fix, these arguments were partial-matching
  on `env_bind()`'s `.env` argument.

* The internal `replace_na()` symbol was renamed to avoid a collision
  with an exported function in tidyverse. This solves an issue
  occurring in old versions of R prior to 3.3.2 (#133).


# rlang 0.1.0

Initial release.
