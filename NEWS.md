# lazyeval 0.1.10.9000

* `funwrap()` "unwraps" a formula removing one level from the tree of 
  parents.

* `explicit_promise()` and `explicit_dots()` make it easy to turn promises
  and `...` into formulas. These should be used sparingly.

* `finterp()` unquotes expressions wrapped in `(( ))`.

* Implement formula helpers `is.formula()` and `rhs()`.

* `feval()` evaluate formulas. If supplied, values are first looked for 
  in an optional `data` argument. Pronouns `.data` and `.env` can be
  used to resolve ambiguity in this case.

* `lazy_dots()` gains `.ignore_empty` argument to drop extra arguments (#32).

* `interp.formula()` only accepts single-sided formulas (#37).

* `interp()` accepts an environment in `.values` (#35).

* `interp.character()` always produes a single string, regardless of
  input length (#27).

* Fixed an infinite loop in `lazy_dots(.follow_symbols = TRUE)` (#22, #24)

* `lazy()` now fails with an informative error when it is applied on
  an object that has already been evaluated (#23, @lionel-).

* `lazy()` no longer follows the expressions of lazily loaded objects
  (#18, @lionel-).

# lazyeval 0.1.10

* `as.lazy_dots()` gains a method for NULL, returning a zero-length
  list.

* `auto_names()` no longer truncates symbols (#19, #20)
