# lazyeval 0.1.10.9000

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
