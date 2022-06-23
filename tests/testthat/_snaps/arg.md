# matches arg

    `arg` must be one of "bar" or "baz", not "foo".

# gives an error with more than one arg

    `arg` must be length 1 or a permutation of `values`.

# gives error with different than rearranged arg vs value

    `myarg` must be one of "fun" or "bar", not "foo".

---

    `arg` must be length 1 or a permutation of `values`.

# `arg_match()` has informative error messages

    Code
      (expect_error(arg_match_wrapper("continuuos", c("discrete", "continuous"),
      "my_arg")))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "discrete" or "continuous", not "continuuos".
      i Did you mean "continuous"?
    Code
      (expect_error(arg_match_wrapper("fou", c("bar", "foo"), "my_arg")))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "bar" or "foo", not "fou".
      i Did you mean "foo"?
    Code
      (expect_error(arg_match_wrapper("fu", c("ba", "fo"), "my_arg")))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "ba" or "fo", not "fu".
      i Did you mean "fo"?
    Code
      (expect_error(arg_match_wrapper("baq", c("foo", "baz", "bas"), "my_arg")))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "foo", "baz", or "bas", not "baq".
      i Did you mean "baz"?
    Code
      (expect_error(arg_match_wrapper("", character(), "my_arg")))
    Output
      <error/rlang_error>
      Error in `arg_match0()`:
      ! `values` must have at least one element.
    Code
      (expect_error(arg_match_wrapper("fo", "foo", quote(f()))))
    Output
      <error/rlang_error>
      Error in `arg_match0()`:
      ! `arg_nm` must be a string or symbol.

# `arg_match()` provides no suggestion when the edit distance is too large

    Code
      (expect_error(arg_match0_wrapper("foobaz", c("fooquxs", "discrete"), "my_arg")))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "fooquxs" or "discrete", not "foobaz".
    Code
      (expect_error(arg_match0_wrapper("a", c("b", "c"), "my_arg")))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "b" or "c", not "a".

# `arg_match()` makes case-insensitive match

    Code
      (expect_error(arg_match0_wrapper("a", c("A", "B"), "my_arg"),
      "Did you mean \"A\"?"))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "A" or "B", not "a".
      i Did you mean "A"?
    Code
      (expect_error(arg_match0_wrapper("aa", c("AA", "aA"), "my_arg"),
      "Did you mean \"aA\"?"))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "AA" or "aA", not "aa".
      i Did you mean "aA"?

# check_required() checks argument is supplied (#1118)

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! `x` is absent but must be supplied.
    Code
      (expect_error(g()))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! `x` is absent but must be supplied.

# arg_match() supports symbols and scalar strings

    Code
      (expect_error(arg_match0_wrapper(chr_get("fo", 0L), c("bar", "foo"), "my_arg")))
    Output
      <error/rlang_error>
      Error in `arg_match0_wrapper()`:
      ! `my_arg` must be one of "bar" or "foo", not "fo".
      i Did you mean "foo"?

# arg_match() requires an argument symbol

    Code
      (expect_error(wrapper()))
    Output
      <error/rlang_error>
      Error in `wrapper()`:
      ! `arg` must be a symbol, not a string.
      i This is an internal error in the rlang package, please report it to the package authors.

# can match multiple arguments

    Code
      (expect_error(my_wrapper("ba")))
    Output
      <error/rlang_error>
      Error in `my_wrapper()`:
      ! `my_arg` must be one of "foo", "bar", or "baz", not "ba".
      i Did you mean "bar"?
    Code
      (expect_error(my_wrapper(c("foo", "ba"))))
    Output
      <error/rlang_error>
      Error in `my_wrapper()`:
      ! `my_arg` must be one of "foo", "bar", or "baz", not "ba".
      i Did you mean "bar"?

# arg_match0() defuses argument

    Code
      (expect_error(fn("foo")))
    Output
      <error/rlang_error>
      Error in `fn()`:
      ! `arg` must be one of "bar" or "baz", not "foo".
    Code
      (expect_error(arg_match0("foo", c("bar", "baz"))))
    Output
      <error/rlang_error>
      Error:
      ! `"foo"` must be one of "bar" or "baz", not "foo".

# check_exclusive works

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `check_exclusive()`:
      ! Must supply at least two arguments.
    Code
      (expect_error(g()))
    Output
      <error/rlang_error>
      Error in `check_exclusive()`:
      ! Must supply at least two arguments.
    Code
      (expect_error(h()))
    Output
      <error/rlang_error>
      Error in `check_exclusive()`:
      ! Must supply at least two arguments.

---

    Code
      (expect_error(f()))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! One of `foo` or `bar` must be supplied.

---

    Code
      # All arguments supplied
      (expect_error(g(foo, bar, baz)))
    Output
      <error/rlang_error>
      Error in `g()`:
      ! Exactly one of `foo`, `bar`, or `baz` must be supplied.
    Code
      # Some arguments supplied
      (expect_error(g(foo, bar)))
    Output
      <error/rlang_error>
      Error in `g()`:
      ! Exactly one of `foo`, `bar`, or `baz` must be supplied.
      x `foo` and `bar` were supplied together.

# arg_match() mentions correct call if wrong type is supplied (#1388)

    Code
      (expect_error(f(1)))
    Output
      <error/rlang_error>
      Error in `f()`:
      ! `my_arg` must be a string or character vector.
    Code
      (expect_error(g(1)))
    Output
      <error/rlang_error>
      Error in `g()`:
      ! `my_arg` must be a character vector, not a number.

# arg_match() backtrace highlights call and arg

    Code
      print_highlighted_trace(err)
    Output
      <error/rlang_error>
      Error in <<CALL `h()`>>:
      ! `my_arg` must be one of "foo" or "bar", not "f".
      i Did you mean "foo"?
      ---
      Backtrace:
           x
        1. +-rlang:::catch_error(f("f"))
        2. | \-rlang::catch_cnd(expr, "error")
        3. |   +-rlang::eval_bare(...)
        4. |   +-base::tryCatch(...)
        5. |   | \-base (local) tryCatchList(expr, classes, parentenv, handlers)
        6. |   |   \-base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
        7. |   |     \-base (local) doTryCatch(return(expr), name, parentenv, handler)
        8. |   \-base::force(expr)
        9. \-rlang (local) f("f")
       10.   \-rlang (local) g(x)
       11.     \-rlang (local) <<CALL h(>><<ARG my_arg = x>><<CALL )>>

