# Splice lists

**\[deprecated\]**

`dots_splice()` is like
[`dots_list()`](https://rlang.r-lib.org/dev/reference/list2.md) but
automatically splices list inputs.

## Usage

``` r
dots_splice(
  ...,
  .ignore_empty = c("trailing", "none", "all"),
  .preserve_empty = FALSE,
  .homonyms = c("keep", "first", "last", "error"),
  .check_assign = FALSE
)
```

## Arguments

- ...:

  Arguments to collect in a list. These dots are
  [dynamic](https://rlang.r-lib.org/dev/reference/dyn-dots.md).

- .ignore_empty:

  Whether to ignore empty arguments. Can be one of `"trailing"`,
  `"none"`, `"all"`. If `"trailing"`, only the last argument is ignored
  if it is empty.

- .preserve_empty:

  Whether to preserve the empty arguments that were not ignored. If
  `TRUE`, empty arguments are stored with
  [`missing_arg()`](https://rlang.r-lib.org/dev/reference/missing_arg.md)
  values. If `FALSE` (the default) an error is thrown when an empty
  argument is detected.

- .homonyms:

  How to treat arguments with the same name. The default, `"keep"`,
  preserves these arguments. Set `.homonyms` to `"first"` to only keep
  the first occurrences, to `"last"` to keep the last occurrences, and
  to `"error"` to raise an informative error and indicate what arguments
  have duplicated names.

- .check_assign:

  Whether to check for `<-` calls. When `TRUE` a warning recommends
  users to use `=` if they meant to match a function parameter or wrap
  the `<-` call in curly braces otherwise. This ensures assignments are
  explicit.
