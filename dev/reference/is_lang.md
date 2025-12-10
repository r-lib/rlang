# Is object a call?

**\[deprecated\]** These functions are deprecated, please use
[`is_call()`](https://rlang.r-lib.org/dev/reference/is_call.md) and its
`n` argument instead.

## Usage

``` r
is_lang(x, name = NULL, n = NULL, ns = NULL)
```

## Arguments

- x:

  An object to test. Formulas and quosures are treated literally.

- name:

  An optional name that the call should match. It is passed to
  [`sym()`](https://rlang.r-lib.org/dev/reference/sym.md) before
  matching. This argument is vectorised and you can supply a vector of
  names to match. In this case,
  [`is_call()`](https://rlang.r-lib.org/dev/reference/is_call.md)
  returns `TRUE` if at least one name matches.

- n:

  An optional number of arguments that the call should match.

- ns:

  The namespace of the call. If `NULL`, the namespace doesn't
  participate in the pattern-matching. If an empty string `""` and `x`
  is a namespaced call,
  [`is_call()`](https://rlang.r-lib.org/dev/reference/is_call.md)
  returns `FALSE`. If any other string,
  [`is_call()`](https://rlang.r-lib.org/dev/reference/is_call.md) checks
  that `x` is namespaced within `ns`.

  Can be a character vector of namespaces, in which case the call has to
  match at least one of them, otherwise
  [`is_call()`](https://rlang.r-lib.org/dev/reference/is_call.md)
  returns `FALSE`.
