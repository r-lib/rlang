# Is object a call?

This function tests if `x` is a
[call](https://rlang.r-lib.org/reference/call2.md). This is a
pattern-matching predicate that returns `FALSE` if `name` and `n` are
supplied and the call does not match these properties.

## Usage

``` r
is_call(x, name = NULL, n = NULL, ns = NULL)
```

## Arguments

- x:

  An object to test. Formulas and quosures are treated literally.

- name:

  An optional name that the call should match. It is passed to
  [`sym()`](https://rlang.r-lib.org/reference/sym.md) before matching.
  This argument is vectorised and you can supply a vector of names to
  match. In this case, `is_call()` returns `TRUE` if at least one name
  matches.

- n:

  An optional number of arguments that the call should match.

- ns:

  The namespace of the call. If `NULL`, the namespace doesn't
  participate in the pattern-matching. If an empty string `""` and `x`
  is a namespaced call, `is_call()` returns `FALSE`. If any other
  string, `is_call()` checks that `x` is namespaced within `ns`.

  Can be a character vector of namespaces, in which case the call has to
  match at least one of them, otherwise `is_call()` returns `FALSE`.

## See also

[`is_expression()`](https://rlang.r-lib.org/reference/is_expression.md)

## Examples

``` r
is_call(quote(foo(bar)))
#> [1] TRUE

# You can pattern-match the call with additional arguments:
is_call(quote(foo(bar)), "foo")
#> [1] TRUE
is_call(quote(foo(bar)), "bar")
#> [1] FALSE
is_call(quote(foo(bar)), quote(foo))
#> [1] TRUE

# Match the number of arguments with is_call():
is_call(quote(foo(bar)), "foo", 1)
#> [1] TRUE
is_call(quote(foo(bar)), "foo", 2)
#> [1] FALSE


# By default, namespaced calls are tested unqualified:
ns_expr <- quote(base::list())
is_call(ns_expr, "list")
#> [1] TRUE

# You can also specify whether the call shouldn't be namespaced by
# supplying an empty string:
is_call(ns_expr, "list", ns = "")
#> [1] FALSE

# Or if it should have a namespace:
is_call(ns_expr, "list", ns = "utils")
#> [1] FALSE
is_call(ns_expr, "list", ns = "base")
#> [1] TRUE

# You can supply multiple namespaces:
is_call(ns_expr, "list", ns = c("utils", "base"))
#> [1] TRUE
is_call(ns_expr, "list", ns = c("utils", "stats"))
#> [1] FALSE

# If one of them is "", unnamespaced calls will match as well:
is_call(quote(list()), "list", ns = "base")
#> [1] FALSE
is_call(quote(list()), "list", ns = c("base", ""))
#> [1] TRUE
is_call(quote(base::list()), "list", ns = c("base", ""))
#> [1] TRUE


# The name argument is vectorised so you can supply a list of names
# to match with:
is_call(quote(foo(bar)), c("bar", "baz"))
#> [1] FALSE
is_call(quote(foo(bar)), c("bar", "foo"))
#> [1] TRUE
is_call(quote(base::list), c("::", ":::", "$", "@"))
#> [1] TRUE
```
