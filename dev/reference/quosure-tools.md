# Quosure getters, setters and predicates

These tools inspect and modify
[quosures](https://rlang.r-lib.org/dev/reference/topic-quosure.md), a
type of [defused
expression](https://rlang.r-lib.org/dev/reference/topic-defuse.md) that
includes a reference to the context where it was created. A quosure is
guaranteed to evaluate in its original environment and can refer to
local objects safely.

- You can access the quosure components with `quo_get_expr()` and
  `quo_get_env()`.

- The `quo_` prefixed predicates test the expression of a quosure,
  `quo_is_missing()`, `quo_is_symbol()`, etc.

All `quo_` prefixed functions expect a quosure and will fail if supplied
another type of object. Make sure the input is a quosure with
[`is_quosure()`](https://rlang.r-lib.org/dev/reference/new_quosure.md).

## Usage

``` r
quo_is_missing(quo)

quo_is_symbol(quo, name = NULL)

quo_is_call(quo, name = NULL, n = NULL, ns = NULL)

quo_is_symbolic(quo)

quo_is_null(quo)

quo_get_expr(quo)

quo_get_env(quo)

quo_set_expr(quo, expr)

quo_set_env(quo, env)
```

## Arguments

- quo:

  A quosure to test.

- name:

  The name of the symbol or function call. If `NULL` the name is not
  tested.

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

- expr:

  A new expression for the quosure.

- env:

  A new environment for the quosure.

## Empty quosures and missing arguments

When missing arguments are captured as quosures, either through
[`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) or
[`quos()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md),
they are returned as an empty quosure. These quosures contain the
[missing argument](https://rlang.r-lib.org/dev/reference/missing_arg.md)
and typically have the [empty
environment](https://rlang.r-lib.org/dev/reference/empty_env.md) as
enclosure.

Use `quo_is_missing()` to test for a missing argument defused with
[`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md).

## See also

- [`quo()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md)
  for creating quosures by [argument
  defusal](https://rlang.r-lib.org/dev/reference/topic-defuse.md).

- [`new_quosure()`](https://rlang.r-lib.org/dev/reference/new_quosure.md)
  and
  [`as_quosure()`](https://rlang.r-lib.org/dev/reference/new_quosure.md)
  for assembling quosures from components.

- [What are quosures and when are they
  needed?](https://rlang.r-lib.org/dev/reference/topic-quosure.md) for
  an overview.

## Examples

``` r
quo <- quo(my_quosure)
quo
#> <quosure>
#> expr: ^my_quosure
#> env:  0x55b26f964b20


# Access and set the components of a quosure:
quo_get_expr(quo)
#> my_quosure
quo_get_env(quo)
#> <environment: 0x55b26f964b20>

quo <- quo_set_expr(quo, quote(baz))
quo <- quo_set_env(quo, empty_env())
quo
#> <quosure>
#> expr: ^baz
#> env:  empty

# Test wether an object is a quosure:
is_quosure(quo)
#> [1] TRUE

# If it is a quosure, you can use the specialised type predicates
# to check what is inside it:
quo_is_symbol(quo)
#> [1] TRUE
quo_is_call(quo)
#> [1] FALSE
quo_is_null(quo)
#> [1] FALSE

# quo_is_missing() checks for a special kind of quosure, the one
# that contains the missing argument:
quo()
#> <quosure>
#> expr: ^
#> env:  empty
quo_is_missing(quo())
#> [1] TRUE

fn <- function(arg) enquo(arg)
fn()
#> <quosure>
#> expr: ^
#> env:  empty
quo_is_missing(fn())
#> [1] TRUE
```
