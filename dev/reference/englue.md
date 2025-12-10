# Defuse function arguments with glue

`englue()` creates a string with the [glue
operators](https://rlang.r-lib.org/dev/reference/glue-operators.md) `{`
and `{{`. These operators are normally used to inject names within
[dynamic dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md).
`englue()` makes them available anywhere within a function.

`englue()` must be used inside a function. `englue("{{ var }}")`
[defuses](https://rlang.r-lib.org/dev/reference/topic-defuse.md) the
argument `var` and transforms it to a string using the default name
operation.

## Usage

``` r
englue(x, env = caller_env(), error_call = current_env(), error_arg = "x")
```

## Arguments

- x:

  A string to interpolate with glue operators.

- env:

  User environment where the interpolation data lives in case you're
  wrapping `englue()` in another function.

- error_call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/dev/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) for more
  information.

- error_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

## Details

`englue("{{ var }}")` is equivalent to `as_label(enquo(var))`. It
[defuses](https://rlang.r-lib.org/dev/reference/topic-defuse.md) `arg`
and transforms the expression to a string with
[`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md).

In dynamic dots, using only `{` is allowed. In `englue()` you must use
`{{` at least once. Use
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
simple interpolation.

Before using `englue()` in a package, first ensure that glue is
installed by adding it to your `Imports:` section.

    usethis::use_package("glue", "Imports")

## Wrapping `englue()`

You can provide englue semantics to a user provided string by supplying
`env`. In this example we create a variant of `englue()` that supports a
special `.qux` pronoun by:

- Creating an environment `masked_env` that inherits from the user env,
  the one where their data lives.

- Overriding the `error_arg` and `error_call` arguments to point to our
  own argument name and call environment. This pattern is slightly
  different from usual error context passing because `englue()` is a
  backend function that uses its own error context by default (and not a
  checking function that uses *your* error context by default).

    my_englue <- function(text) {
      masked_env <- env(caller_env(), .qux = "QUX")

      englue(
        text,
        env = masked_env,
        error_arg = "text",
        error_call = current_env()
      )
    }

    # Users can then use your wrapper as they would use `englue()`:
    fn <- function(x) {
      foo <- "FOO"
      my_englue("{{ x }}_{.qux}_{foo}")
    }

    fn(bar)
    #> [1] "bar_QUX_FOO"

If you are creating a low level package on top of englue(), you should
also consider exposing `env`, `error_arg` and `error_call` in your
`englue()` wrapper so users can wrap your wrapper.

## See also

- [Injecting with !!, !!!, and glue
  syntax](https://rlang.r-lib.org/dev/reference/topic-inject.md)

## Examples

``` r
g <- function(var) englue("{{ var }}")
g(cyl)
#> [1] "cyl"
g(1 + 1)
#> [1] "1 + 1"
g(!!letters)
#> [1] "<chr>"

# These are equivalent to
as_label(quote(cyl))
#> [1] "cyl"
as_label(quote(1 + 1))
#> [1] "1 + 1"
as_label(letters)
#> [1] "<chr>"
```
