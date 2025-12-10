# Match supplied arguments to function definition

`call_match()` is like
[`match.call()`](https://rdrr.io/r/base/match.call.html) with these
differences:

- It supports matching missing argument to their defaults in the
  function definition.

- It requires you to be a little more specific in some cases. Either all
  arguments are inferred from the call stack or none of them are (see
  the Inference section).

## Usage

``` r
call_match(
  call = NULL,
  fn = NULL,
  ...,
  defaults = FALSE,
  dots_env = NULL,
  dots_expand = TRUE
)
```

## Arguments

- call:

  A call. The arguments will be matched to `fn`.

- fn:

  A function definition to match arguments to.

- ...:

  These dots must be empty.

- defaults:

  Whether to match missing arguments to their defaults.

- dots_env:

  An execution environment where to find dots. If supplied and dots
  exist in this environment, and if `call` includes `...`, the forwarded
  dots are matched to numbered dots (e.g. `..1`, `..2`, etc). By default
  this is set to the empty environment which means that `...` expands to
  nothing.

- dots_expand:

  If `FALSE`, arguments passed through `...` will not be spliced into
  `call`. Instead, they are gathered in a pairlist and assigned to an
  argument named `...`. Gathering dots arguments is useful if you need
  to separate them from the other named arguments.

  Note that the resulting call is not meant to be evaluated since R does
  not support passing dots through a named argument, even if named
  `"..."`.

## Inference from the call stack

When `call` is not supplied, it is inferred from the call stack along
with `fn` and `dots_env`.

- `call` and `fn` are inferred from the calling environment:
  `sys.call(sys.parent())` and `sys.function(sys.parent())`.

- `dots_env` is inferred from the caller of the calling environment:
  `caller_env(2)`.

If `call` is supplied, then you must supply `fn` as well. Also consider
supplying `dots_env` as it is set to the empty environment when not
inferred.

## Examples

``` r
# `call_match()` supports matching missing arguments to their
# defaults
fn <- function(x = "default") fn
call_match(quote(fn()), fn)
#> fn()
call_match(quote(fn()), fn, defaults = TRUE)
#> fn(x = "default")
```
