# Modify the arguments of a call

If you are working with a user-supplied call, make sure the arguments
are standardised with
[`call_match()`](https://rlang.r-lib.org/dev/reference/call_match.md)
before modifying the call.

## Usage

``` r
call_modify(
  .call,
  ...,
  .homonyms = c("keep", "first", "last", "error"),
  .standardise = NULL,
  .env = caller_env()
)
```

## Arguments

- .call:

  Can be a call, a formula quoting a call in the right-hand side, or a
  frame object from which to extract the call expression.

- ...:

  \<[dynamic](https://rlang.r-lib.org/dev/reference/dyn-dots.md)\> Named
  or unnamed expressions (constants, names or calls) used to modify the
  call. Use [`zap()`](https://rlang.r-lib.org/dev/reference/zap.md) to
  remove arguments. Empty arguments are preserved.

- .homonyms:

  How to treat arguments with the same name. The default, `"keep"`,
  preserves these arguments. Set `.homonyms` to `"first"` to only keep
  the first occurrences, to `"last"` to keep the last occurrences, and
  to `"error"` to raise an informative error and indicate what arguments
  have duplicated names.

- .standardise, .env:

  Deprecated as of rlang 0.3.0. Please call
  [`call_match()`](https://rlang.r-lib.org/dev/reference/call_match.md)
  manually.

## Value

A quosure if `.call` is a quosure, a call otherwise.

## Examples

``` r
call <- quote(mean(x, na.rm = TRUE))

# Modify an existing argument
call_modify(call, na.rm = FALSE)
#> mean(x, na.rm = FALSE)
call_modify(call, x = quote(y))
#> mean(x, na.rm = TRUE, x = y)

# Remove an argument
call_modify(call, na.rm = zap())
#> mean(x)

# Add a new argument
call_modify(call, trim = 0.1)
#> mean(x, na.rm = TRUE, trim = 0.1)

# Add an explicit missing argument:
call_modify(call, na.rm = )
#> mean(x, na.rm = )

# Supply a list of new arguments with `!!!`
newargs <- list(na.rm = zap(), trim = 0.1)
call <- call_modify(call, !!!newargs)
call
#> mean(x, trim = 0.1)

# Remove multiple arguments by splicing zaps:
newargs <- rep_named(c("na.rm", "trim"), list(zap()))
call <- call_modify(call, !!!newargs)
call
#> mean(x)


# Modify the `...` arguments as if it were a named argument:
call <- call_modify(call, ... = )
call
#> mean(x, ...)

call <- call_modify(call, ... = zap())
call
#> mean(x)


# When you're working with a user-supplied call, standardise it
# beforehand in case it includes unmatched arguments:
user_call <- quote(matrix(x, nc = 3))
call_modify(user_call, ncol = 1)
#> matrix(x, nc = 3, ncol = 1)

# `call_match()` applies R's argument matching rules. Matching
# ensures you're modifying the intended argument.
user_call <- call_match(user_call, matrix)
user_call
#> matrix(data = x, ncol = 3)
call_modify(user_call, ncol = 1)
#> matrix(data = x, ncol = 1)


# By default, arguments with the same name are kept. This has
# subtle implications, for instance you can move an argument to
# last position by removing it and remapping it:
call <- quote(foo(bar = , baz))
call_modify(call, bar = zap(), bar = missing_arg())
#> foo(baz, bar = )

# You can also choose to keep only the first or last homonym
# arguments:
args <-  list(bar = zap(), bar = missing_arg())
call_modify(call, !!!args, .homonyms = "first")
#> foo(baz)
call_modify(call, !!!args, .homonyms = "last")
#> foo(bar = , baz)
```
