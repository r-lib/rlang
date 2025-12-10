# Check that arguments are mutually exclusive

`check_exclusive()` checks that only one argument is supplied out of a
set of mutually exclusive arguments. An informative error is thrown if
multiple arguments are supplied.

## Usage

``` r
check_exclusive(..., .require = TRUE, .frame = caller_env(), .call = .frame)
```

## Arguments

- ...:

  Function arguments.

- .require:

  Whether at least one argument must be supplied.

- .frame:

  Environment where the arguments in `...` are defined.

- .call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.

## Value

The supplied argument name as a string. If `.require` is `FALSE` and no
argument is supplied, the empty string `""` is returned.

## Examples

``` r
f <- function(x, y) {
  switch(
    check_exclusive(x, y),
    x = message("`x` was supplied."),
    y = message("`y` was supplied.")
  )
}

# Supplying zero or multiple arguments is forbidden
try(f())
#> Error in f() : One of `x` or `y` must be supplied.
try(f(NULL, NULL))
#> Error in f(NULL, NULL) : 
#>   Exactly one of `x` or `y` must be supplied.

# The user must supply one of the mutually exclusive arguments
f(NULL)
#> `x` was supplied.
f(y = NULL)
#> `y` was supplied.


# With `.require` you can allow zero arguments
f <- function(x, y) {
  switch(
    check_exclusive(x, y, .require = FALSE),
    x = message("`x` was supplied."),
    y = message("`y` was supplied."),
    message("No arguments were supplied")
  )
}
f()
#> No arguments were supplied
```
