# Create a function

This constructs a new function given its three components: list of
arguments, body code and parent environment.

## Usage

``` r
new_function(args, body, env = caller_env())
```

## Arguments

- args:

  A named list or pairlist of default arguments. Note that if you want
  arguments that don't have defaults, you'll need to use the special
  function
  [`pairlist2()`](https://rlang.r-lib.org/dev/reference/pairlist2.md).
  If you need quoted defaults, use
  [`exprs()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md).

- body:

  A language object representing the code inside the function. Usually
  this will be most easily generated with
  [`base::quote()`](https://rdrr.io/r/base/substitute.html)

- env:

  The parent environment of the function, defaults to the calling
  environment of `new_function()`

## Examples

``` r
f <- function() letters
g <- new_function(NULL, quote(letters))
identical(f, g)
#> [1] TRUE

# Pass a list or pairlist of named arguments to create a function
# with parameters. The name becomes the parameter name and the
# argument the default value for this parameter:
new_function(list(x = 10), quote(x))
#> function (x = 10) 
#> x
#> <environment: 0x55f2fd544950>
new_function(pairlist2(x = 10), quote(x))
#> function (x = 10) 
#> x
#> <environment: 0x55f2fd544950>

# Use `exprs()` to create quoted defaults. Compare:
new_function(pairlist2(x = 5 + 5), quote(x))
#> function (x = 10) 
#> x
#> <environment: 0x55f2fd544950>
new_function(exprs(x = 5 + 5), quote(x))
#> function (x = 5 + 5) 
#> x
#> <environment: 0x55f2fd544950>

# Pass empty arguments to omit defaults. `list()` doesn't allow
# empty arguments but `pairlist2()` does:
new_function(pairlist2(x = , y = 5 + 5), quote(x + y))
#> function (x, y = 10) 
#> x + y
#> <environment: 0x55f2fd544950>
new_function(exprs(x = , y = 5 + 5), quote(x + y))
#> function (x, y = 5 + 5) 
#> x + y
#> <environment: 0x55f2fd544950>
```
