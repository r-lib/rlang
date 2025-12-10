# Get or set function body

`fn_body()` is a simple wrapper around
[`base::body()`](https://rdrr.io/r/base/body.html). It always returns a
`\{` expression and throws an error when the input is a primitive
function (whereas [`body()`](https://rdrr.io/r/base/body.html) returns
`NULL`). The setter version preserves attributes, unlike `body<-`.

## Usage

``` r
fn_body(fn = caller_fn())

fn_body(fn) <- value
```

## Arguments

- fn:

  A function. It is looked up in the calling frame if not supplied.

- value:

  New formals or formals names for `fn`.

## Examples

``` r
# fn_body() is like body() but always returns a block:
fn <- function() do()
body(fn)
#> do()
fn_body(fn)
#> {
#>     do()
#> }

# It also throws an error when used on a primitive function:
try(fn_body(base::list))
#> Error in fn_body(base::list) : 
#>   `fn` must be an R function, not a primitive function.
```
