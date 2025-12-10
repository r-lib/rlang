# Jump to or from a frame

**\[questioning\]**

While [`base::return()`](https://rdrr.io/r/base/function.html) can only
return from the current local frame, `return_from()` will return from
any frame on the current evaluation stack, between the global and the
currently active context.

## Usage

``` r
return_from(frame, value = NULL)
```

## Arguments

- frame:

  An execution environment of a currently running function.

- value:

  The return value.

## Examples

``` r
fn <- function() {
  g(current_env())
  "ignored"
}
g <- function(env) {
  h(env)
  "ignored"
}
h <- function(env) {
  return_from(env, "early return")
  "ignored"
}

fn()
#> [1] "early return"
```
