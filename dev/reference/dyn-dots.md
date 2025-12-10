# Dynamic dots features

The base `...` syntax supports:

- **Forwarding** arguments from function to function, matching them
  along the way to arguments.

- **Collecting** arguments inside data structures, e.g. with
  [`c()`](https://rdrr.io/r/base/c.html) or
  [`list()`](https://rdrr.io/r/base/list.html).

Dynamic dots offer a few additional features,
[injection](https://rlang.r-lib.org/dev/reference/topic-inject.md) in
particular:

1.  You can **splice** arguments saved in a list with the splice
    operator
    [`!!!`](https://rlang.r-lib.org/dev/reference/splice-operator.md).

2.  You can **inject** names with [glue
    syntax](https://rlang.r-lib.org/dev/reference/glue-operators.md) on
    the left-hand side of `:=`.

3.  Trailing commas are ignored, making it easier to copy and paste
    lines of arguments.

## Add dynamic dots support in your functions

If your function takes dots, adding support for dynamic features is as
easy as collecting the dots with
[`list2()`](https://rlang.r-lib.org/dev/reference/list2.md) instead of
[`list()`](https://rdrr.io/r/base/list.html). See also
[`dots_list()`](https://rlang.r-lib.org/dev/reference/list2.md), which
offers more control over the collection.

In general, passing `...` to a function that supports dynamic dots
causes your function to inherit the dynamic behaviour.

In packages, document dynamic dots with this standard tag:

     @param ... <[`dynamic-dots`][rlang::dyn-dots]> What these dots do.

## Examples

``` r
f <- function(...) {
  out <- list2(...)
  rev(out)
}

# Trailing commas are ignored
f(this = "that", )
#> $this
#> [1] "that"
#> 

# Splice lists of arguments with `!!!`
x <- list(alpha = "first", omega = "last")
f(!!!x)
#> $omega
#> [1] "last"
#> 
#> $alpha
#> [1] "first"
#> 

# Inject a name using glue syntax
if (is_installed("glue")) {
  nm <- "key"
  f("{nm}" := "value")
  f("prefix_{nm}" := "value")
}
#> $prefix_key
#> [1] "value"
#> 
```
