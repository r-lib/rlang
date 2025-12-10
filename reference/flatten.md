# Flatten or squash a list of lists into a simpler vector

**\[deprecated\]**

These functions are deprecated in favour of
[`purrr::list_c()`](https://purrr.tidyverse.org/reference/list_c.html)
and
[`purrr::list_flatten()`](https://purrr.tidyverse.org/reference/list_flatten.html).

`flatten()` removes one level hierarchy from a list, while `squash()`
removes all levels. These functions are similar to
[`unlist()`](https://rdrr.io/r/base/unlist.html) but they are
type-stable so you always know what the type of the output is.

## Usage

``` r
flatten(x)

flatten_lgl(x)

flatten_int(x)

flatten_dbl(x)

flatten_cpl(x)

flatten_chr(x)

flatten_raw(x)

squash(x)

squash_lgl(x)

squash_int(x)

squash_dbl(x)

squash_cpl(x)

squash_chr(x)

squash_raw(x)

flatten_if(x, predicate = is_spliced)

squash_if(x, predicate = is_spliced)
```

## Arguments

- x:

  A list to flatten or squash. The contents of the list can be anything
  for unsuffixed functions `flatten()` and `squash()` (as a list is
  returned), but the contents must match the type for the other
  functions.

- predicate:

  A function of one argument returning whether it should be spliced.

## Value

`flatten()` returns a list, `flatten_lgl()` a logical vector,
`flatten_int()` an integer vector, `flatten_dbl()` a double vector, and
`flatten_chr()` a character vector. Similarly for `squash()` and the
typed variants (`squash_lgl()` etc).

## Examples

``` r
x <- replicate(2, sample(4), simplify = FALSE)
x
#> [[1]]
#> [1] 1 4 3 2
#> 
#> [[2]]
#> [1] 3 1 2 4
#> 

flatten(x)
#> Warning: `flatten()` is deprecated as of rlang 1.1.0.
#> ℹ Please use `purrr::list_flatten()` or `purrr::list_c()`.
#> This warning is displayed once every 8 hours.
#> [[1]]
#> [1] 1 4 3 2
#> 
#> [[2]]
#> [1] 3 1 2 4
#> 
flatten_int(x)
#> Warning: `flatten_int()` is deprecated as of rlang 1.1.0.
#> ℹ Please use `purrr::list_flatten()` and/or `purrr::list_c()`.
#> This warning is displayed once every 8 hours.
#> [1] 1 4 3 2 3 1 2 4

# With flatten(), only one level gets removed at a time:
deep <- list(1, list(2, list(3)))
flatten(deep)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [[3]][[1]]
#> [1] 3
#> 
#> 
flatten(flatten(deep))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 

# But squash() removes all levels:
squash(deep)
#> Warning: `squash()` is deprecated as of rlang 1.1.0.
#> This warning is displayed once every 8 hours.
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
squash_dbl(deep)
#> Warning: `squash_dbl()` is deprecated as of rlang 1.1.0.
#> This warning is displayed once every 8 hours.
#> [1] 1 2 3

# The typed flatten functions remove one level and coerce to an atomic
# vector at the same time:
flatten_dbl(list(1, list(2)))
#> Warning: `flatten_dbl()` is deprecated as of rlang 1.1.0.
#> ℹ Please use `purrr::list_flatten()` and/or `purrr::list_c()`.
#> This warning is displayed once every 8 hours.
#> [1] 1 2

# Only bare lists are flattened, but you can splice S3 lists
# explicitly:
foo <- set_attrs(list("bar"), class = "foo")
#> Warning: `set_attrs()` is deprecated as of rlang 0.3.0
#> This warning is displayed once every 8 hours.
str(flatten(list(1, foo, list(100))))
#> List of 3
#>  $ : num 1
#>  $ :List of 1
#>   ..$ : chr "bar"
#>   ..- attr(*, "class")= chr "foo"
#>  $ : num 100
str(flatten(list(1, splice(foo), list(100))))
#> List of 3
#>  $ : num 1
#>  $ : chr "bar"
#>  $ : num 100

# Instead of splicing manually, flatten_if() and squash_if() let
# you specify a predicate function:
is_foo <- function(x) inherits(x, "foo") || is_bare_list(x)
str(flatten_if(list(1, foo, list(100)), is_foo))
#> List of 3
#>  $ : num 1
#>  $ : chr "bar"
#>  $ : num 100

# squash_if() does the same with deep lists:
deep_foo <- list(1, list(foo, list(foo, 100)))
str(deep_foo)
#> List of 2
#>  $ : num 1
#>  $ :List of 2
#>   ..$ :List of 1
#>   .. ..$ : chr "bar"
#>   .. ..- attr(*, "class")= chr "foo"
#>   ..$ :List of 2
#>   .. ..$ :List of 1
#>   .. .. ..$ : chr "bar"
#>   .. .. ..- attr(*, "class")= chr "foo"
#>   .. ..$ : num 100

str(squash(deep_foo))
#> List of 4
#>  $ : num 1
#>  $ :List of 1
#>   ..$ : chr "bar"
#>   ..- attr(*, "class")= chr "foo"
#>  $ :List of 1
#>   ..$ : chr "bar"
#>   ..- attr(*, "class")= chr "foo"
#>  $ : num 100
str(squash_if(deep_foo, is_foo))
#> Warning: `squash_if()` is deprecated as of rlang 1.1.0.
#> This warning is displayed once every 8 hours.
#> List of 4
#>  $ : num 1
#>  $ : chr "bar"
#>  $ : chr "bar"
#>  $ : num 100
```
