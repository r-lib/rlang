# Extract arguments from a call

Extract arguments from a call

## Usage

``` r
call_args(call)

call_args_names(call)
```

## Arguments

- call:

  A defused call.

## Value

A named list of arguments.

## See also

[`fn_fmls()`](https://rlang.r-lib.org/reference/fn_fmls.md) and
[`fn_fmls_names()`](https://rlang.r-lib.org/reference/fn_fmls.md)

## Examples

``` r
call <- quote(f(a, b))

# Subsetting a call returns the arguments converted to a language
# object:
call[-1]
#> a(b)

# On the other hand, call_args() returns a regular list that is
# often easier to work with:
str(call_args(call))
#> List of 2
#>  $ : symbol a
#>  $ : symbol b

# When the arguments are unnamed, a vector of empty strings is
# supplied (rather than NULL):
call_args_names(call)
#> [1] "" ""
```
