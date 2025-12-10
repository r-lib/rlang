# Extract arguments from a function

`fn_fmls()` returns a named list of formal arguments. `fn_fmls_names()`
returns the names of the arguments. `fn_fmls_syms()` returns formals as
a named list of symbols. This is especially useful for forwarding
arguments in [constructed
calls](https://rlang.r-lib.org/reference/lang.md).

## Usage

``` r
fn_fmls(fn = caller_fn())

fn_fmls_names(fn = caller_fn())

fn_fmls_syms(fn = caller_fn())

fn_fmls(fn) <- value

fn_fmls_names(fn) <- value
```

## Arguments

- fn:

  A function. It is looked up in the calling frame if not supplied.

- value:

  New formals or formals names for `fn`.

## Details

Unlike [`formals()`](https://rdrr.io/r/base/formals.html), these helpers
throw an error with primitive functions instead of returning `NULL`.

## See also

[`call_args()`](https://rlang.r-lib.org/reference/call_args.md) and
[`call_args_names()`](https://rlang.r-lib.org/reference/call_args.md)

## Examples

``` r
# Extract from current call:
fn <- function(a = 1, b = 2) fn_fmls()
fn()
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 

# fn_fmls_syms() makes it easy to forward arguments:
call2("apply", !!! fn_fmls_syms(lapply))
#> apply(X = X, FUN = FUN, ...)

# You can also change the formals:
fn_fmls(fn) <- list(A = 10, B = 20)
fn()
#> $A
#> [1] 10
#> 
#> $B
#> [1] 20
#> 

fn_fmls_names(fn) <- c("foo", "bar")
fn()
#> $foo
#> [1] 10
#> 
#> $bar
#> [1] 20
#> 
```
