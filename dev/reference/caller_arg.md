# Find the caller argument for error messages

`caller_arg()` is a variant of
[`substitute()`](https://rdrr.io/r/base/substitute.html) or
[`ensym()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md)
for arguments that reference other arguments. Unlike
[`substitute()`](https://rdrr.io/r/base/substitute.html) which returns
an expression, `caller_arg()` formats the expression as a single line
string which can be included in error messages.

- When included in an error message, the resulting label should
  generally be formatted as argument, for instance using the `.arg` in
  the cli package.

- Use `@inheritParams rlang::args_error_context` to document an `arg` or
  `error_arg` argument that takes `error_arg()` as default.

## Arguments

- arg:

  An argument name in the current function.

## Examples

``` r
arg_checker <- function(x, arg = caller_arg(x), call = caller_env()) {
  cli::cli_abort("{.arg {arg}} must be a thingy.", arg = arg, call = call)
}

my_function <- function(my_arg) {
  arg_checker(my_arg)
}

try(my_function(NULL))
#> Error in my_function(NULL) : `my_arg` must be a thingy.
```
