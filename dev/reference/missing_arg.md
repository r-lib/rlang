# Generate or handle a missing argument

These functions help using the missing argument as a regular R object.

- `missing_arg()` generates a missing argument.

- `is_missing()` is like
  [`base::missing()`](https://rdrr.io/r/base/missing.html) but also
  supports testing for missing arguments contained in other objects like
  lists. It is also more consistent with default arguments which are
  never treated as missing (see section below).

- `maybe_missing()` is useful to pass down an input that might be
  missing to another function, potentially substituting by a default
  value. It avoids triggering an "argument is missing" error.

## Usage

``` r
missing_arg()

is_missing(x)

maybe_missing(x, default = missing_arg())
```

## Arguments

- x:

  An object that might be the missing argument.

- default:

  The object to return if the input is missing, defaults to
  `missing_arg()`.

## Other ways to reify the missing argument

- `base::quote(expr = )` is the canonical way to create a missing
  argument object.

- [`expr()`](https://rlang.r-lib.org/dev/reference/expr.md) called
  without argument creates a missing argument.

- [`quo()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md)
  called without argument creates an empty quosure, i.e. a quosure
  containing the missing argument object.

## `is_missing()` and default arguments

The base function
[`missing()`](https://rlang.r-lib.org/dev/reference/missing.md) makes a
distinction between default values supplied explicitly and default
values generated through a missing argument:

    fn <- function(x = 1) base::missing(x)

    fn()
    #> [1] TRUE
    fn(1)
    #> [1] FALSE

This only happens within a function. If the default value has been
generated in a calling function, it is never treated as missing:

    caller <- function(x = 1) fn(x)
    caller()
    #> [1] FALSE

`rlang::is_missing()` simplifies these rules by never treating default
arguments as missing, even in internal contexts:

    fn <- function(x = 1) rlang::is_missing(x)

    fn()
    #> [1] FALSE
    fn(1)
    #> [1] FALSE

This is a little less flexible because you can't specialise behaviour
based on implicitly supplied default values. However, this makes the
behaviour of `is_missing()` and functions using it simpler to
understand.

## Fragility of the missing argument object

The missing argument is an object that triggers an error if and only if
it is the result of evaluating a symbol. No error is produced when a
function call evaluates to the missing argument object. For instance, it
is possible to bind the missing argument to a variable with an
expression like `x[[1]] <- missing_arg()`. Likewise, `x[[1]]` is safe to
use as argument, e.g. `list(x[[1]])` even when the result is the missing
object.

However, as soon as the missing argument is passed down between
functions through a bare variable, it is likely to cause a missing
argument error:

    x <- missing_arg()
    list(x)
    #> Error:
    #> ! argument "x" is missing, with no default

To work around this, `is_missing()` and `maybe_missing(x)` use a bit of
magic to determine if the input is the missing argument without
triggering a missing error.

    x <- missing_arg()
    list(maybe_missing(x))
    #> [[1]]
    #>

`maybe_missing()` is particularly useful for prototyping
meta-programming algorithms in R. The missing argument is a likely input
when computing on the language because it is a standard object in
formals lists. While C functions are always allowed to return the
missing argument and pass it to other C functions, this is not the case
on the R side. If you're implementing your meta-programming algorithm in
R, use `maybe_missing()` when an input might be the missing argument
object.

## Examples

``` r
# The missing argument usually arises inside a function when the
# user omits an argument that does not have a default:
fn <- function(x) is_missing(x)
fn()
#> [1] TRUE

# Creating a missing argument can also be useful to generate calls
args <- list(1, missing_arg(), 3, missing_arg())
quo(fn(!!! args))
#> <quosure>
#> expr: ^fn(1, , 3, )
#> env:  0x55b2702ba160

# Other ways to create that object include:
quote(expr = )
#> 
expr()
#> 

# It is perfectly valid to generate and assign the missing
# argument in a list.
x <- missing_arg()
l <- list(missing_arg())

# Just don't evaluate a symbol that contains the empty argument.
# Evaluating the object `x` that we created above would trigger an
# error.
# x  # Not run

# On the other hand accessing a missing argument contained in a
# list does not trigger an error because subsetting is a function
# call:
l[[1]]
#> 
is.null(l[[1]])
#> [1] FALSE

# In case you really need to access a symbol that might contain the
# empty argument object, use maybe_missing():
maybe_missing(x)
#> 
is.null(maybe_missing(x))
#> [1] FALSE
is_missing(maybe_missing(x))
#> [1] TRUE


# Note that base::missing() only works on symbols and does not
# support complex expressions. For this reason the following lines
# would throw an error:

#> missing(missing_arg())
#> missing(l[[1]])

# while is_missing() will work as expected:
is_missing(missing_arg())
#> [1] TRUE
is_missing(l[[1]])
#> [1] TRUE
```
