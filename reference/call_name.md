# Extract function name or namespace of a call

`call_name()` and `call_ns()` extract the function name or namespace of
*simple* calls as a string. They return `NULL` for complex calls.

- Simple calls: `foo()`, `bar::foo()`.

- Complex calls: `foo()()`, `bar::foo`, `foo$bar()`,
  `(function() NULL)()`.

The `is_call_simple()` predicate helps you determine whether a call is
simple. There are two invariants you can count on:

1.  If `is_call_simple(x)` returns `TRUE`, `call_name(x)` returns a
    string. Otherwise it returns `NULL`.

2.  If `is_call_simple(x, ns = TRUE)` returns `TRUE`, `call_ns()`
    returns a string. Otherwise it returns `NULL`.

## Usage

``` r
call_name(call)

call_ns(call)

is_call_simple(x, ns = NULL)
```

## Arguments

- call:

  A defused call.

- x:

  An object to test.

- ns:

  Whether call is namespaced. If `NULL`, `is_call_simple()` is
  insensitive to namespaces. If `TRUE`, `is_call_simple()` detects
  namespaced calls. If `FALSE`, it detects unnamespaced calls.

## Value

The function name or namespace as a string, or `NULL` if the call is not
named or namespaced.

## Examples

``` r
# Is the function named?
is_call_simple(quote(foo()))
#> [1] TRUE
is_call_simple(quote(foo[[1]]()))
#> [1] FALSE

# Is the function namespaced?
is_call_simple(quote(list()), ns = TRUE)
#> [1] FALSE
is_call_simple(quote(base::list()), ns = TRUE)
#> [1] TRUE

# Extract the function name from quoted calls:
call_name(quote(foo(bar)))
#> [1] "foo"
call_name(quo(foo(bar)))
#> [1] "foo"

# Namespaced calls are correctly handled:
call_name(quote(base::matrix(baz)))
#> [1] "matrix"

# Anonymous and subsetted functions return NULL:
call_name(quote(foo$bar()))
#> NULL
call_name(quote(foo[[bar]]()))
#> NULL
call_name(quote(foo()()))
#> NULL

# Extract namespace of a call with call_ns():
call_ns(quote(base::bar()))
#> [1] "base"

# If not namespaced, call_ns() returns NULL:
call_ns(quote(bar()))
#> NULL
```
