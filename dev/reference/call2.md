# Create a call

Quoted function calls are one of the two types of
[symbolic](https://rlang.r-lib.org/dev/reference/is_expression.md)
objects in R. They represent the action of calling a function, possibly
with arguments. There are two ways of creating a quoted call:

- By [quoting](https://rlang.r-lib.org/dev/reference/topic-defuse.md)
  it. Quoting prevents functions from being called. Instead, you get the
  description of the function call as an R object. That is, a quoted
  function call.

- By constructing it with
  [`base::call()`](https://rdrr.io/r/base/call.html),
  [`base::as.call()`](https://rdrr.io/r/base/call.html), or `call2()`.
  In this case, you pass the call elements (the function to call and the
  arguments to call it with) separately.

See section below for the difference between `call2()` and the base
constructors.

## Usage

``` r
call2(.fn, ..., .ns = NULL)
```

## Arguments

- .fn:

  Function to call. Must be a callable object: a string, symbol, call,
  or a function.

- ...:

  \<[dynamic](https://rlang.r-lib.org/dev/reference/dyn-dots.md)\>
  Arguments for the function call. Empty arguments are preserved.

- .ns:

  Namespace with which to prefix `.fn`. Must be a string or symbol.

## Difference with base constructors

`call2()` is more flexible than
[`base::call()`](https://rdrr.io/r/base/call.html):

- The function to call can be a string or a
  [callable](https://rlang.r-lib.org/dev/reference/is_callable.md)
  object: a symbol, another call (e.g. a `$` or `[[` call), or a
  function to inline. [`base::call()`](https://rdrr.io/r/base/call.html)
  only supports strings and you need to use
  [`base::as.call()`](https://rdrr.io/r/base/call.html) to construct a
  call with a callable object.

      call2(list, 1, 2)

      as.call(list(list, 1, 2))

- The `.ns` argument is convenient for creating namespaced calls.

      call2("list", 1, 2, .ns = "base")

      # Equivalent to
      ns_call <- call("::", as.symbol("list"), as.symbol("base"))
      as.call(list(ns_call, 1, 2))

- `call2()` has [dynamic
  dots](https://rlang.r-lib.org/dev/reference/list2.md) support. You can
  splice lists of arguments with `!!!` or unquote an argument name with
  glue syntax.

      args <- list(na.rm = TRUE, trim = 0)

      call2("mean", 1:10, !!!args)

      # Equivalent to
      as.call(c(list(as.symbol("mean"), 1:10), args))

## Caveats of inlining objects in calls

`call2()` makes it possible to inline objects in calls, both in function
and argument positions. Inlining an object or a function has the
advantage that the correct object is used in all environments. If all
components of the code are inlined, you can even evaluate in the [empty
environment](https://rlang.r-lib.org/dev/reference/empty_env.md).

However inlining also has drawbacks. It can cause issues with NSE
functions that expect symbolic arguments. The objects may also leak in
representations of the call stack, such as
[`traceback()`](https://rdrr.io/r/base/traceback.html).

## See also

[`call_modify()`](https://rlang.r-lib.org/dev/reference/call_modify.md)

## Examples

``` r
# fn can either be a string, a symbol or a call
call2("f", a = 1)
#> f(a = 1)
call2(quote(f), a = 1)
#> f(a = 1)
call2(quote(f()), a = 1)
#> f()(a = 1)

#' Can supply arguments individually or in a list
call2(quote(f), a = 1, b = 2)
#> f(a = 1, b = 2)
call2(quote(f), !!!list(a = 1, b = 2))
#> f(a = 1, b = 2)

# Creating namespaced calls is easy:
call2("fun", arg = quote(baz), .ns = "mypkg")
#> mypkg::fun(arg = baz)

# Empty arguments are preserved:
call2("[", quote(x), , drop = )
#> x[, drop = ]
```
