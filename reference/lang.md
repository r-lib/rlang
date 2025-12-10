# Create a call

**\[deprecated\]** These functions are deprecated, please use
[`call2()`](https://rlang.r-lib.org/reference/call2.md) and
[`new_call()`](https://rlang.r-lib.org/reference/new_call.md) instead.

## Usage

``` r
lang(.fn, ..., .ns = NULL)
```

## Arguments

- .fn:

  Function to call. Must be a callable object: a string, symbol, call,
  or a function.

- ...:

  \<[dynamic](https://rlang.r-lib.org/reference/dyn-dots.md)\> Arguments
  for the function call. Empty arguments are preserved.

- .ns:

  Namespace with which to prefix `.fn`. Must be a string or symbol.
