# Create a new call from components

Create a new call from components

## Usage

``` r
new_call(car, cdr = NULL)
```

## Arguments

- car:

  The head of the call. It should be a
  [callable](https://rlang.r-lib.org/reference/is_callable.md) object: a
  symbol, call, or literal function.

- cdr:

  The tail of the call, i.e. a
  [pairlist](https://rlang.r-lib.org/reference/new_node.md) of
  arguments.
