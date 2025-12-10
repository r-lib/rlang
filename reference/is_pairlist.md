# Is object a node or pairlist?

- `is_pairlist()` checks that `x` has type `pairlist`.

- `is_node()` checks that `x` has type `pairlist` or `language`. It
  tests whether `x` is a node that has a CAR and a CDR, including
  callable nodes (language objects).

- `is_node_list()` checks that `x` has type `pairlist` or `NULL`. `NULL`
  is the empty node list.

## Usage

``` r
is_pairlist(x)

is_node(x)

is_node_list(x)
```

## Arguments

- x:

  Object to test.

## Life cycle

These functions are experimental. We are still figuring out a good
naming convention to refer to the different lisp-like lists in R.

## See also

[`is_call()`](https://rlang.r-lib.org/reference/is_call.md) tests for
language nodes.
