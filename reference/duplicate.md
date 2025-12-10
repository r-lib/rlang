# Duplicate an R object

`duplicate()` is an interface to the C-level `duplicate()` and
`shallow_duplicate()` functions. It is mostly meant for users of the C
API of R, e.g. for debugging, experimenting, or prototyping C code in R.

## Usage

``` r
duplicate(x, shallow = FALSE)
```

## Arguments

- x:

  An R object. Uncopyable objects like symbols and environments are
  returned as is (just like with `<-`).

- shallow:

  Recursive data structures like lists, calls and pairlists are
  duplicated in full by default. A shallow copy only duplicates the
  top-level data structure.

## See also

pairlist
