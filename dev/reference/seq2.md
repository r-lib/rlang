# Increasing sequence of integers in an interval

These helpers take two endpoints and return the sequence of all integers
within that interval. For `seq2_along()`, the upper endpoint is taken
from the length of a vector. Unlike
[`base::seq()`](https://rdrr.io/r/base/seq.html), they return an empty
vector if the starting point is a larger integer than the end point.

## Usage

``` r
seq2(from, to)

seq2_along(from, x)
```

## Arguments

- from:

  The starting point of the sequence.

- to:

  The end point.

- x:

  A vector whose length is the end point.

## Value

An integer vector containing a strictly increasing sequence.

## Examples

``` r
seq2(2, 10)
#> [1]  2  3  4  5  6  7  8  9 10
seq2(10, 2)
#> integer(0)
seq(10, 2)
#> [1] 10  9  8  7  6  5  4  3  2

seq2_along(10, letters)
#>  [1] 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
```
