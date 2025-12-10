# Poke values into a vector

**\[experimental\]**

These tools are for R experts only. They copy elements from `y` into `x`
by mutation. You should only do this if you own `x`, i.e. if you have
created it or if you are certain that it doesn't exist in any other
context. Otherwise you might create unintended side effects that have
undefined consequences.

## Usage

``` r
vec_poke_n(x, start, y, from = 1L, n = length(y))

vec_poke_range(x, start, y, from = 1L, to = length(y) - from + 1L)
```

## Arguments

- x:

  The destination vector.

- start:

  The index indicating where to start modifying `x`.

- y:

  The source vector.

- from:

  The index indicating where to start copying from `y`.

- n:

  How many elements should be copied from `y` to `x`.

- to:

  The index indicating the end of the range to copy from `y`.
