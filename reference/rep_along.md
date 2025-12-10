# Create vectors matching the length of a given vector

These functions take the idea of
[`seq_along()`](https://rdrr.io/r/base/seq.html) and apply it to
repeating values.

## Usage

``` r
rep_along(along, x)

rep_named(names, x)
```

## Arguments

- along:

  Vector whose length determine how many times `x` is repeated.

- x:

  Values to repeat.

- names:

  Names for the new vector. The length of `names` determines how many
  times `x` is repeated.

## See also

new-vector

## Examples

``` r
x <- 0:5
rep_along(x, 1:2)
#> [1] 1 2 1 2 1 2
rep_along(x, 1)
#> [1] 1 1 1 1 1 1

# Create fresh vectors by repeating missing values:
rep_along(x, na_int)
#> [1] NA NA NA NA NA NA
rep_along(x, na_chr)
#> [1] NA NA NA NA NA NA

# rep_named() repeats a value along a names vectors
rep_named(c("foo", "bar"), list(letters))
#> $foo
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
#> 
#> $bar
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
#> 
```
