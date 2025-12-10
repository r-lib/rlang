# Set names of a vector

This is equivalent to
[`stats::setNames()`](https://rdrr.io/r/stats/setNames.html), with more
features and stricter argument checking.

## Usage

``` r
set_names(x, nm = x, ...)
```

## Arguments

- x:

  Vector to name.

- nm, ...:

  Vector of names, the same length as `x`. If length 1, `nm` is recycled
  to the length of `x` following the recycling rules of the tidyverse..

  You can specify names in the following ways:

  - If not supplied, `x` will be named to `as.character(x)`.

  - If `x` already has names, you can provide a function or formula to
    transform the existing names. In that case, `...` is passed to the
    function.

  - Otherwise if `...` is supplied, `x` is named to `c(nm, ...)`.

  - If `nm` is `NULL`, the names are removed (if present).

## Life cycle

`set_names()` is stable and exported in purrr.

## Examples

``` r
set_names(1:4, c("a", "b", "c", "d"))
#> a b c d 
#> 1 2 3 4 
set_names(1:4, letters[1:4])
#> a b c d 
#> 1 2 3 4 
set_names(1:4, "a", "b", "c", "d")
#> a b c d 
#> 1 2 3 4 

# If the second argument is omitted a vector is named with itself
set_names(letters[1:5])
#>   a   b   c   d   e 
#> "a" "b" "c" "d" "e" 

# Alternatively you can supply a function
set_names(1:10, ~ letters[seq_along(.)])
#>  a  b  c  d  e  f  g  h  i  j 
#>  1  2  3  4  5  6  7  8  9 10 
set_names(head(mtcars), toupper)
#>                    MPG CYL DISP  HP DRAT    WT  QSEC VS AM GEAR CARB
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

# If the input vector is unnamed, it is first named after itself
# before the function is applied:
set_names(letters, toupper)
#>   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q 
#> "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" 
#>   R   S   T   U   V   W   X   Y   Z 
#> "r" "s" "t" "u" "v" "w" "x" "y" "z" 

# `...` is passed to the function:
set_names(head(mtcars), paste0, "_foo")
#>                   mpg_foo cyl_foo disp_foo hp_foo drat_foo wt_foo
#> Mazda RX4            21.0       6      160    110     3.90  2.620
#> Mazda RX4 Wag        21.0       6      160    110     3.90  2.875
#> Datsun 710           22.8       4      108     93     3.85  2.320
#> Hornet 4 Drive       21.4       6      258    110     3.08  3.215
#> Hornet Sportabout    18.7       8      360    175     3.15  3.440
#> Valiant              18.1       6      225    105     2.76  3.460
#>                   qsec_foo vs_foo am_foo gear_foo carb_foo
#> Mazda RX4            16.46      0      1        4        4
#> Mazda RX4 Wag        17.02      0      1        4        4
#> Datsun 710           18.61      1      1        4        1
#> Hornet 4 Drive       19.44      1      0        3        1
#> Hornet Sportabout    17.02      0      0        3        2
#> Valiant              20.22      1      0        3        1

# If length 1, the second argument is recycled to the length of the first:
set_names(1:3, "foo")
#> foo foo foo 
#>   1   2   3 
set_names(list(), "")
#> named list()
```
