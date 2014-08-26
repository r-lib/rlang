# lazy

[![Build Status](https://travis-ci.org/hadley/lazy.png?branch=master)](https://travis-ci.org/hadley/lazy)

The lazy package provides the tools necessary to do non-standard evaluation "right" in R. There are three principles:

* Instead of using `substitute()`, use `lazy()` to capture both expression
  and environment. (Or use `lazy_dots()` to capture all promises in `...`)
  
* Every function that uses NSE should have a standard evaluation partner
  that does all the work. This function should end with `_`.
  
* The SE-partner has a flexible input specification to make it easy for people
  to program with.

See the vignette for more details.

To install:

```r
devtools::install_github("hadley/lazy")
```
