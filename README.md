# lazyeval

[![Build Status](https://travis-ci.org/hadley/lazyeval.png?branch=master)](https://travis-ci.org/hadley/lazyeval)

The lazyeval package provides the tools necessary to do non-standard evaluation (NSE) "right" in R. There are three principles:
  
  * Instead of using `substitute()`, use `lazyeval::lazy()` to capture both the 
    expression and the environment associated with a function argument (aka
    promise). Or use `lazyeval::lazy_dots(...)` to capture all the promises 
    in `...`.
    
  * Every function that uses NSE should have a SE partner with a name that ends 
    in `_`. This function should do all the work. The NSE function should look
    something like this:
  
    ```R
    # If only y needs NSE
    f <- function(x, y) {
      f_(x, lazyeval::lazy(y))
    }
    ```
  
    If needed, make the SE version generic, not the NSE version.
  
  * Instead the SE function, use `lazyeval::make_call()` or 
    `lazyeval::lazy_eval()` to evalute the expression in the right context. The 

See the vignette for more details.

## Installation

To install:

```r
devtools::install_github("hadley/lazyeval")
```
