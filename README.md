# lazyeval

[![Build Status](https://travis-ci.org/hadley/lazyeval.png?branch=master)](https://travis-ci.org/hadley/lazyeval)
[![Coverage Status](https://img.shields.io/codecov/c/github/hadley/lazyeval/master.svg)](https://codecov.io/github/hadley/lazyeval?branch=master)

Lazy evaluation is a principled way to do non-standard evaluation (NSE) in R. It is centered around formulas which capture an expression and the current environment.

There are three key components:

* `feval()` allows you to evaluate a formula in its enclosing environment.
  When used with an optional `data` argument, it provides `.data` and `.env` 
  pronouns so you can be unambiguous when needed.
  
* `finterp()` (called by `feval()`) implements full quasiquotation with
  both unquote, `(( ))`, and unquote-splice, `({ })`, operators. This makes
  it easy to generate code.
  
* `explicit_promise()` turns an unevaluated argument (a _promise_) into
  a formula.

You use lazy evaluation by requiring the user to "quote" specially evaluated arguments with `~`, and then using the lazyeval package to compute with those formulas. It is also possible to eliminate the use of the `~` by converting promises to formulas. This does make programming with such functions a little harder, but it can be worth it in certain situations.


See `vignette("lazyeval")` for more details.

## Installation

Install the released version from CRAN with:

```R
install.packages("lazyeval")
```

Install the development version from github with:

```R
# install.packages("devtools")
devtools::install_github("hadley/lazyeval", build_vignettes = TRUE)
```
