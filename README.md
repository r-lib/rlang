rlang <img src="man/figures/rlang.png" align="right" />
=======================================================

[![Build Status](https://travis-ci.org/r-lib/rlang.svg?branch=master)](https://travis-ci.org/r-lib/rlang)
[![Coverage Status](https://codecov.io/gh/r-lib/rlang/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/rlang?branch=master)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

**Important**: The rlang API is still maturing. Please see
`?rlang::lifecycle` for the list of functions that are considered
stable.


## Overview

The rlang package provides tools to work with core language features
of R and the tidyverse:

*   The __tidy eval__ framework, which is a well-founded system for
    non-standard evaluation built on quasiquotation (`!!`) and
    quosures (`quo()`).

*   Consistent tools for working with base types. Note that overall
    this is a work in progress that is still in flux:

    * Vectors, including construction (`lgl()`, `int()`, ...)
      coercion (`as_logical()`, `as_character()`, ...), and
      predicates (`is_logical()`, `is_character()`).

    * Language objects, such as calls (`lang()`) and symbols (`sym()`).

    * Attributes, e.g. `set_names()`.

    * Functions, e.g. `new_function()`, `as_function()`. The latter
      supports the purrr-style formula notation for lambda functions.

    * Environments, e.g. `env()`, `env_has()`, `env_get()`, `env_bind()`,
      `env_unbind()`.

*   A comprehensive set of predicates to determine if an object satisfies
    various conditions, e.g. `has_length()`, `is_list()`, `is_empty()`.

*   The condition (message, warning, error) and restart system.


## Installation

You can install the released version of rlang from CRAN with:

```r
install.packages("rlang")
```

Or install the development version from github with:

```r
# install.packages("devtools")
devtools::install_github("r-lib/rlang", build_vignettes = TRUE)
```
