# rlang

[![Build Status](https://travis-ci.org/tidyverse/rlang.svg?branch=master)](https://travis-ci.org/tidyverse/rlang)
[![Coverage Status](https://img.shields.io/codecov/c/github/tidyverse/rlang/master.svg)](https://codecov.io/github/tidyverse/rlang?branch=master)

## Overview

The rlang package provides tools to work with core language features
of R and the tidyverse:

*   The __tidyeval__ framework, which is a well-founded system for non-standard
    evaluation built on quasiquotation (`UQ()`) and quosures (`quo()`). 
    Read more in `vignette("tidy-evaluation")`.

*   Consistent tools for working with base types:
    
    * Vectors, including construction (`lgl()`, `int()`, ...)
      coercion (`as_logical()`, `as_character()`, ...), and
      predicates (`is_logical()`, `is_character()`).
      
    * Language objects, such as calls (`lang()`) and symbols (`sym()`).
    
    * Attributes, e.g. `set_attrs()`, `set_names()`.
    
    * Functions, e.g. `new_function()`, `as_function()`, `is_function()`.
    
    * Environments, e.g. `env()`, `env_has()`, `env_get()`, `env_bind()`,
      `env_unbind()`.

*   A comprehensive set of predicates to determine if an object satisfies 
    various conditions, e.g. `has_length()`, `is_list()`, `is_empty()`.
    
*   The condition (message, warning, error) and restart system.

*   Call and context stacks.

## Installation

You can install the released version of rlang from CRAN with:

```r
install.packages("rlang")
```

Or install the development version from github with:

```r
# install.packages("devtools")
devtools::install_github("tidyverse/rlang", build_vignettes = TRUE)
```
