rlang <img src="man/figures/rlang.png" align="right" />
=======================================================

[![Build Status](https://travis-ci.org/r-lib/rlang.svg?branch=master)](https://travis-ci.org/r-lib/rlang)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-lib/rlang?branch=master&svg=true)](https://ci.appveyor.com/project/lionel-/rlang)
[![Coverage Status](https://codecov.io/gh/r-lib/rlang/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/rlang?branch=master)
[![Lifecycle Status](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/)

**Important**: The rlang API is still maturing. Please see
`?rlang::lifecycle` for the list of functions that are considered
stable.


## Overview

The rlang package provides tools to work with core language features
of R and the tidyverse:

*   The __tidy eval__ framework, which is a well-founded system for
    non-standard evaluation built on quasiquotation (`!!`) and
    quoted arguments (`enquo()`). See <https://tidyeval.tidyverse.org>.

*   User-friendly __error reporting__ with backtraces and chained errors
    (`abort()`, `trace_back()`, `with_abort()`).

*   A consistent API for working with __base types__. Note that overall
    this is a work in progress that is still in flux:

    * Environments, e.g. `env()`, `env_has()`, `env_get()`,
      `env_bind()`, `env_unbind()`, `env_print()`.

    * Calls and symbols, e.g. `call2()`, `is_call()`, `sym()`, `syms()`.

    * Functions, e.g. `new_function()`, `as_function()`. The latter
      supports the purrr-style formula notation for lambda functions.

    * Vectors, including construction (`lgl()`, `int()`, ...) and
      predicates (`is_logical()`, `is_character()`).

    * Attributes, e.g. `set_names()`.


## Installation

You can install the released version of rlang from CRAN with:

```r
install.packages("rlang")
```

Or install the development version from github with:

```r
# install.packages("remotes")
remotes::install_github("r-lib/rlang")
```

## Cheatsheet

<a href="https://github.com/rstudio/cheatsheets/blob/master/tidyeval.pdf"><img src="https://raw.githubusercontent.com/rstudio/cheatsheets/master/pngs/thumbnails/tidy-eval-thumbs.png" alt = "tidy eval cheatsheet" width="630" height="252"/></a>  
