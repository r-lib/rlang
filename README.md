rlang <img src="man/figures/logo.png" align="right" />
=======================================================

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-lib/rlang/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/rlang/actions)
[![Codecov test coverage](https://codecov.io/gh/r-lib/rlang/branch/master/graph/badge.svg)](https://codecov.io/gh/r-lib/rlang?branch=master)
[![Lifecycle Status](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->


## Overview

The rlang package provides tools to work with core language features
of R and the tidyverse:

*   The __tidy eval__ framework, which is a well-founded system for
    non-standard evaluation built on quasiquotation (`!!`) and
    quoted arguments (`enquo()`). See <https://tidyeval.tidyverse.org>.

*   User-friendly __error reporting__ with backtraces and chained errors
    (`abort()`, `trace_back()`).

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
