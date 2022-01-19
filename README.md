rlang <img src="man/figures/logo.png" align="right" />
=======================================================

<!-- badges: start -->
[![R-CMD-check](https://github.com/r-lib/rlang/workflows/R-CMD-check/badge.svg)](https://github.com/r-lib/rlang/actions)
[![Codecov test coverage](https://codecov.io/gh/r-lib/rlang/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-lib/rlang?branch=main)
[![Lifecycle Status](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->


rlang is a collection of frameworks and APIs for programming with R.


## Frameworks

Two comprehensive frameworks are implemented in rlang.

*   __tidy eval__, a programmable [data-masking](https://rlang.r-lib.org/reference/topic-data-mask.html) framework used in tidyverse packages like dplyr and ggplot2. As a user, you will encounter the embracing operator [`{{`](https://rlang.r-lib.org/reference/embrace-operator.html) and name injection with the [glue](https://glue.tidyverse.org/) operators [`"{"`](https://rlang.r-lib.org/reference/glue-operators.html) and [`"{{"`](https://rlang.r-lib.org/reference/glue-operators.html).

*   __rlang errors__, a set of tools to signal and display errors. This includes backtrace capture with `global_entrace()` and backtrace display with `last_error()` and `last_warnings()`. Use `abort()` to create errors with bullet lists, structured metadata, and error chaining support.

    The display of error messages is optimised for bullet lists and chained errors and optionally integrates with the cli package (see `local_use_cli()`).


## Argument intake

A set of tools help you check, validate, and preprocess arguments.

*   Checking function arguments, e.g. `arg_match()`, `check_required()`, and `check_exclusive()`.

*   Checking dots, e.g. `check_dots_used()` and `check_dots_empty()`.

*   Collecting [dynamic dots](https://rlang.r-lib.org/reference/dyn-dots.html), e.g. `list2()`. These dots support splicing with [`!!!`](https://rlang.r-lib.org/reference/splice-operator.html) and name injection with the [glue](https://glue.tidyverse.org/) operators [`"{"`](https://rlang.r-lib.org/reference/glue-operators.html) and [`"{{"`](https://rlang.r-lib.org/reference/glue-operators.html).


## Programming interfaces

rlang provides various interfaces for working with R and R objects.

*   The R session, e.g. `check_installed()`, `on_load()`, and `on_package_load()`.

*   Environments, e.g. `env()`, `env_has()`, `env_get()`, `env_bind()`, `env_unbind()`, `env_print()`, and `local_bindings()`.

*   Evaluation, e.g. `inject()` and `eval_bare()`.

*   Calls and symbols, e.g. `call2()`, `is_call()`, `is_call_simple()`, `data_sym()`, and `data_syms()`.

*   Functions, e.g. `new_function()` and `as_function()`. The latter supports the purrr-style formula notation for lambda functions.


## Installation

Install the released version of rlang from CRAN:

```r
install.packages("rlang")
```

Or install the development version from GitHub with:

```r
# install.packages("pak")
pak::pkg_install("r-lib/rlang")
```


## Code of Conduct

Please note that the rlang project is released with a [Contributor Code of Conduct](https://rlang.r-lib.org/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
