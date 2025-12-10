# rlang

rlang is a collection of frameworks and APIs for programming with R.

## Frameworks

Two comprehensive frameworks are implemented in rlang.

- **tidy eval**, a programmable
  [data-masking](https://rlang.r-lib.org/reference/topic-data-mask.html)
  framework used in tidyverse packages like dplyr and ggplot2. As a
  user, you will encounter the embracing operator
  [`{{`](https://rlang.r-lib.org/reference/embrace-operator.html) and
  name injection with the [glue](https://glue.tidyverse.org/) operators
  [`"{"`](https://rlang.r-lib.org/reference/glue-operators.html) and
  [`"{{"`](https://rlang.r-lib.org/reference/glue-operators.html).

- **rlang errors**, a set of tools to signal and display errors. This
  includes backtrace capture with
  [`global_entrace()`](https://rlang.r-lib.org/reference/global_entrace.md)
  and backtrace display with
  [`last_error()`](https://rlang.r-lib.org/reference/last_error.md) and
  [`last_warnings()`](https://rlang.r-lib.org/reference/last_warnings.md).
  Use [`abort()`](https://rlang.r-lib.org/reference/abort.md) to create
  errors with bullet lists, structured metadata, and error chaining
  support.

  The display of error messages is optimised for bullet lists and
  chained errors and optionally integrates with the cli package (see
  [`local_use_cli()`](https://rlang.r-lib.org/reference/local_use_cli.md)).

## Argument intake

A set of tools help you check, validate, and preprocess arguments.

- Checking function arguments,
  e.g. [`arg_match()`](https://rlang.r-lib.org/reference/arg_match.md),
  [`check_required()`](https://rlang.r-lib.org/reference/check_required.md),
  and
  [`check_exclusive()`](https://rlang.r-lib.org/reference/check_exclusive.md).

- Checking dots,
  e.g. [`check_dots_used()`](https://rlang.r-lib.org/reference/check_dots_used.md)
  and
  [`check_dots_empty()`](https://rlang.r-lib.org/reference/check_dots_empty.md).

- Collecting [dynamic
  dots](https://rlang.r-lib.org/reference/dyn-dots.html),
  e.g. [`list2()`](https://rlang.r-lib.org/reference/list2.md). These
  dots support splicing with
  [`!!!`](https://rlang.r-lib.org/reference/splice-operator.html) and
  name injection with the [glue](https://glue.tidyverse.org/) operators
  [`"{"`](https://rlang.r-lib.org/reference/glue-operators.html) and
  [`"{{"`](https://rlang.r-lib.org/reference/glue-operators.html).

## Programming interfaces

rlang provides various interfaces for working with R and R objects.

- The R session,
  e.g. [`check_installed()`](https://rlang.r-lib.org/reference/is_installed.md),
  [`on_load()`](https://rlang.r-lib.org/reference/on_load.md), and
  [`on_package_load()`](https://rlang.r-lib.org/reference/on_load.md).

- Environments,
  e.g. [`env()`](https://rlang.r-lib.org/reference/env.md),
  [`env_has()`](https://rlang.r-lib.org/reference/env_has.md),
  [`env_get()`](https://rlang.r-lib.org/reference/env_get.md),
  [`env_bind()`](https://rlang.r-lib.org/reference/env_bind.md),
  [`env_unbind()`](https://rlang.r-lib.org/reference/env_unbind.md),
  [`env_print()`](https://rlang.r-lib.org/reference/env_print.md), and
  [`local_bindings()`](https://rlang.r-lib.org/reference/local_bindings.md).

- Evaluation,
  e.g. [`inject()`](https://rlang.r-lib.org/reference/inject.md) and
  [`eval_bare()`](https://rlang.r-lib.org/reference/eval_bare.md).

- Calls and symbols,
  e.g. [`call2()`](https://rlang.r-lib.org/reference/call2.md),
  [`is_call()`](https://rlang.r-lib.org/reference/is_call.md),
  [`is_call_simple()`](https://rlang.r-lib.org/reference/call_name.md),
  [`data_sym()`](https://rlang.r-lib.org/reference/sym.md), and
  [`data_syms()`](https://rlang.r-lib.org/reference/sym.md).

- Functions,
  e.g. [`new_function()`](https://rlang.r-lib.org/reference/new_function.md)
  and
  [`as_function()`](https://rlang.r-lib.org/reference/as_function.md).
  The latter supports the purrr-style formula notation for lambda
  functions.

## Installation

Install the released version of rlang from CRAN:

``` r
install.packages("rlang")
```

Or install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pkg_install("r-lib/rlang")
```

## Code of Conduct

Please note that the rlang project is released with a [Contributor Code
of Conduct](https://rlang.r-lib.org/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
