# Register default global handlers

`global_handle()` sets up a default configuration for error, warning,
and message handling. It calls:

- [`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md)
  to enable rlang errors and warnings globally.

- [`global_prompt_install()`](https://rlang.r-lib.org/dev/reference/global_prompt_install.md)
  to recover from `packageNotFoundError`s with a user prompt to install
  the missing package. Note that at the time of writing (R 4.1), there
  are only very limited situations where this handler works.

## Usage

``` r
global_handle(entrace = TRUE, prompt_install = TRUE)
```

## Arguments

- entrace:

  Passed as `enable` argument to
  [`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md).

- prompt_install:

  Passed as `enable` argument to
  [`global_prompt_install()`](https://rlang.r-lib.org/dev/reference/global_prompt_install.md).
