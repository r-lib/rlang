# Prompt user to install missing packages

When enabled, `packageNotFoundError` thrown by
[`loadNamespace()`](https://rdrr.io/r/base/ns-load.html) cause a user
prompt to install the missing package and continue without interrupting
the current program.

This is similar to how
[`check_installed()`](https://rlang.r-lib.org/reference/is_installed.md)
prompts users to install required packages. It uses the same install
strategy, using pak if available and
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
otherwise.

## Usage

``` r
global_prompt_install(enable = TRUE)
```

## Arguments

- enable:

  Whether to enable or disable global handling.
