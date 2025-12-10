# Is R running interactively?

Like [`base::interactive()`](https://rdrr.io/r/base/interactive.html),
`is_interactive()` returns `TRUE` when the function runs interactively
and `FALSE` when it runs in batch mode. It also checks, in this order:

- The `rlang_interactive` global option. If set to a single `TRUE` or
  `FALSE`, `is_interactive()` returns that value immediately. This
  escape hatch is useful in unit tests or to manually turn on
  interactive features in RMarkdown outputs.

- Whether knitr or testthat is in progress, in which case
  `is_interactive()` returns `FALSE`.

`with_interactive()` and `local_interactive()` set the global option
conveniently.

## Usage

``` r
is_interactive()

local_interactive(value = TRUE, frame = caller_env())

with_interactive(expr, value = TRUE)
```

## Arguments

- value:

  A single `TRUE` or `FALSE`. This overrides the return value of
  `is_interactive()`.

- frame:

  The environment of a running function which defines the scope of the
  temporary options. When the function returns, the options are reset to
  their original values.

- expr:

  An expression to evaluate with interactivity set to `value`.
