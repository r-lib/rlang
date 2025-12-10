# Deprecated `scoped_` functions

**\[deprecated\]**

Deprecated as of rlang 0.4.2. Use
[`local_interactive()`](https://rlang.r-lib.org/dev/reference/is_interactive.md),
[`local_options()`](https://rlang.r-lib.org/dev/reference/local_options.md),
or
[`local_bindings()`](https://rlang.r-lib.org/dev/reference/local_bindings.md)
instead.

## Usage

``` r
scoped_interactive(value = TRUE, frame = caller_env())

scoped_options(..., .frame = caller_env())

scoped_bindings(..., .env = .frame, .frame = caller_env())
```

## Arguments

- value:

  A single `TRUE` or `FALSE`. This overrides the return value of
  [`is_interactive()`](https://rlang.r-lib.org/dev/reference/is_interactive.md).

- frame, .frame:

  The environment of a running function which defines the scope of the
  temporary options. When the function returns, the options are reset to
  their original values.

- ...:

  For
  [`local_options()`](https://rlang.r-lib.org/dev/reference/local_options.md)
  and
  [`push_options()`](https://rlang.r-lib.org/dev/reference/local_options.md),
  named values defining new option values. For
  [`peek_options()`](https://rlang.r-lib.org/dev/reference/local_options.md),
  strings or character vectors of option names.

- .env:

  An environment.
