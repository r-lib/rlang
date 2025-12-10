# Entrace unexpected errors

`global_entrace()` enriches base errors, warnings, and messages with
rlang features.

- They are assigned a backtrace. You can configure whether to display a
  backtrace on error with the
  [rlang_backtrace_on_error](https://rlang.r-lib.org/reference/rlang_backtrace_on_error.md)
  global option.

- They are recorded in
  [`last_error()`](https://rlang.r-lib.org/reference/last_error.md),
  [`last_warnings()`](https://rlang.r-lib.org/reference/last_warnings.md),
  or
  [`last_messages()`](https://rlang.r-lib.org/reference/last_warnings.md).
  You can inspect backtraces at any time by calling these functions.

Set global entracing in your RProfile with:

    rlang::global_entrace()

## Usage

``` r
global_entrace(enable = TRUE, class = c("error", "warning", "message"))
```

## Arguments

- enable:

  Whether to enable or disable global handling.

- class:

  A character vector of one or several classes of conditions to be
  entraced.

## Inside RMarkdown documents

Call `global_entrace()` inside an RMarkdown document to cause errors and
warnings to be promoted to rlang conditions that include a backtrace.
This needs to be done in a separate setup chunk before the first error
or warning.

This is useful in conjunction with
[`rlang_backtrace_on_error_report`](https://rlang.r-lib.org/reference/rlang_backtrace_on_error.md)
and
[`rlang_backtrace_on_warning_report`](https://rlang.r-lib.org/reference/rlang_backtrace_on_error.md).
To get full entracing in an Rmd document, include this in a setup chunk
before the first error or warning is signalled.

    ```{r setup}
    rlang::global_entrace()
    options(rlang_backtrace_on_warning_report = "full")
    options(rlang_backtrace_on_error_report = "full")
    ```

## Under the hood

On R 4.0 and newer, `global_entrace()` installs a global handler with
[`globalCallingHandlers()`](https://rdrr.io/r/base/conditions.html). On
older R versions,
[`entrace()`](https://rlang.r-lib.org/reference/entrace.md) is set as an
`option(error = )` handler. The latter method has the disadvantage that
only one handler can be set at a time. This means that you need to
manually switch between
[`entrace()`](https://rlang.r-lib.org/reference/entrace.md) and other
handlers like [`recover()`](https://rdrr.io/r/utils/recover.html). Also
this causes a conflict with IDE handlers (e.g. in RStudio).
