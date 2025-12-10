# Display backtrace on error

rlang errors carry a backtrace that can be inspected by calling
[`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md).
You can also control the default display of the backtrace by setting the
option `rlang_backtrace_on_error` to one of the following values:

- `"none"` show nothing.

- `"reminder"`, the default in interactive sessions, displays a reminder
  that you can see the backtrace with
  [`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md).

- `"branch"` displays a simplified backtrace.

- `"full"`, the default in non-interactive sessions, displays the full
  tree.

rlang errors are normally thrown with
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md). If you
promote base errors to rlang errors with
[`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md),
`rlang_backtrace_on_error` applies to all errors.

## Promote base errors to rlang errors

You can use `options(error = rlang::entrace)` to promote base errors to
rlang errors. This does two things:

- It saves the base error as an rlang object so you can call
  [`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md)
  to print the backtrace or inspect its data.

- It prints the backtrace for the current error according to the
  `rlang_backtrace_on_error` option.

## Warnings and errors in RMarkdown

The display of errors depends on whether they're expected (i.e. chunk
option `error = TRUE`) or unexpected:

- Expected errors are controlled by the global option
  `"rlang_backtrace_on_error_report"` (note the `_report` suffix). The
  default is `"none"` so that your expected errors don't include a
  reminder to run
  [`rlang::last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md).
  Customise this option if you want to demonstrate what the error
  backtrace will look like.

  You can also use
  [`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md)
  to display the trace like you would in your session, but it currently
  only works in the next chunk.

- Unexpected errors are controlled by the global option
  `"rlang_backtrace_on_error"`. The default is `"branch"` so you'll see
  a simplified backtrace in the knitr output to help you figure out what
  went wrong.

When knitr is running (as determined by the `knitr.in.progress` global
option), the default top environment for backtraces is set to the chunk
environment
[`knitr::knit_global()`](https://rdrr.io/pkg/knitr/man/knit_global.html).
This ensures that the part of the call stack belonging to knitr does not
end up in backtraces. If needed, you can override this by setting the
`rlang_trace_top_env` global option.

Similarly to `rlang_backtrace_on_error_report`, you can set
`rlang_backtrace_on_warning_report` inside RMarkdown documents to tweak
the display of warnings. This is useful in conjunction with
[`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md).
Because of technical limitations, there is currently no corresponding
`rlang_backtrace_on_warning` option for normal R sessions.

To get full entracing in an Rmd document, include this in a setup chunk
before the first error or warning is signalled.

    ```{r setup}
    rlang::global_entrace()
    options(rlang_backtrace_on_warning_report = "full")
    options(rlang_backtrace_on_error_report = "full")
    ```

## See also

rlang_backtrace_on_warning

## Examples

``` r
# Display a simplified backtrace on error for both base and rlang
# errors:

# options(
#   rlang_backtrace_on_error = "branch",
#   error = rlang::entrace
# )
# stop("foo")
```
