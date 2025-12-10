# Last `abort()` error

- `last_error()` returns the last error entraced by
  [`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) or
  [`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md).
  The error is printed with a backtrace in simplified form.

- `last_trace()` is a shortcut to return the backtrace stored in the
  last error. This backtrace is printed in full form.

## Usage

``` r
last_error()

last_trace(drop = NULL)
```

## Arguments

- drop:

  Whether to drop technical calls. These are hidden from users by
  default, set `drop` to `FALSE` to see the full backtrace.

## See also

- [`rlang_backtrace_on_error`](https://rlang.r-lib.org/dev/reference/rlang_backtrace_on_error.md)
  to control what is displayed when an error is thrown.

- [`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md)
  to enable `last_error()` logging for all errors.

- [`last_warnings()`](https://rlang.r-lib.org/dev/reference/last_warnings.md)
  and
  [`last_messages()`](https://rlang.r-lib.org/dev/reference/last_warnings.md).
