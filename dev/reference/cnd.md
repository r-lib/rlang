# Create a condition object

These constructors create subclassed conditions, the objects that power
the error, warning, and message system in R.

- `cnd()` creates bare conditions that only inherit from `condition`.

- Conditions created with `error_cnd()`, `warning_cnd()`, and
  `message_cnd()` inherit from `"error"`, `"warning"`, or `"message"`.

- `error_cnd()` creates subclassed errors. See
  [`"rlang_error"`](https://rlang.r-lib.org/dev/reference/rlang_error.md).

Use
[`cnd_signal()`](https://rlang.r-lib.org/dev/reference/cnd_signal.md) to
emit the relevant signal for a particular condition class.

## Usage

``` r
cnd(class, ..., message = "", call = NULL, use_cli_format = NULL)

error_cnd(
  class = NULL,
  ...,
  message = "",
  call = NULL,
  trace = NULL,
  parent = NULL,
  use_cli_format = NULL
)

warning_cnd(
  class = NULL,
  ...,
  message = "",
  call = NULL,
  use_cli_format = NULL
)

message_cnd(
  class = NULL,
  ...,
  message = "",
  call = NULL,
  use_cli_format = NULL
)
```

## Arguments

- class:

  The condition subclass.

- ...:

  \<[dynamic](https://rlang.r-lib.org/dev/reference/dyn-dots.md)\> Named
  data fields stored inside the condition object.

- message:

  A default message to inform the user about the condition when it is
  signalled.

- call:

  A function call to be included in the error message. If an execution
  environment of a running function, the corresponding function call is
  retrieved.

- use_cli_format:

  Whether to use the cli package to format `message`. See
  [`local_use_cli()`](https://rlang.r-lib.org/dev/reference/local_use_cli.md).

- trace:

  A `trace` object created by
  [`trace_back()`](https://rlang.r-lib.org/dev/reference/trace_back.md).

- parent:

  A parent condition object.

## See also

[`cnd_signal()`](https://rlang.r-lib.org/dev/reference/cnd_signal.md),
[`try_fetch()`](https://rlang.r-lib.org/dev/reference/try_fetch.md).

## Examples

``` r
# Create a condition inheriting only from the S3 class "foo":
cnd <- cnd("foo")

# Signal the condition to potential handlers. Since this is a bare
# condition the signal has no effect if no handlers are set up:
cnd_signal(cnd)

# When a relevant handler is set up, the signal transfers control
# to the handler
with_handlers(cnd_signal(cnd), foo = function(c) "caught!")
#> Warning: `with_handlers()` is deprecated as of rlang 1.0.0.
#> ℹ Please use `tryCatch()`, `withCallingHandlers()`, or `try_fetch()`.
#> This warning is displayed once every 8 hours.
#> [1] "caught!"
tryCatch(cnd_signal(cnd), foo = function(c) "caught!")
#> [1] "caught!"
```
