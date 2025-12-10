# Signal a condition object

`cnd_signal()` takes a condition as argument and emits the corresponding
signal. The type of signal depends on the class of the condition:

- A message is signalled if the condition inherits from `"message"`.
  This is equivalent to signalling with
  [`inform()`](https://rlang.r-lib.org/reference/abort.md) or
  [`base::message()`](https://rdrr.io/r/base/message.html).

- A warning is signalled if the condition inherits from `"warning"`.
  This is equivalent to signalling with
  [`warn()`](https://rlang.r-lib.org/reference/abort.md) or
  [`base::warning()`](https://rdrr.io/r/base/warning.html).

- An error is signalled if the condition inherits from `"error"`. This
  is equivalent to signalling with
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) or
  [`base::stop()`](https://rdrr.io/r/base/stop.html).

- An interrupt is signalled if the condition inherits from
  `"interrupt"`. This is equivalent to signalling with
  [`interrupt()`](https://rlang.r-lib.org/reference/interrupt.md).

## Usage

``` r
cnd_signal(cnd, ...)
```

## Arguments

- cnd:

  A condition object (see
  [`cnd()`](https://rlang.r-lib.org/reference/cnd.md)). If `NULL`,
  `cnd_signal()` returns without signalling a condition.

- ...:

  These dots are for future extensions and must be empty.

## See also

- [`cnd_type()`](https://rlang.r-lib.org/reference/cnd_type.md) to
  determine the type of a condition.

- [`abort()`](https://rlang.r-lib.org/reference/abort.md),
  [`warn()`](https://rlang.r-lib.org/reference/abort.md) and
  [`inform()`](https://rlang.r-lib.org/reference/abort.md) for creating
  and signalling structured R conditions in one go.

- [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md) for
  establishing condition handlers for particular condition classes.

## Examples

``` r
# The type of signal depends on the class. If the condition
# inherits from "warning", a warning is issued:
cnd <- warning_cnd("my_warning_class", message = "This is a warning")
cnd_signal(cnd)
#> Warning: This is a warning

# If it inherits from "error", an error is raised:
cnd <- error_cnd("my_error_class", message = "This is an error")
try(cnd_signal(cnd))
#> Error : This is an error
```
