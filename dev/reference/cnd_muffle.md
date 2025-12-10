# Muffle a condition

Unlike
[`exiting()`](https://rlang.r-lib.org/dev/reference/with_handlers.md)
handlers,
[`calling()`](https://rlang.r-lib.org/dev/reference/with_handlers.md)
handlers must be explicit that they have handled a condition to stop it
from propagating to other handlers. Use `cnd_muffle()` within a calling
handler (or as a calling handler, see examples) to prevent any other
handlers from being called for that condition.

## Usage

``` r
cnd_muffle(cnd)
```

## Arguments

- cnd:

  A condition to muffle.

## Value

If `cnd` is mufflable, `cnd_muffle()` jumps to the muffle restart and
doesn't return. Otherwise, it returns `FALSE`.

## Mufflable conditions

Most conditions signalled by base R are mufflable, although the name of
the restart varies. cnd_muffle() will automatically call the correct
restart for you. It is compatible with the following conditions:

- `warning` and `message` conditions. In this case `cnd_muffle()` is
  equivalent to
  [`base::suppressMessages()`](https://rdrr.io/r/base/message.html) and
  [`base::suppressWarnings()`](https://rdrr.io/r/base/warning.html).

- Bare conditions signalled with
  [`signal()`](https://rlang.r-lib.org/dev/reference/abort.md) or
  [`cnd_signal()`](https://rlang.r-lib.org/dev/reference/cnd_signal.md).
  Note that conditions signalled with
  [`base::signalCondition()`](https://rdrr.io/r/base/conditions.html)
  are not mufflable.

- Interrupts are sometimes signalled with a `resume` restart on recent R
  versions. When this is the case, you can muffle the interrupt with
  `cnd_muffle()`. Check if a restart is available with
  `base::findRestart("resume")`.

If you call `cnd_muffle()` with a condition that is not mufflable you
will cause a new error to be signalled.

- Errors are not mufflable since they are signalled in critical
  situations where execution cannot continue safely.

- Conditions captured with
  [`base::tryCatch()`](https://rdrr.io/r/base/conditions.html),
  [`with_handlers()`](https://rlang.r-lib.org/dev/reference/with_handlers.md)
  or [`catch_cnd()`](https://rlang.r-lib.org/dev/reference/catch_cnd.md)
  are no longer mufflable. Muffling restarts *must* be called from a
  [calling](https://rlang.r-lib.org/dev/reference/with_handlers.md)
  handler.

## Examples

``` r
fn <- function() {
  inform("Beware!", "my_particular_msg")
  inform("On your guard!")
  "foobar"
}

# Let's install a muffling handler for the condition thrown by `fn()`.
# This will suppress all `my_particular_wng` warnings but let other
# types of warnings go through:
with_handlers(fn(),
  my_particular_msg = calling(function(cnd) {
    inform("Dealt with this particular message")
    cnd_muffle(cnd)
  })
)
#> Warning: `calling()` is deprecated as of rlang 1.0.0.
#> This warning is displayed once every 8 hours.
#> Dealt with this particular message
#> On your guard!
#> [1] "foobar"

# Note how execution of `fn()` continued normally after dealing
# with that particular message.

# cnd_muffle() can also be passed to with_handlers() as a calling
# handler:
with_handlers(fn(),
  my_particular_msg = calling(cnd_muffle)
)
#> On your guard!
#> [1] "foobar"
```
