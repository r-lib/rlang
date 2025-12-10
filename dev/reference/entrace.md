# Add backtrace from error handler

`entrace()` is a low level function. See
[`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md)
for a user-friendly way of enriching errors and other conditions from
your RProfile.

- `entrace()` is meant to be used as a global handler. It enriches
  conditions with a backtrace. Errors are saved to
  [`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md)
  and rethrown immediately. Messages and warnings are recorded into
  [`last_messages()`](https://rlang.r-lib.org/dev/reference/last_warnings.md)
  and
  [`last_warnings()`](https://rlang.r-lib.org/dev/reference/last_warnings.md)
  and let through.

- `cnd_entrace()` adds a backtrace to a condition object, without any
  other effect. It should be called from a condition handler.

`entrace()` also works as an `option(error = )` handler for
compatibility with versions of R older than 4.0.

When used as calling handler, rlang trims the handler invocation context
from the backtrace.

## Usage

``` r
entrace(cnd, ..., top = NULL, bottom = NULL)

cnd_entrace(cnd, ..., top = NULL, bottom = NULL)
```

## Arguments

- cnd:

  When `entrace()` is used as a calling handler, `cnd` is the condition
  to handle.

- ...:

  Unused. These dots are for future extensions.

- top:

  The first frame environment to be included in the backtrace. This
  becomes the top of the backtrace tree and represents the oldest call
  in the backtrace.

  This is needed in particular when you call
  [`trace_back()`](https://rlang.r-lib.org/dev/reference/trace_back.md)
  indirectly or from a larger context, for example in tests or inside an
  RMarkdown document where you don't want all of the knitr evaluation
  mechanisms to appear in the backtrace.

  If not supplied, the `rlang_trace_top_env` global option is consulted.
  This makes it possible to trim the embedding context for all
  backtraces created while the option is set. If knitr is in progress,
  the default value for this option is
  [`knitr::knit_global()`](https://rdrr.io/pkg/knitr/man/knit_global.html)
  so that the knitr context is trimmed out of backtraces.

- bottom:

  The last frame environment to be included in the backtrace. This
  becomes the rightmost leaf of the backtrace tree and represents the
  youngest call in the backtrace.

  Set this when you would like to capture a backtrace without the
  capture context.

  Can also be an integer that will be passed to
  [`caller_env()`](https://rlang.r-lib.org/dev/reference/stack.md).

## See also

[`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md)
for configuring errors with `entrace()`. `cnd_entrace()` to manually add
a backtrace to a condition.

## Examples

``` r
quote({  # Not run

# Set `entrace()` globally in your RProfile
globalCallingHandlers(error = rlang::entrace)

# On older R versions which don't feature `globalCallingHandlers`,
# set the error handler like this:
options(error = rlang::entrace)

})
#> {
#>     globalCallingHandlers(error = rlang::entrace)
#>     options(error = rlang::entrace)
#> }
```
