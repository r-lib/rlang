# Signal an error, warning, or message

These functions are equivalent to base functions
[`base::stop()`](https://rdrr.io/r/base/stop.html),
[`base::warning()`](https://rdrr.io/r/base/warning.html), and
[`base::message()`](https://rdrr.io/r/base/message.html). They signal a
condition (an error, warning, or message respectively) and make it easy
to supply condition metadata:

- Supply `class` to create a classed condition that can be caught or
  handled selectively, allowing for finer-grained error handling.

- Supply metadata with named `...` arguments. This data is stored in the
  condition object and can be examined by handlers.

- Supply `call` to inform users about which function the error occurred
  in.

- Supply another condition as `parent` to create a [chained
  condition](https://rlang.r-lib.org/reference/topic-error-chaining.md).

Certain components of condition messages are formatted with unicode
symbols and terminal colours by default. These aspects can be
customised, see [Customising condition
messages](https://rlang.r-lib.org/reference/topic-condition-customisation.md).

## Usage

``` r
abort(
  message = NULL,
  class = NULL,
  ...,
  call,
  body = NULL,
  footer = NULL,
  trace = NULL,
  parent = NULL,
  use_cli_format = NULL,
  .inherit = TRUE,
  .internal = FALSE,
  .file = NULL,
  .frame = caller_env(),
  .trace_bottom = NULL,
  .subclass = deprecated()
)

warn(
  message = NULL,
  class = NULL,
  ...,
  body = NULL,
  footer = NULL,
  parent = NULL,
  use_cli_format = NULL,
  .inherit = NULL,
  .frequency = c("always", "regularly", "once"),
  .frequency_id = NULL,
  .subclass = deprecated()
)

inform(
  message = NULL,
  class = NULL,
  ...,
  body = NULL,
  footer = NULL,
  parent = NULL,
  use_cli_format = NULL,
  .inherit = NULL,
  .file = NULL,
  .frequency = c("always", "regularly", "once"),
  .frequency_id = NULL,
  .subclass = deprecated()
)

signal(message = "", class, ..., .subclass = deprecated())

reset_warning_verbosity(id)

reset_message_verbosity(id)
```

## Arguments

- message:

  The message to display, formatted as a **bulleted list**. The first
  element is displayed as an *alert* bullet prefixed with `!` by
  default. Elements named `"*"`, `"i"`, `"v"`, `"x"`, and `"!"` are
  formatted as regular, info, success, failure, and error bullets
  respectively. See [Formatting messages with
  cli](https://rlang.r-lib.org/reference/topic-condition-formatting.md)
  for more about bulleted messaging.

  If a message is not supplied, it is expected that the message is
  generated **lazily** through
  [`cnd_header()`](https://rlang.r-lib.org/reference/cnd_message.md) and
  [`cnd_body()`](https://rlang.r-lib.org/reference/cnd_message.md)
  methods. In that case, `class` must be supplied. Only `inform()`
  allows empty messages as it is occasionally useful to build user
  output incrementally.

  If a function, it is stored in the `header` field of the error
  condition. This acts as a
  [`cnd_header()`](https://rlang.r-lib.org/reference/cnd_message.md)
  method that is invoked lazily when the error message is displayed.

- class:

  Subclass of the condition.

- ...:

  Additional data to be stored in the condition object. If you supply
  condition fields, you should usually provide a `class` argument. You
  may consider prefixing condition fields with the name of your package
  or organisation to prevent name collisions.

- call:

  The execution environment of a currently running function, e.g.
  `call = caller_env()`. The corresponding function call is retrieved
  and mentioned in error messages as the source of the error.

  You only need to supply `call` when throwing a condition from a helper
  function which wouldn't be relevant to mention in the message.

  Can also be `NULL` or a [defused function
  call](https://rlang.r-lib.org/reference/topic-defuse.md) to
  respectively not display any call or hard-code a code to display.

  For more information about error calls, see [Including function calls
  in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.md).

- body, footer:

  Additional bullets.

- trace:

  A `trace` object created by
  [`trace_back()`](https://rlang.r-lib.org/reference/trace_back.md).

- parent:

  Supply `parent` when you rethrow an error from a condition handler
  (e.g. with
  [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md)).

  - If `parent` is a condition object, a *chained error* is created,
    which is useful when you want to enhance an error with more details,
    while still retaining the original information.

  - If `parent` is `NA`, it indicates an unchained rethrow, which is
    useful when you want to take ownership over an error and rethrow it
    with a custom message that better fits the surrounding context.

    Technically, supplying `NA` lets `abort()` know it is called from a
    condition handler. This helps it create simpler backtraces where the
    condition handling context is hidden by default.

  For more information about error calls, see [Including contextual
  information with error
  chains](https://rlang.r-lib.org/reference/topic-error-chaining.md).

- use_cli_format:

  Whether to format `message` lazily using [cli](https://cli.r-lib.org/)
  if available. This results in prettier and more accurate formatting of
  messages. See
  [`local_use_cli()`](https://rlang.r-lib.org/reference/local_use_cli.md)
  to set this condition field by default in your package namespace.

  If set to `TRUE`, `message` should be a character vector of individual
  and unformatted lines. Any newline character `"\\n"` already present
  in `message` is reformatted by cli's paragraph formatter. See
  [Formatting messages with
  cli](https://rlang.r-lib.org/reference/topic-condition-formatting.md).

- .inherit:

  Whether the condition inherits from `parent` according to
  [`cnd_inherits()`](https://rlang.r-lib.org/reference/cnd_inherits.md)
  and [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md).
  By default, parent conditions of higher severity are not inherited.
  For instance an error chained to a warning is not inherited to avoid
  unexpectedly catching an error downgraded to a warning.

- .internal:

  If `TRUE`, a footer bullet is added to `message` to let the user know
  that the error is internal and that they should report it to the
  package authors. This argument is incompatible with `footer`.

- .file:

  A connection or a string specifying where to print the message. The
  default depends on the context, see the `stdout` vs `stderr` section.

- .frame:

  The throwing context. Used as default for `.trace_bottom`, and to
  determine the internal package to mention in internal errors when
  `.internal` is `TRUE`.

- .trace_bottom:

  Used in the display of simplified backtraces as the last relevant call
  frame to show. This way, the irrelevant parts of backtraces
  corresponding to condition handling
  ([`tryCatch()`](https://rdrr.io/r/base/conditions.html),
  [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md),
  `abort()`, etc.) are hidden by default. Defaults to `call` if it is an
  environment, or `.frame` otherwise. Without effect if `trace` is
  supplied.

- .subclass:

  **\[deprecated\]** This argument was renamed to `class` in rlang 0.4.2
  for consistency with our conventions for class constructors documented
  in <https://adv-r.hadley.nz/s3.html#s3-subclassing>.

- .frequency:

  How frequently should the warning or message be displayed? By default
  (`"always"`) it is displayed at each time. If `"regularly"`, it is
  displayed once every 8 hours. If `"once"`, it is displayed once per
  session.

- .frequency_id:

  A unique identifier for the warning or message. This is used when
  `.frequency` is supplied to recognise recurring conditions. This
  argument must be supplied if `.frequency` is not set to `"always"`.

- id:

  The identifying string of the condition that was supplied as
  `.frequency_id` to `warn()` or `inform()`.

## Details

- `abort()` throws subclassed errors, see
  [`"rlang_error"`](https://rlang.r-lib.org/reference/rlang_error.md).

- `warn()` temporarily set the `warning.length` global option to the
  maximum value (8170), unless that option has been changed from the
  default value. The default limit (1000 characters) is especially easy
  to hit when the message contains a lot of ANSI escapes, as created by
  the crayon or cli packages

## Error prefix

As with [`base::stop()`](https://rdrr.io/r/base/stop.html), errors
thrown with `abort()` are prefixed with `"Error: "`. Calls and source
references are included in the prefix, e.g.
`"Error in `my_function()` at myfile.R:1:2:"`. There are a few cosmetic
differences:

- The call is stripped from its arguments to keep it simple. It is then
  formatted using the [cli package](https://cli.r-lib.org/) if
  available.

- A line break between the prefix and the message when the former is too
  long. When a source location is included, a line break is always
  inserted.

If your throwing code is highly structured, you may have to explicitly
inform `abort()` about the relevant user-facing call to include in the
prefix. Internal helpers are rarely relevant to end users. See the
`call` argument of `abort()`.

## Backtrace

`abort()` saves a backtrace in the `trace` component of the error
condition. You can print a simplified backtrace of the last error by
calling
[`last_error()`](https://rlang.r-lib.org/reference/last_error.md) and a
full backtrace with `summary(last_error())`. Learn how to control what
is displayed when an error is thrown with
[`rlang_backtrace_on_error`](https://rlang.r-lib.org/reference/rlang_backtrace_on_error.md).

## Muffling and silencing conditions

Signalling a condition with `inform()` or `warn()` displays a message in
the console. These messages can be muffled as usual with
[`base::suppressMessages()`](https://rdrr.io/r/base/message.html) or
[`base::suppressWarnings()`](https://rdrr.io/r/base/warning.html).

`inform()` and `warn()` messages can also be silenced with the global
options `rlib_message_verbosity` and `rlib_warning_verbosity`. These
options take the values:

- `"default"`: Verbose unless the `.frequency` argument is supplied.

- `"verbose"`: Always verbose.

- `"quiet"`: Always quiet.

When set to quiet, the message is not displayed and the condition is not
signalled.

## `stdout` and `stderr`

By default, `abort()` and `inform()` print to standard output in
interactive sessions. This allows rlang to be in control of the
appearance of messages in IDEs like RStudio.

There are two situations where messages are streamed to `stderr`:

- In non-interactive sessions, messages are streamed to standard error
  so that R scripts can easily filter them out from normal output by
  redirecting `stderr`.

- If a sink is active (either on output or on messages) messages are
  always streamed to `stderr`.

These exceptions ensure consistency of behaviour in interactive and
non-interactive sessions, and when sinks are active.

## See also

- [Including function calls in error
  messages](https://rlang.r-lib.org/reference/topic-error-call.md)

- [Including contextual information with error
  chains](https://rlang.r-lib.org/reference/topic-error-chaining.md)

## Examples

``` r
# These examples are guarded to avoid throwing errors
if (FALSE) {

# Signal an error with a message just like stop():
abort("The error message.")


# Unhandled errors are saved automatically by `abort()` and can be
# retrieved with `last_error()`. The error prints with a simplified
# backtrace:
f <- function() try(g())
g <- function() evalq(h())
h <- function() abort("Tilt.")
last_error()

# Use `summary()` to print the full backtrace and the condition fields:
summary(last_error())


# Give a class to the error:
abort("The error message", "mypkg_bad_error")

# This allows callers to handle the error selectively
tryCatch(
  mypkg_function(),
  mypkg_bad_error = function(err) {
    warn(conditionMessage(err)) # Demote the error to a warning
    NA                          # Return an alternative value
  }
)

# You can also specify metadata that will be stored in the condition:
abort("The error message.", "mypkg_bad_error", data = 1:10)

# This data can then be consulted by user handlers:
tryCatch(
  mypkg_function(),
  mypkg_bad_error = function(err) {
    # Compute an alternative return value with the data:
    recover_error(err$data)
  }
)


# If you call low-level APIs it may be a good idea to create a
# chained error with the low-level error wrapped in a more
# user-friendly error. Use `try_fetch()` to fetch errors of a given
# class and rethrow them with the `parent` argument of `abort()`:
file <- "http://foo.bar/baz"
try(
  try_fetch(
    download(file),
    error = function(err) {
      msg <- sprintf("Can't download `%s`", file)
      abort(msg, parent = err)
  })
)

# You can also hard-code the call when it's not easy to
# forward it from the caller
 f <- function() {
  abort("my message", call = call("my_function"))
}
g <- function() {
  f()
}
# Shows that the error occurred in `my_function()`
try(g())

}
```
