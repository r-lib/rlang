# Formatting messages with cli

Condition formatting is a set of operations applied to raw inputs for
error messages that includes:

- Transforming a character vector of lines to a width-wrapped list of
  error bullets. This makes it easy to write messages in a list format
  where each bullet conveys a single important point.

      abort(c(
        "The error header",
        "*" = "An error bullet",
        "i" = "An info bullet",
        "x" = "A cross bullet"
      ))
      #> Error:
      #> ! The error header
      #> * An error bullet
      #> i An info bullet
      #> x A cross bullet

  See the [tidyverse error style
  guide](https://style.tidyverse.org/errors.html) for more about this
  style of error messaging.

- Applying style (emphasis, boldness, ...) and colours to message
  elements.

While the rlang package embeds rudimentary formatting routines, the main
formatting engine is implemented in the [cli
package](https://cli.r-lib.org/).

### Formatting messages with cli

By default, rlang uses an internal mechanism to format bullets. It is
preferable to delegate formatting to the [cli
package](https://cli.r-lib.org/) by using
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html),
[`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html), and
[`cli::cli_inform()`](https://cli.r-lib.org/reference/cli_abort.html)
instead of the rlang versions. These wrappers enable cli formatting with
sophisticated paragraph wrapping and bullet indenting that make long
lines easier to read. In the following example, a long `!` bullet is
broken with an indented newline:

    rlang::global_entrace(class = "errorr")
    #> Error in `rlang::global_entrace()`:
    #> ! `class` must be one of "error", "warning", or "message",
    #>   not "errorr".
    #> i Did you mean "error"?

The cli wrappers also add many features such as interpolation, semantic
formatting of text elements, and pluralisation:

    inform_marbles <- function(n_marbles) {
      cli::cli_inform(c(
        "i" = "I have {n_marbles} shiny marble{?s} in my bag.",
        "v" = "Way to go {.code cli::cli_inform()}!"
      ))
    }

    inform_marbles(1)
    #> i I have 1 shiny marble in my bag.
    #> v Way to go `cli::cli_inform()`!

    inform_marbles(2)
    #> i I have 2 shiny marbles in my bag.
    #> v Way to go `cli::cli_inform()`!

### Transitioning from [`abort()`](https://rlang.r-lib.org/reference/abort.md) to `cli_abort()`

If you plan to mass-rename calls from
[`abort()`](https://rlang.r-lib.org/reference/abort.md) to
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html), be
careful if you assemble error messages from user inputs. If these
individual pieces contain cli or glue syntax, this will result in
hard-to-debug errors and possibly [unexpected
behaviour](https://xkcd.com/327/).

    user_input <- "{base::stop('Wrong message.', call. = FALSE)}"
    cli::cli_abort(sprintf("Can't handle input `%s`.", user_input))
    #> Error:
    #> ! ! Could not evaluate cli `{}` expression: `base::stop('Wrong...`.
    #> Caused by error:
    #> ! Wrong message.

To avoid this, protect your error messages by using cli to assemble the
pieces:

    user_input <- "{base::stop('Wrong message.', call. = FALSE)}"
    cli::cli_abort("Can't handle input {.code {user_input}}.")
    #> Error:
    #> ! Can't handle input `{base::stop('Wrong message.', call. = FALSE)}`.

### Enabling cli formatting globally

To enable cli formatting for all
[`abort()`](https://rlang.r-lib.org/reference/abort.md) calls in your
namespace, call
[`local_use_cli()`](https://rlang.r-lib.org/reference/local_use_cli.md)
in the `onLoad` hook of your package. Using
[`on_load()`](https://rlang.r-lib.org/reference/on_load.md) (make sure
to call [`run_on_load()`](https://rlang.r-lib.org/reference/on_load.md)
in your hook):

    on_load(local_use_cli())

Enabling cli formatting in
[`abort()`](https://rlang.r-lib.org/reference/abort.md) is useful for:

- Transitioning from
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) to
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  progressively.

- Using [`abort()`](https://rlang.r-lib.org/reference/abort.md) when
  you'd like to disable interpolation syntax.

- Creating error conditions with
  [`error_cnd()`](https://rlang.r-lib.org/reference/cnd.md). These
  condition messages will be automatically formatted with cli as well.
