# Display last messages and warnings

`last_warnings()` and `last_messages()` return a list of all warnings
and messages that occurred during the last R command.

[`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md)
must be active in order to log the messages and warnings.

By default the warnings and messages are printed with a simplified
backtrace, like
[`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md).
Use [`summary()`](https://rdrr.io/r/base/summary.html) to print the
conditions with a full backtrace.

## Usage

``` r
last_warnings(n = NULL)

last_messages(n = NULL)
```

## Arguments

- n:

  How many warnings or messages to display. Defaults to all.

## Examples

Enable backtrace capture with
[`global_entrace()`](https://rlang.r-lib.org/dev/reference/global_entrace.md):

    global_entrace()

Signal some warnings in nested functions. The warnings inform about
which function emitted a warning but they don't provide information
about the call stack:

    f <- function() { warning("foo"); g() }
    g <- function() { warning("bar", immediate. = TRUE); h() }
    h <- function() warning("baz")

    f()
    #> Warning in g() : bar
    #> Warning messages:
    #> 1: In f() : foo
    #> 2: In h() : baz

Call `last_warnings()` to see backtraces for each of these warnings:

    last_warnings()
    #> [[1]]
    #> <warning/rlang_warning>
    #> Warning in `f()`:
    #> foo
    #> Backtrace:
    #>     x
    #>  1. \-global f()
    #>
    #> [[2]]
    #> <warning/rlang_warning>
    #> Warning in `g()`:
    #> bar
    #> Backtrace:
    #>     x
    #>  1. \-global f()
    #>  2.   \-global g()
    #>
    #> [[3]]
    #> <warning/rlang_warning>
    #> Warning in `h()`:
    #> baz
    #> Backtrace:
    #>     x
    #>  1. \-global f()
    #>  2.   \-global g()
    #>  3.     \-global h()

This works similarly with messages:

    f <- function() { inform("Hey!"); g() }
    g <- function() { inform("Hi!"); h() }
    h <- function() inform("Hello!")

    f()
    #> Hey!
    #> Hi!
    #> Hello!

    rlang::last_messages()
    #> [[1]]
    #> <message/rlang_message>
    #> Message:
    #> Hey!
    #> ---
    #> Backtrace:
    #>     x
    #>  1. \-global f()
    #>
    #> [[2]]
    #> <message/rlang_message>
    #> Message:
    #> Hi!
    #> ---
    #> Backtrace:
    #>     x
    #>  1. \-global f()
    #>  2.   \-global g()
    #>
    #> [[3]]
    #> <message/rlang_message>
    #> Message:
    #> Hello!
    #> ---
    #> Backtrace:
    #>     x
    #>  1. \-global f()
    #>  2.   \-global g()
    #>  3.     \-global h()

## See also

[`last_error()`](https://rlang.r-lib.org/dev/reference/last_error.md)
