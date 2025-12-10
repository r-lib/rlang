# Customising condition messages

Various aspects of the condition messages displayed by
[`abort()`](https://rlang.r-lib.org/reference/abort.md),
[`warn()`](https://rlang.r-lib.org/reference/abort.md), and
[`inform()`](https://rlang.r-lib.org/reference/abort.md) can be
customised using options from the [cli](https://cli.r-lib.org) package.

## Turning off unicode bullets

By default, bulleted lists are prefixed with unicode symbols:

    rlang::abort(c(
      "The error message.",
      "*" = "Regular bullet.",
      "i" = "Informative bullet.",
      "x" = "Cross bullet.",
      "v" = "Victory bullet.",
      ">" = "Arrow bullet."
    ))
    #> Error:
    #> ! The error message.
    #> • Regular bullet.
    #> ℹ Informative bullet.
    #> ✖ Cross bullet.
    #> ✔ Victory bullet.
    #> → Arrow bullet.

Set this option to use simple letters instead:

    options(cli.condition_unicode_bullets = FALSE)

    rlang::abort(c(
      "The error message.",
      "*" = "Regular bullet.",
      "i" = "Informative bullet.",
      "x" = "Cross bullet.",
      "v" = "Victory bullet.",
      ">" = "Arrow bullet."
    ))
    #> Error:
    #> ! The error message.
    #> * Regular bullet.
    #> i Informative bullet.
    #> x Cross bullet.
    #> v Victory bullet.
    #> > Arrow bullet.

## Changing the bullet symbols

You can specify what symbol to use for each type of bullet through your
cli user theme. For instance, here is how to uniformly use `*` for all
bullet kinds:

    options(cli.user_theme = list(
      ".cli_rlang .bullet-*" = list(before = "* "),
      ".cli_rlang .bullet-i" = list(before = "* "),
      ".cli_rlang .bullet-x" = list(before = "* "),
      ".cli_rlang .bullet-v" = list(before = "* "),
      ".cli_rlang .bullet->" = list(before = "* ")
    ))

    rlang::abort(c(
      "The error message.",
      "*" = "Regular bullet.",
      "i" = "Informative bullet.",
      "x" = "Cross bullet.",
      "v" = "Victory bullet.",
      ">" = "Arrow bullet."
    ))
    #> Error:
    #> ! The error message.
    #> * Regular bullet.
    #> * Informative bullet.
    #> * Cross bullet.
    #> * Victory bullet.
    #> * Arrow bullet.

If you want all the bullets to be the same, including the leading
bullet, you can achieve this using the `bullet` class:

    options(cli.user_theme = list(
      ".cli_rlang .bullet" = list(before = "* ")
    ))

    rlang::abort(c(
      "The error message.",
      "*" = "Regular bullet.",
      "i" = "Informative bullet.",
      "x" = "Cross bullet.",
      "v" = "Victory bullet.",
      ">" = "Arrow bullet."
    ))
    #> Error:
    #> * The error message.
    #> * Regular bullet.
    #> * Informative bullet.
    #> * Cross bullet.
    #> * Victory bullet.
    #> * Arrow bullet.

## Changing the foreground and background colour of error calls

When called inside a function,
[`abort()`](https://rlang.r-lib.org/reference/abort.md) displays the
function call to help contextualise the error:

    splash <- function() {
      abort("Can't splash without water.")
    }

    splash()
    #> Error in `splash()`:
    #> ! Can't splash without water.

The call is formatted with cli as a `code` element. This is not visible
in the manual, but code text is formatted with a highlighted background
colour by default. When this can be reliably detected, that background
colour is different depending on whether you're using a light or dark
theme.

You can override the colour of code elements in your cli theme. Here is
my personal configuration that fits well with the colour theme I
currently use in my IDE:

    options(cli.user_theme = list(
      span.code = list(
        "background-color" = "#3B4252",
        color = "#E5E9F0"
      )
    ))
