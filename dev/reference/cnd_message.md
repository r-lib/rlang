# Build an error message from parts

`cnd_message()` assembles an error message from three generics:

- `cnd_header()`

- `cnd_body()`

- `cnd_footer()`

Methods for these generics must return a character vector. The elements
are combined into a single string with a newline separator. Bullets
syntax is supported, either through rlang (see
[`format_error_bullets()`](https://rlang.r-lib.org/dev/reference/format_error_bullets.md)),
or through cli if the condition has `use_cli_format` set to `TRUE`.

The default method for the error header returns the `message` field of
the condition object. The default methods for the body and footer return
the the `body` and `footer` fields if any, or empty character vectors
otherwise.

`cnd_message()` is automatically called by the
[`conditionMessage()`](https://rdrr.io/r/base/conditions.html) for rlang
errors, warnings, and messages. Error classes created with
[`abort()`](https://rlang.r-lib.org/dev/reference/abort.md) only need to
implement header, body or footer methods. This provides a lot of
flexibility for hierarchies of error classes, for instance you could
inherit the body of an error message from a parent class while
overriding the header and footer.

## Usage

``` r
cnd_message(cnd, ..., inherit = TRUE, prefix = FALSE)

cnd_header(cnd, ...)

cnd_body(cnd, ...)

cnd_footer(cnd, ...)
```

## Arguments

- cnd:

  A condition object.

- ...:

  Arguments passed to methods.

- inherit:

  Wether to include parent messages. Parent messages are printed with a
  "Caused by error:" prefix, even if `prefix` is `FALSE`.

- prefix:

  Whether to print the full message, including the condition prefix
  (`Error:`, `Warning:`, `Message:`, or `Condition:`). The prefix
  mentions the `call` field if present, and the `srcref` info if
  present. If `cnd` has a `parent` field (i.e. the condition is
  chained), the parent messages are included in the message with a
  `Caused by` prefix.

## Overriding header, body, and footer methods

Sometimes the contents of an error message depends on the state of your
checking routine. In that case, it can be tricky to lazily generate
error messages with `cnd_header()`, `cnd_body()`, and `cnd_footer()`:
you have the choice between overspecifying your error class hierarchies
with one class per state, or replicating the type-checking control flow
within the `cnd_body()` method. None of these options are ideal.

A better option is to define `header`, `body`, or `footer` fields in
your condition object. These can be a static string, a
[lambda-formula](https://rlang.r-lib.org/dev/reference/as_function.md),
or a function with the same signature as `cnd_header()`, `cnd_body()`,
or `cnd_footer()`. These fields override the message generics and make
it easy to generate an error message tailored to the state in which the
error was constructed.
