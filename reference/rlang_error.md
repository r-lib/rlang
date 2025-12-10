# Errors of class `rlang_error`

[`abort()`](https://rlang.r-lib.org/reference/abort.md) and
[`error_cnd()`](https://rlang.r-lib.org/reference/cnd.md) create errors
of class `"rlang_error"`. The differences with base errors are:

- Implementing
  [`conditionMessage()`](https://rdrr.io/r/base/conditions.html) methods
  for subclasses of `"rlang_error"` is undefined behaviour. Instead,
  implement the
  [`cnd_header()`](https://rlang.r-lib.org/reference/cnd_message.md)
  method (and possibly
  [`cnd_body()`](https://rlang.r-lib.org/reference/cnd_message.md) and
  [`cnd_footer()`](https://rlang.r-lib.org/reference/cnd_message.md)).
  These methods return character vectors which are assembled by rlang
  when needed: when
  [`conditionMessage.rlang_error()`](https://rdrr.io/r/base/conditions.html)
  is called (e.g. via [`try()`](https://rdrr.io/r/base/try.html)), when
  the error is displayed through
  [`print()`](https://rdrr.io/r/base/print.html) or
  [`format()`](https://rdrr.io/r/base/format.html), and of course when
  the error is displayed to the user by
  [`abort()`](https://rlang.r-lib.org/reference/abort.md).

- [`cnd_header()`](https://rlang.r-lib.org/reference/cnd_message.md),
  [`cnd_body()`](https://rlang.r-lib.org/reference/cnd_message.md), and
  [`cnd_footer()`](https://rlang.r-lib.org/reference/cnd_message.md)
  methods can be overridden by storing closures in the `header`, `body`,
  and `footer` fields of the condition. This is useful to lazily
  generate messages based on state captured in the closure environment.

- **\[experimental\]** The `use_cli_format` condition field instructs
  whether to use cli (or rlang's fallback method if cli is not
  installed) to format the error message at print time.

  In this case, the `message` field may be a character vector of header
  and bullets. These are formatted at the last moment to take the
  context into account (starting position on the screen and
  indentation).

  See
  [`local_use_cli()`](https://rlang.r-lib.org/reference/local_use_cli.md)
  for automatically setting this field in errors thrown with
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) within your
  package.
