# Use cli to format error messages

**\[experimental\]**

`local_use_cli()` marks a package namespace or the environment of a
running function with a special flag that instructs
[`abort()`](https://rlang.r-lib.org/reference/abort.md) to use cli to
format error messages. This formatting happens lazily, at print-time, in
various places:

- When an unexpected error is displayed to the user.

- When a captured error is printed in the console, for instance via
  [`last_error()`](https://rlang.r-lib.org/reference/last_error.md).

- When [`conditionMessage()`](https://rdrr.io/r/base/conditions.html) is
  called.

cli formats messages and bullets with indentation and width-wrapping to
produce a polished display of messages.

## Usage

``` r
local_use_cli(..., format = TRUE, inline = FALSE, frame = caller_env())
```

## Arguments

- ...:

  These dots are for future extensions and must be empty.

- format:

  Whether to use cli at print-time to format messages and bullets.

- inline:

  **\[experimental\]** Whether to use cli at throw-time to format the
  inline parts of a message. This makes it possible to use cli
  interpolation and formatting with
  [`abort()`](https://rlang.r-lib.org/reference/abort.md).

- frame:

  A package namespace or an environment of a running function.

## Usage

To use cli formatting automatically in your package:

1.  Make sure
    [`run_on_load()`](https://rlang.r-lib.org/reference/on_load.md) is
    called from your `.onLoad()` hook.

2.  Call `on_load(local_use_cli())` at the top level of your namespace.

It is also possible to call `local_use_cli()` inside a running function,
in which case the flag only applies within that function.
