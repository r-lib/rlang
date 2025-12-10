# Inform about name repair

Inform about name repair

## Usage

``` r
names_inform_repair(old, new)
```

## Arguments

- old:

  Original names vector.

- new:

  Repaired names vector.

## Muffling and silencing messages

Name repair messages are signaled with
[`inform()`](https://rlang.r-lib.org/dev/reference/abort.md) and are
given the class `"rlib_message_name_repair"`. These messages can be
muffled with
[`base::suppressMessages()`](https://rdrr.io/r/base/message.html).

Name repair messages can also be silenced with the global option
`rlib_name_repair_verbosity`. This option takes the values:

- `"verbose"`: Always verbose.

- `"quiet"`: Always quiet.

When set to quiet, the message is not displayed and the condition is not
signaled. This is particularly useful for silencing messages during
testing when combined with
[`local_options()`](https://rlang.r-lib.org/dev/reference/local_options.md).
