# Documentation anchor for error arguments

Use `@inheritParams rlang::args_error_context` in your package to
document `arg` and `call` arguments (or equivalently their prefixed
versions `error_arg` and `error_call`).

- `arg` parameters should be formatted as argument (e.g. using cli's
  `.arg` specifier) and included in error messages. See also
  [`caller_arg()`](https://rlang.r-lib.org/reference/caller_arg.md).

- `call` parameters should be included in error conditions in a field
  named `call`. An easy way to do this is by passing a `call` argument
  to [`abort()`](https://rlang.r-lib.org/reference/abort.md). See also
  [`local_error_call()`](https://rlang.r-lib.org/reference/local_error_call.md).

## Arguments

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- error_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.

- error_call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.
