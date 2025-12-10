# Try an expression with condition handlers

**\[experimental\]**

`try_fetch()` establishes handlers for conditions of a given class
(`"error"`, `"warning"`, `"message"`, ...). Handlers are functions that
take a condition object as argument and are called when the
corresponding condition class has been signalled.

A condition handler can:

- **Recover from conditions** with a value. In this case the computation
  of `expr` is aborted and the recovery value is returned from
  `try_fetch()`. Error recovery is useful when you don't want errors to
  abruptly interrupt your program but resume at the catching site
  instead.

      # Recover with the value 0
      try_fetch(1 + "", error = function(cnd) 0)

- **Rethrow conditions**, e.g. using `abort(msg, parent = cnd)`. See the
  `parent` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md). This is
  typically done to add information to low-level errors about the
  high-level context in which they occurred.

      try_fetch(1 + "", error = function(cnd) abort("Failed.", parent = cnd))

- **Inspect conditions**, for instance to log data about warnings or
  errors. In this case, the handler must return the
  [`zap()`](https://rlang.r-lib.org/reference/zap.md) sentinel to
  instruct `try_fetch()` to ignore (or zap) that particular handler. The
  next matching handler is called if any, and errors bubble up to the
  user if no handler remains.

      log <- NULL
      try_fetch(1 + "", error = function(cnd) {
        log <<- cnd
        zap()
      })

Whereas [`tryCatch()`](https://rdrr.io/r/base/conditions.html) catches
conditions (discarding any running code along the way) and then calls
the handler, `try_fetch()` first calls the handler with the condition on
top of the currently running code (fetches it where it stands) and then
catches the return value. This is a subtle difference that has
implications for the debuggability of your functions. See the comparison
with [`tryCatch()`](https://rdrr.io/r/base/conditions.html) section
below.

Another difference between `try_fetch()` and the base equivalent is that
errors are matched across chains, see the `parent` argument of
[`abort()`](https://rlang.r-lib.org/reference/abort.md). This is a
useful property that makes `try_fetch()` insensitive to changes of
implementation or context of evaluation that cause a classed error to
suddenly get chained to a contextual error. Note that some chained
conditions are not inherited, see the `.inherit` argument of
[`abort()`](https://rlang.r-lib.org/reference/abort.md) or
[`warn()`](https://rlang.r-lib.org/reference/abort.md). In particular,
downgraded conditions (e.g. from error to warning or from warning to
message) are not matched across parents.

## Usage

``` r
try_fetch(expr, ...)
```

## Arguments

- expr:

  An R expression.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.md)\>
  Named condition handlers. The names specify the condition class for
  which a handler will be called.

## Stack overflows

A stack overflow occurs when a program keeps adding to itself until the
stack memory (whose size is very limited unlike heap memory) is
exhausted.

    # A function that calls itself indefinitely causes stack overflows
    f <- function() f()
    f()
    #> Error: C stack usage  9525680 is too close to the limit

Because memory is very limited when these errors happen, it is not
possible to call the handlers on the existing program stack. Instead,
error conditions are first caught by `try_fetch()` and only then error
handlers are called. Catching the error interrupts the program up to the
`try_fetch()` context, which allows R to reclaim stack memory.

The practical implication is that error handlers should never assume
that the whole call stack is preserved. For instance a
[`trace_back()`](https://rlang.r-lib.org/reference/trace_back.md)
capture might miss frames.

Note that error handlers are only run for stack overflows on R \>= 4.2.
On older versions of R the handlers are simply not run. This is because
these errors do not inherit from the class `stackOverflowError` before R
4.2. Consider using
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) instead with
critical error handlers that need to capture all errors on old versions
of R.

## Comparison with [`tryCatch()`](https://rdrr.io/r/base/conditions.html)

`try_fetch()` generalises
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) and
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html) in a
single function. It reproduces the behaviour of both calling and exiting
handlers depending on the return value of the handler. If the handler
returns the [`zap()`](https://rlang.r-lib.org/reference/zap.md)
sentinel, it is taken as a calling handler that declines to recover from
a condition. Otherwise, it is taken as an exiting handler which returns
a value from the catching site.

The important difference between
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) and `try_fetch()`
is that the program in `expr` is still fully running when an error
handler is called. Because the call stack is preserved, this makes it
possible to capture a full backtrace from within the handler, e.g. when
rethrowing the error with `abort(parent = cnd)`. Technically,
`try_fetch()` is more similar to (and implemented on top of)
[`base::withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
than `tryCatch().`
