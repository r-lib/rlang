# Does a condition or its ancestors inherit from a class?

Like any R objects, errors captured with catchers like
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) have a
[`class()`](https://rdrr.io/r/base/class.html) which you can test with
[`inherits()`](https://rdrr.io/r/base/class.html). However, with chained
errors, the class of a captured error might be different than the error
that was originally signalled. Use `cnd_inherits()` to detect whether an
error or any of its *parent* inherits from a class.

Whereas [`inherits()`](https://rdrr.io/r/base/class.html) tells you
whether an object is a particular kind of error, `cnd_inherits()`
answers the question whether an object is a particular kind of error or
has been caused by such an error.

Some chained conditions carry parents that are not inherited. See the
`.inherit` argument of
[`abort()`](https://rlang.r-lib.org/reference/abort.md),
[`warn()`](https://rlang.r-lib.org/reference/abort.md), and
[`inform()`](https://rlang.r-lib.org/reference/abort.md).

## Usage

``` r
cnd_inherits(cnd, class)
```

## Arguments

- cnd:

  A condition to test.

- class:

  A class passed to [`inherits()`](https://rdrr.io/r/base/class.html).

## Capture an error with `cnd_inherits()`

Error catchers like
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) and
[`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md) can only
match the class of a condition, not the class of its parents. To match a
class across the ancestry of an error, you'll need a bit of craftiness.

Ancestry matching can't be done with
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) at all so you'll
need to switch to
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html).
Alternatively, you can use the experimental rlang function
[`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md) which is
able to perform the roles of both
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) and
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html).

### [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)

Unlike [`tryCatch()`](https://rdrr.io/r/base/conditions.html),
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html) does
not capture an error. If you don't explicitly jump with an *error* or a
*value* throw, nothing happens.

Since we don't want to throw an error, we'll throw a value using
[`callCC()`](https://rdrr.io/r/base/callCC.html):

    f <- function() {
      parent <- error_cnd("bar", message = "Bar")
      abort("Foo", parent = parent)
    }

    cnd <- callCC(function(throw) {
      withCallingHandlers(
        f(),
        error = function(x) if (cnd_inherits(x, "bar")) throw(x)
      )
    })

    class(cnd)
    #> [1] "rlang_error" "error"       "condition"
    class(cnd$parent)
    #> [1] "bar"         "rlang_error" "error"       "condition"

### [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md)

This pattern is easier with
[`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md). Like
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html), it
doesn't capture a matching error right away. Instead, it captures it
only if the handler doesn't return a
[`zap()`](https://rlang.r-lib.org/reference/zap.md) value.

    cnd <- try_fetch(
      f(),
      error = function(x) if (cnd_inherits(x, "bar")) x else zap()
    )

    class(cnd)
    #> [1] "rlang_error" "error"       "condition"
    class(cnd$parent)
    #> [1] "bar"         "rlang_error" "error"       "condition"

Note that
[`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.md) uses
`cnd_inherits()` internally. This makes it very easy to match a parent
condition:

    cnd <- try_fetch(
      f(),
      bar = function(x) x
    )

    # This is the parent
    class(cnd)
    #> [1] "bar"         "rlang_error" "error"       "condition"
