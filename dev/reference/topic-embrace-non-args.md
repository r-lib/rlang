# Does `{{` work on regular objects?

The embrace operator
[`{{`](https://rlang.r-lib.org/dev/reference/embrace-operator.md) should
be used exclusively with function arguments:

    fn <- function(arg) {
      quo(foo({{ arg }}))
    }

    fn(1 + 1)
    #> <quosure>
    #> expr: ^foo(^1 + 1)
    #> env:  0x7ffd89aac518

However you may have noticed that it also works on regular objects:

    fn <- function(arg) {
      arg <- force(arg)
      quo(foo({{ arg }}))
    }

    fn(1 + 1)
    #> <quosure>
    #> expr: ^foo(^2)
    #> env:  0x7ffd8a633398

In that case, `{{` captures the *value* of the expression instead of a
defused expression. That's because only function arguments can be
defused.

Note that this issue also applies to
[`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) (on which
`{{` is based).

## Why is this not an error?

Ideally we would have made `{{` on regular objects an error. However
this is not possible because in compiled R code it is not always
possible to distinguish a regular variable from a function argument. See
[Why are strings and other constants enquosed in the empty
environment?](https://rlang.r-lib.org/dev/reference/topic-embrace-constants.md)
for more about this.
