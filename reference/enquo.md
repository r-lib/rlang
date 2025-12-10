# Defuse function arguments

`enquo()` and `enquos()`
[defuse](https://rlang.r-lib.org/reference/topic-defuse.md) function
arguments. A defused expression can be examined, modified, and injected
into other expressions.

Defusing function arguments is useful for:

- Creating data-masking functions.

- Interfacing with another
  [data-masking](https://rlang.r-lib.org/reference/topic-data-mask.md)
  function using the
  [defuse-and-inject](https://rlang.r-lib.org/reference/topic-metaprogramming.md)
  pattern.

These are advanced tools. Make sure to first learn about the embrace
operator [`{{`](https://rlang.r-lib.org/reference/embrace-operator.md)
in [Data mask programming
patterns](https://rlang.r-lib.org/reference/topic-data-mask-programming.md).
`{{` is easier to work with less theory, and it is sufficient in most
applications.

## Usage

``` r
enquo(arg)

enquos(
  ...,
  .named = FALSE,
  .ignore_empty = c("trailing", "none", "all"),
  .ignore_null = c("none", "all"),
  .unquote_names = TRUE,
  .homonyms = c("keep", "first", "last", "error"),
  .check_assign = FALSE
)
```

## Arguments

- arg:

  An unquoted argument name. The expression supplied to that argument is
  defused and returned.

- ...:

  Names of arguments to defuse.

- .named:

  If `TRUE`, unnamed inputs are automatically named with
  [`as_label()`](https://rlang.r-lib.org/reference/as_label.md). This is
  equivalent to applying
  [`exprs_auto_name()`](https://rlang.r-lib.org/reference/exprs_auto_name.md)
  on the result. If `FALSE`, unnamed elements are left as is and, if
  fully unnamed, the list is given minimal names (a vector of `""`). If
  `NULL`, fully unnamed results are left with `NULL` names.

- .ignore_empty:

  Whether to ignore empty arguments. Can be one of `"trailing"`,
  `"none"`, `"all"`. If `"trailing"`, only the last argument is ignored
  if it is empty. Named arguments are not considered empty.

- .ignore_null:

  Whether to ignore unnamed null arguments. Can be `"none"` or `"all"`.

- .unquote_names:

  Whether to treat `:=` as `=`. Unlike `=`, the `:=` syntax supports
  [names
  injection](https://rlang.r-lib.org/reference/glue-operators.md).

- .homonyms:

  How to treat arguments with the same name. The default, `"keep"`,
  preserves these arguments. Set `.homonyms` to `"first"` to only keep
  the first occurrences, to `"last"` to keep the last occurrences, and
  to `"error"` to raise an informative error and indicate what arguments
  have duplicated names.

- .check_assign:

  Whether to check for `<-` calls. When `TRUE` a warning recommends
  users to use `=` if they meant to match a function parameter or wrap
  the `<-` call in curly braces otherwise. This ensures assignments are
  explicit.

## Value

`enquo()` returns a
[quosure](https://rlang.r-lib.org/reference/topic-quosure.md) and
`enquos()` returns a list of quosures.

## Implicit injection

Arguments defused with `enquo()` and `enquos()` automatically gain
[injection](https://rlang.r-lib.org/reference/topic-inject.md) support.

    my_mean <- function(data, var) {
      var <- enquo(var)
      dplyr::summarise(data, mean(!!var))
    }

    # Can now use `!!` and `{{`
    my_mean(mtcars, !!sym("cyl"))

See [`enquo0()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
and
[`enquos0()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
for variants that don't enable injection.

## See also

- [Defusing R
  expressions](https://rlang.r-lib.org/reference/topic-defuse.md) for an
  overview.

- [`expr()`](https://rlang.r-lib.org/reference/expr.md) to defuse your
  own local expressions.

- [Advanced defusal
  operators](https://rlang.r-lib.org/reference/defusing-advanced.md).

- [`base::eval()`](https://rdrr.io/r/base/eval.html) and
  [`eval_bare()`](https://rlang.r-lib.org/reference/eval_bare.md) for
  resuming evaluation of a defused expression.

## Examples

``` r
# `enquo()` defuses the expression supplied by your user
f <- function(arg) {
  enquo(arg)
}

f(1 + 1)
#> <quosure>
#> expr: ^1 + 1
#> env:  0x55fbe2c1f010

# `enquos()` works with arguments and dots. It returns a list of
# expressions
f <- function(...) {
  enquos(...)
}

f(1 + 1, 2 * 10)
#> <list_of<quosure>>
#> 
#> [[1]]
#> <quosure>
#> expr: ^1 + 1
#> env:  0x55fbe2c1f010
#> 
#> [[2]]
#> <quosure>
#> expr: ^2 * 10
#> env:  0x55fbe2c1f010
#> 


# `enquo()` and `enquos()` enable _injection_ and _embracing_ for
# your users
g <- function(arg) {
  f({{ arg }} * 2)
}
g(100)
#> <list_of<quosure>>
#> 
#> [[1]]
#> <quosure>
#> expr: ^(^100) * 2
#> env:  0x55fbe1b386e0
#> 

column <- sym("cyl")
g(!!column)
#> <list_of<quosure>>
#> 
#> [[1]]
#> <quosure>
#> expr: ^(^cyl) * 2
#> env:  0x55fbdfe0d600
#> 
```
