# Advanced defusal operators

These advanced operators
[defuse](https://rlang.r-lib.org/dev/reference/topic-defuse.md) R
expressions. [`expr()`](https://rlang.r-lib.org/dev/reference/expr.md),
[`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md), and
[`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md) are
sufficient for most purposes but rlang provides these other operations,
either for completeness or because they are useful to experts.

- `exprs()` is the plural variant of
  [`expr()`](https://rlang.r-lib.org/dev/reference/expr.md). It returns
  a list of expressions. It is like
  [`base::alist()`](https://rdrr.io/r/base/list.html) but with
  [injection](https://rlang.r-lib.org/dev/reference/topic-inject.md)
  support.

- `quo()` and `quos()` are like
  [`expr()`](https://rlang.r-lib.org/dev/reference/expr.md) and
  `exprs()` but return quosures instead of naked expressions. When you
  are defusing your own local expressions (by opposition to function
  arguments where non-local expressions are supplied by your users),
  there is generally no need to attach the current environment in a
  quosure. See [What are quosures and when are they
  needed?](https://rlang.r-lib.org/dev/reference/topic-quosure.md).

- `enexpr()` and `enexprs()` are like
  [`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) and
  [`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md) but
  return naked expressions instead of quosures. These operators should
  very rarely be used because they lose track of the environment of
  defused arguments.

- `ensym()` and `ensyms()` are like `enexpr()` and `enexprs()` but they
  throw an error when the defused expressions are not simple symbols.
  They also support strings which are interpreted as symbols. These
  functions are modelled on the behaviour of the left-hand side of `=`
  and `<-` where you can supply symbols and strings interchangeably.

      "foo" <- NULL
      list("foo" = NULL)

- `enquo0` and `enquos0()` are like
  [`enquo()`](https://rlang.r-lib.org/dev/reference/enquo.md) and
  [`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md) but
  without injection support. The injection operators `!!`, `!!!`, and
  `{{` are not processed, instead they are preserved in the defused
  expression. This makes it possible to defuse expressions that
  potentially contain injection operators meant for later use. The trade
  off is that it makes it harder for users to inject expressions in your
  function. They have to enable injection explicitly with
  [`inject()`](https://rlang.r-lib.org/dev/reference/inject.md).

  None of the features of [dynamic
  dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md) are available
  when defusing with `enquos0()`. For instance, trailing empty arguments
  are not automatically trimmed.

## Usage

``` r
enexpr(arg)

exprs(
  ...,
  .named = FALSE,
  .ignore_empty = c("trailing", "none", "all"),
  .unquote_names = TRUE
)

enexprs(
  ...,
  .named = FALSE,
  .ignore_empty = c("trailing", "none", "all"),
  .ignore_null = c("none", "all"),
  .unquote_names = TRUE,
  .homonyms = c("keep", "first", "last", "error"),
  .check_assign = FALSE
)

ensym(arg)

ensyms(
  ...,
  .named = FALSE,
  .ignore_empty = c("trailing", "none", "all"),
  .ignore_null = c("none", "all"),
  .unquote_names = TRUE,
  .homonyms = c("keep", "first", "last", "error"),
  .check_assign = FALSE
)

quo(expr)

quos(
  ...,
  .named = FALSE,
  .ignore_empty = c("trailing", "none", "all"),
  .unquote_names = TRUE
)

enquo0(arg)

enquos0(...)
```

## Arguments

- arg:

  An unquoted argument name. The expression supplied to that argument is
  defused and returned.

- ...:

  For `enexprs()`, `ensyms()` and
  [`enquos()`](https://rlang.r-lib.org/dev/reference/enquo.md), names of
  arguments to defuse. For `exprs()` and `quos()`, expressions to
  defuse.

- .named:

  If `TRUE`, unnamed inputs are automatically named with
  [`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md).
  This is equivalent to applying
  [`exprs_auto_name()`](https://rlang.r-lib.org/dev/reference/exprs_auto_name.md)
  on the result. If `FALSE`, unnamed elements are left as is and, if
  fully unnamed, the list is given minimal names (a vector of `""`). If
  `NULL`, fully unnamed results are left with `NULL` names.

- .ignore_empty:

  Whether to ignore empty arguments. Can be one of `"trailing"`,
  `"none"`, `"all"`. If `"trailing"`, only the last argument is ignored
  if it is empty. Named arguments are not considered empty.

- .unquote_names:

  Whether to treat `:=` as `=`. Unlike `=`, the `:=` syntax supports
  [names
  injection](https://rlang.r-lib.org/dev/reference/glue-operators.md).

- .ignore_null:

  Whether to ignore unnamed null arguments. Can be `"none"` or `"all"`.

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

- expr:

  An expression to defuse.

## Examples

``` r
# `exprs()` is the plural variant of `expr()`
exprs(foo, bar, bar)
#> [[1]]
#> foo
#> 
#> [[2]]
#> bar
#> 
#> [[3]]
#> bar
#> 

# `quo()` and `quos()` are the quosure variants of `expr()` and `exprs()`
quo(foo)
#> <quosure>
#> expr: ^foo
#> env:  0x55eb8a5f39d8
quos(foo, bar)
#> <list_of<quosure>>
#> 
#> [[1]]
#> <quosure>
#> expr: ^foo
#> env:  0x55eb8a5f39d8
#> 
#> [[2]]
#> <quosure>
#> expr: ^bar
#> env:  0x55eb8a5f39d8
#> 

# `enexpr()` and `enexprs()` are the naked variants of `enquo()` and `enquos()`
my_function1 <- function(arg) enexpr(arg)
my_function2 <- function(arg, ...) enexprs(arg, ...)
my_function1(1 + 1)
#> 1 + 1
my_function2(1 + 1, 10 * 2)
#> [[1]]
#> 1 + 1
#> 
#> [[2]]
#> 10 * 2
#> 


# `ensym()` and `ensyms()` are symbol variants of `enexpr()` and `enexprs()`
my_function3 <- function(arg) ensym(arg)
my_function4 <- function(arg, ...) ensyms(arg, ...)

# The user must supply symbols
my_function3(foo)
#> foo
my_function4(foo, bar)
#> [[1]]
#> foo
#> 
#> [[2]]
#> bar
#> 

# Complex expressions are an error
try(my_function3(1 + 1))
#> Error in ensym(arg) : Can't convert to a symbol.
try(my_function4(1 + 1, 10 * 2))
#> Error in sym(expr) : Can't convert a call to a symbol.


# `enquo0()` and `enquos0()` disable injection operators
automatic_injection <- function(x) enquo(x)
no_injection <- function(x) enquo0(x)

automatic_injection(foo(!!!1:3))
#> <quosure>
#> expr: ^foo(1L, 2L, 3L)
#> env:  0x55eb8a5f39d8
no_injection(foo(!!!1:3))
#> <quosure>
#> expr: ^foo(!!!1:3)
#> env:  0x55eb8a5f39d8

# Injection can still be done explicitly
inject(no_injection(foo(!!!1:3)))
#> <quosure>
#> expr: ^foo(1L, 2L, 3L)
#> env:  0x55eb8a5f39d8
```
