# Injecting with `!!`, `!!!`, and glue syntax

The injection operators are extensions of R implemented by rlang to
modify a piece of code before R processes it. There are two main
families:

- The [dynamic dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md)
  operators,
  [`!!!`](https://rlang.r-lib.org/dev/reference/splice-operator.md) and
  [`"{"`](https://rlang.r-lib.org/dev/reference/glue-operators.md).

- The [metaprogramming
  operators](https://rlang.r-lib.org/dev/reference/topic-metaprogramming.md)
  [`!!`](https://rlang.r-lib.org/dev/reference/injection-operator.md),
  [`{{`](https://rlang.r-lib.org/dev/reference/embrace-operator.md), and
  [`"{{"`](https://rlang.r-lib.org/dev/reference/glue-operators.md).
  Splicing with
  [`!!!`](https://rlang.r-lib.org/dev/reference/splice-operator.md) can
  also be done in metaprogramming context.

## Dots injection

Unlike regular `...`, [dynamic
dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md) are
programmable with injection operators.

### Splicing with `!!!`

For instance, take a function like
[`rbind()`](https://rdrr.io/r/base/cbind.html) which takes data in
`...`. To bind rows, you supply them as separate arguments:

    rbind(a = 1:2, b = 3:4)
    #>   [,1] [,2]
    #> a    1    2
    #> b    3    4

But how do you bind a variable number of rows stored in a list? The base
R solution is to invoke [`rbind()`](https://rdrr.io/r/base/cbind.html)
with [`do.call()`](https://rdrr.io/r/base/do.call.html):

    rows <- list(a = 1:2, b = 3:4)

    do.call("rbind", rows)
    #>   [,1] [,2]
    #> a    1    2
    #> b    3    4

Functions that implement dynamic dots include a built-in way of folding
a list of arguments in `...`. To illustrate this, we'll create a variant
of [`rbind()`](https://rdrr.io/r/base/cbind.html) that takes dynamic
dots by collecting `...` with
[`list2()`](https://rlang.r-lib.org/dev/reference/list2.md):

    rbind2 <- function(...) {
      do.call("rbind", list2(...))
    }

It can be used just like [`rbind()`](https://rdrr.io/r/base/cbind.html):

    rbind2(a = 1:2, b = 3:4)
    #>   [,1] [,2]
    #> a    1    2
    #> b    3    4

And a list of arguments can be supplied by *splicing* the list with
[`!!!`](https://rlang.r-lib.org/dev/reference/splice-operator.md):

    rbind2(!!!rows, c = 5:6)
    #>   [,1] [,2]
    #> a    1    2
    #> b    3    4
    #> c    5    6

### Injecting names with `"{"`

A related problem comes up when an argument name is stored in a
variable. With dynamic dots, you can inject the name using [glue
syntax](https://glue.tidyverse.org/) with
[`"{"`](https://rlang.r-lib.org/dev/reference/glue-operators.md):

    name <- "foo"

    rbind2("{name}" := 1:2, bar = 3:4)
    #>     [,1] [,2]
    #> foo    1    2
    #> bar    3    4

    rbind2("prefix_{name}" := 1:2, bar = 3:4)
    #>            [,1] [,2]
    #> prefix_foo    1    2
    #> bar           3    4

## Metaprogramming injection

[Data-masked](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
arguments support the following injection operators. They can also be
explicitly enabled with
[`inject()`](https://rlang.r-lib.org/dev/reference/inject.md).

### Embracing with `{{`

The embracing operator
[`{{`](https://rlang.r-lib.org/dev/reference/embrace-operator.md) is
made specially for function arguments. It
[defuses](https://rlang.r-lib.org/dev/reference/topic-defuse.md) the
expression supplied as argument and immediately injects it in place. The
injected argument is then evaluated in another context such as a [data
mask](https://rlang.r-lib.org/dev/reference/topic-data-mask.md).

    # Inject function arguments that might contain
    # data-variables by embracing them with {{ }}
    mean_by <- function(data, by, var) {
      data %>%
        dplyr::group_by({{ by }}) %>%
        dplyr::summarise(avg = mean({{ var }}, na.rm = TRUE))
    }

    # The data-variables `cyl` and `disp` inside the
    # env-variables `by` and `var` are injected inside `group_by()`
    # and `summarise()`
    mtcars %>% mean_by(by = cyl, var = disp)
    #> # A tibble: 3 x 2
    #>     cyl   avg
    #>   <dbl> <dbl>
    #> 1     4  105.
    #> 2     6  183.
    #> 3     8  353.

Learn more about this pattern in [Data mask programming
patterns](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md).

### Injecting with `!!`

Unlike [`!!!`](https://rlang.r-lib.org/dev/reference/splice-operator.md)
which injects a list of arguments, the injection operator
[`!!`](https://rlang.r-lib.org/dev/reference/injection-operator.md)
(pronounced "bang-bang") injects a *single* object. One use case for
`!!` is to substitute an environment-variable (created with `<-`) with a
data-variable (inside a data frame).

    # The env-variable `var` contains a data-symbol object, in this
    # case a reference to the data-variable `height`
    var <- data_sym("disp")

    # We inject the data-variable contained in `var` inside `summarise()`
    mtcars %>%
      dplyr::summarise(avg = mean(!!var, na.rm = TRUE))
    #> # A tibble: 1 x 1
    #>     avg
    #>   <dbl>
    #> 1  231.

Another use case is to inject a variable by value to avoid [name
collisions](https://rlang.r-lib.org/dev/reference/topic-data-mask-ambiguity.md).

    df <- data.frame(x = 1)

    # This name conflicts with a column in `df`
    x <- 100

    # Inject the env-variable
    df %>%
      dplyr::mutate(x = x / !!x)
    #>      x
    #> 1 0.01

Note that in most cases you don't need injection with `!!`. For
instance, the
[`.data`](https://rlang.r-lib.org/dev/reference/dot-data.md) and
[`.env`](https://rlang.r-lib.org/dev/reference/dot-data.md) pronouns
provide more intuitive alternatives to injecting a column name and
injecting a value.

### Splicing with `!!!`

The splice operator
[`!!!`](https://rlang.r-lib.org/dev/reference/splice-operator.md) of
dynamic dots can also be used in metaprogramming context (inside
[data-masked](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
arguments and inside
[`inject()`](https://rlang.r-lib.org/dev/reference/inject.md)). For
instance, we could reimplement the `rbind2()` function presented above
using [`inject()`](https://rlang.r-lib.org/dev/reference/inject.md)
instead of [`do.call()`](https://rdrr.io/r/base/do.call.html):

    rbind2 <- function(...) {
      inject(rbind(!!!list2(...)))
    }

There are two things going on here. We collect `...` with
[`list2()`](https://rlang.r-lib.org/dev/reference/list2.md) so that the
callers of `rbind2()` may use `!!!`. And we use
[`inject()`](https://rlang.r-lib.org/dev/reference/inject.md) so that
`rbind2()` itself may use `!!!` to splice the list of arguments passed
to `rbind2()`.

## Injection in other languages

Injection is known as **quasiquotation** in other programming languages
and in computer science.
[`expr()`](https://rlang.r-lib.org/dev/reference/expr.md) is similar to
a quasiquotation operator and `!!` is the unquote operator. These terms
have a rich history in Lisp languages, and live on in modern languages
like [Julia](https://docs.julialang.org/en/v1/manual/metaprogramming/)
and [Racket](https://docs.racket-lang.org/reference/quasiquote.html). In
base R, quasiquotation is performed with
[`bquote()`](https://rdrr.io/r/base/bquote.html).

The main difference between rlang and other languages is that
quasiquotation is often implicit instead of explicit. You can use
injection operators in any defusing / quoting function (unless that
function defuses its argument with a special operator like
[`enquo0()`](https://rlang.r-lib.org/dev/reference/defusing-advanced.md)).
This is not the case in lisp languages for example where injection /
unquoting is explicit and only enabled within a backquote.

## See also

- [What happens if I use injection operators out of
  context?](https://rlang.r-lib.org/dev/reference/topic-inject-out-of-context.md)
