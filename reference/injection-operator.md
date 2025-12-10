# Injection operator `!!`

The [injection](https://rlang.r-lib.org/reference/topic-inject.md)
operator `!!` injects a value or expression inside another expression.
In other words, it modifies a piece of code before R evaluates it.

There are two main cases for injection. You can inject constant values
to work around issues of [scoping
ambiguity](https://rlang.r-lib.org/reference/topic-data-mask-ambiguity.md),
and you can inject [defused
expressions](https://rlang.r-lib.org/reference/topic-defuse.md) like
[symbolised](https://rlang.r-lib.org/reference/sym.md) column names.

## Where does `!!` work?

`!!` does not work everywhere, you can only use it within certain
special functions:

- Functions taking
  [defused](https://rlang.r-lib.org/reference/topic-defuse.md) and
  [data-masked](https://rlang.r-lib.org/reference/topic-data-mask.md)
  arguments.

  Technically, this means function arguments defused with
  [`{{`](https://rlang.r-lib.org/reference/embrace-operator.md) or
  `en`-prefixed operators like
  [`enquo()`](https://rlang.r-lib.org/reference/enquo.md),
  [`enexpr()`](https://rlang.r-lib.org/reference/defusing-advanced.md),
  etc.

- Inside [`inject()`](https://rlang.r-lib.org/reference/inject.md).

All data-masking verbs in the tidyverse support injection operators out
of the box. With base functions, you need to use
[`inject()`](https://rlang.r-lib.org/reference/inject.md) to enable
`!!`. Using `!!` out of context may lead to incorrect results, see [What
happens if I use injection operators out of
context?](https://rlang.r-lib.org/reference/topic-inject-out-of-context.md).

The examples below are built around the base function
[`with()`](https://rdrr.io/r/base/with.html). Since it's not a tidyverse
function we will use
[`inject()`](https://rlang.r-lib.org/reference/inject.md) to enable `!!`
usage.

## Injecting values

Data-masking functions like [`with()`](https://rdrr.io/r/base/with.html)
are handy because you can refer to column names in your computations.
This comes at the price of data mask ambiguity: if you have defined an
env-variable of the same name as a data-variable, you get a name
collisions. This collision is always resolved by giving precedence to
the data-variable (it masks the env-variable):

    cyl <- c(100, 110)
    with(mtcars, mean(cyl))
    #> [1] 6.1875

The injection operator offers one way of solving this. Use it to inject
the env-variable inside the data-masked expression:

    inject(
      with(mtcars, mean(!!cyl))
    )
    #> [1] 105

Note that the [`.env`](https://rlang.r-lib.org/reference/dot-data.md)
pronoun is a simpler way of solving the ambiguity. See [The data mask
ambiguity](https://rlang.r-lib.org/reference/topic-data-mask-ambiguity.md)
for more about this.

## Injecting expressions

Injection is also useful for modifying parts of a [defused
expression](https://rlang.r-lib.org/reference/topic-defuse.md). In the
following example we use the [symbolise-and-inject
pattern](https://rlang.r-lib.org/reference/topic-metaprogramming.md) to
inject a column name inside a data-masked expression.

    var <- sym("cyl")
    inject(
      with(mtcars, mean(!!var))
    )
    #> [1] 6.1875

Since [`with()`](https://rdrr.io/r/base/with.html) is a base function,
you can't inject
[quosures](https://rlang.r-lib.org/reference/topic-quosure.md), only
naked symbols and calls. This isn't a problem here because we're
injecting the name of a data frame column. If the environment is
important, try injecting a pre-computed value instead.

## When do I need `!!`?

With tidyverse APIs, injecting expressions with `!!` is no longer a
common pattern. First, the
[`.env`](https://rlang.r-lib.org/reference/dot-data.md) pronoun solves
the ambiguity problem in a more intuitive way:

    cyl <- 100
    mtcars %>% dplyr::mutate(cyl = cyl * .env$cyl)

Second, the embrace operator
[`{{`](https://rlang.r-lib.org/reference/embrace-operator.md) makes the
[defuse-and-inject
pattern](https://rlang.r-lib.org/reference/topic-metaprogramming.md)
easier to learn and use.

    my_mean <- function(data, var) {
      data %>% dplyr::summarise(mean({{ var }}))
    }

    # Equivalent to
    my_mean <- function(data, var) {
      data %>% dplyr::summarise(mean(!!enquo(var)))
    }

`!!` is a good tool to learn for advanced applications but our hope is
that it isn't needed for common data analysis cases.

## See also

- [Injecting with !!, !!!, and glue
  syntax](https://rlang.r-lib.org/reference/topic-inject.md)

- [Metaprogramming
  patterns](https://rlang.r-lib.org/reference/topic-metaprogramming.md)
