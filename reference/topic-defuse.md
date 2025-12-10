# Defusing R expressions

When a piece of R code is defused, R doesn't return its value like it
normally would. Instead it returns the expression in a special tree-like
object that describes how to compute a value. These defused expressions
can be thought of as blueprints or recipes for computing values.

Using [`expr()`](https://rlang.r-lib.org/reference/expr.md) we can
observe the difference between computing an expression and defusing it:

    # Return the result of `1 + 1`
    1 + 1
    #> [1] 2

    # Return the expression `1 + 1`
    expr(1 + 1)
    #> 1 + 1

Evaluation of a defused expression can be resumed at any time with
[`eval()`](https://rdrr.io/r/base/eval.html) (see also
[`eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.md)).

    # Return the expression `1 + 1`
    e <- expr(1 + 1)

    # Return the result of `1 + 1`
    eval(e)
    #> [1] 2

The most common use case for defusing expressions is to resume its
evaluation in a [data
mask](https://rlang.r-lib.org/reference/topic-data-mask.md). This makes
it possible for the expression to refer to columns of a data frame as if
they were regular objects.

    e <- expr(mean(cyl))
    eval(e, mtcars)
    #> [1] 6.1875

## Do I need to know about defused expressions?

As a tidyverse user you will rarely need to defuse expressions manually
with [`expr()`](https://rlang.r-lib.org/reference/expr.md), and even
more rarely need to resume evaluation with
[`eval()`](https://rdrr.io/r/base/eval.html) or
[`eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.md).
Instead, you call
[data-masking](https://rlang.r-lib.org/reference/topic-data-mask.md)
functions which take care of defusing your arguments and resuming them
in the context of a data mask.

    mtcars %>% dplyr::summarise(
      mean(cyl)  # This is defused and data-masked
    )
    #> # A tibble: 1 x 1
    #>   `mean(cyl)`
    #>         <dbl>
    #> 1        6.19

It is important to know that a function defuses its arguments because it
requires slightly different methods when called from a function. The
main thing is that arguments must be transported with the [embrace
operator](https://rlang.r-lib.org/reference/embrace-operator.md) `{{`.
It allows the data-masking function to defuse the correct expression.

    my_mean <- function(data, var) {
      dplyr::summarise(data, mean = mean({{ var }}))
    }

Read more about this in:

- [What is data-masking and why do I need
  {{?](https://rlang.r-lib.org/reference/topic-data-mask.md)

- [Data mask programming
  patterns](https://rlang.r-lib.org/reference/topic-data-mask-programming.md)

## The booby trap analogy

The term "defusing" comes from an analogy to the evaluation model in R.
As you may know, R uses lazy evaluation, which means that arguments are
only evaluated when they are needed for a computation. Let's take two
functions, `ignore()` which doesn't do anything with its argument, and
[`force()`](https://rdrr.io/r/base/force.html) which returns it:

    ignore <- function(arg) NULL
    force <- function(arg) arg

    ignore(warning("boom"))
    #> NULL

    force(warning("boom"))
    #> Warning in force(warning("boom")): boom

A warning is only emitted when the function actually *triggers*
evaluation of its argument. Evaluation of arguments can be chained by
passing them to other functions. If one of the functions ignores its
argument, it breaks the chain of evaluation.

    f <- function(x) g(x)
    g <- function(y) h(y)
    h <- function(z) ignore(z)

    f(warning("boom"))
    #> NULL

In a way, arguments are like *booby traps* which explode (evaluate) when
touched. Defusing an argument can be seen as defusing the booby trap.

    expr(force(warning("boom")))
    #> force(warning("boom"))

## Types of defused expressions

- **Calls**, like `f(1, 2, 3)` or `1 + 1` represent the action of
  calling a function to compute a new value, such as a vector.

- **Symbols**, like `x` or `df`, represent named objects. When the
  object pointed to by the symbol was defined in a function or in the
  global environment, we call it an environment-variable. When the
  object is a column in a data frame, we call it a data-variable.

- **Constants**, like `1` or `NULL`.

You can create new call or symbol objects by using the defusing function
[`expr()`](https://rlang.r-lib.org/reference/expr.md):

    # Create a symbol representing objects called `foo`
    expr(foo)
    #> foo

    # Create a call representing the computation of the mean of `foo`
    expr(mean(foo, na.rm = TRUE))
    #> mean(foo, na.rm = TRUE)

    # Return a constant
    expr(1)
    #> [1] 1

    expr(NULL)
    #> NULL

Defusing is not the only way to create defused expressions. You can also
assemble them from data:

    # Assemble a symbol from a string
    var <- "foo"
    sym(var)

    # Assemble a call from strings, symbols, and constants
    call("mean", sym(var), na.rm = TRUE)

## Local expressions versus function arguments

There are two main ways to defuse expressions, to which correspond two
functions in rlang,
[`expr()`](https://rlang.r-lib.org/reference/expr.md) and
[`enquo()`](https://rlang.r-lib.org/reference/enquo.md):

- You can defuse your *own* R expressions with
  [`expr()`](https://rlang.r-lib.org/reference/expr.md).

- You can defuse the expressions supplied by *the user* of your function
  with the `en`-prefixed operators, such as
  [`enquo()`](https://rlang.r-lib.org/reference/enquo.md) and
  [`enquos()`](https://rlang.r-lib.org/reference/enquo.md). These
  operators defuse function arguments.

## Defuse and inject

One purpose for defusing evaluation of an expression is to interface
with
[data-masking](https://rlang.r-lib.org/reference/topic-data-mask.md)
functions by injecting the expression back into another function with
`!!`. This is the [defuse-and-inject
pattern](https://rlang.r-lib.org/reference/topic-metaprogramming.md).

    my_summarise <- function(data, arg) {
      # Defuse the user expression in `arg`
      arg <- enquo(arg)

      # Inject the expression contained in `arg`
      # inside a `summarise()` argument
      data |> dplyr::summarise(mean = mean(!!arg, na.rm = TRUE))
    }

Defuse-and-inject is usually performed in a single step with the embrace
operator [`{{`](https://rlang.r-lib.org/reference/embrace-operator.md).

    my_summarise <- function(data, arg) {
      # Defuse and inject in a single step with the embracing operator
      data |> dplyr::summarise(mean = mean({{ arg }}, na.rm = TRUE))
    }

Using [`enquo()`](https://rlang.r-lib.org/reference/enquo.md) and `!!`
separately is useful in more complex cases where you need access to the
defused expression instead of just passing it on.

## Defused arguments and quosures

If you inspect the return values of
[`expr()`](https://rlang.r-lib.org/reference/expr.md) and
[`enquo()`](https://rlang.r-lib.org/reference/enquo.md), you'll notice
that the latter doesn't return a raw expression like the former. Instead
it returns a
[quosure](https://rlang.r-lib.org/reference/quosure-tools.md), a wrapper
containing an expression and an environment.

    expr(1 + 1)
    #> 1 + 1

    my_function <- function(arg) enquo(arg)
    my_function(1 + 1)
    #> <quosure>
    #> expr: ^1 + 1
    #> env:  global

R needs information about the environment to properly evaluate argument
expressions because they come from a different context than the current
function. For instance when a function in your package calls
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
the quosure environment indicates where all the private functions of
your package are defined.

Read more about the role of quosures in [What are quosures and when are
they needed?](https://rlang.r-lib.org/reference/topic-quosure.md).

## Comparison with base R

Defusing is known as *quoting* in other frameworks.

- The equivalent of
  [`expr()`](https://rlang.r-lib.org/reference/expr.md) is
  [`base::bquote()`](https://rdrr.io/r/base/bquote.html).

- The equivalent of
  [`enquo()`](https://rlang.r-lib.org/reference/enquo.md) is
  [`base::substitute()`](https://rdrr.io/r/base/substitute.html). The
  latter returns a naked expression instead of a quosure.

- There is no equivalent for `enquos(...)` but you can defuse dots as a
  list of naked expressions with `eval(substitute(alist(...)))`.
