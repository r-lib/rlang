# The data mask ambiguity

[Data masking](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
is an R feature that blends programming variables that live inside
environments (env-variables) with statistical variables stored in data
frames (data-variables). This mixture makes it easy to refer to data
frame columns as well as objects defined in the current environment.

    x <- 100
    mtcars %>% dplyr::summarise(mean(disp / x))
    #> # A tibble: 1 x 1
    #>   `mean(disp/x)`
    #>            <dbl>
    #> 1           2.31

However this convenience introduces an ambiguity between data-variables
and env-variables which might cause **collisions**.

### Column collisions

In the following snippet, are we referring to the env-variable `x` or to
the data-variable of the same name?

    df <- data.frame(x = NA, y = 2)
    x <- 100

    df %>% dplyr::mutate(y = y / x)
    #>    x  y
    #> 1 NA NA

A column collision occurs when you want to use an object defined outside
of the data frame, but a column of the same name happens to exist.

### Object collisions

The opposite problem occurs when there is a typo in a data-variable name
and an env-variable of the same name exists:

    df <- data.frame(foo = "right")
    ffo <- "wrong"

    df %>% dplyr::mutate(foo = toupper(ffo))
    #>     foo
    #> 1 WRONG

Instead of a typo, it might also be that you were expecting a column in
the data frame which is unexpectedly missing. In both cases, if a
variable can't be found in the data mask, R looks for variables in the
surrounding environment. This isn't what we intended here and it would
have been better to fail early with a "Column not found" error.

### Preventing collisions

In casual scripts or interactive programming, data mask ambiguity is not
a huge deal compared to the payoff of iterating quickly while developing
your analysis. However in production code and in package functions, the
ambiguity might cause collision bugs in the long run.

Fortunately it is easy to be explicit about the scoping of variables
with a little more verbose code. This topic lists the solutions and
workarounds that have been created to solve ambiguity issues in data
masks.

#### The `.data` and `.env` pronouns

The simplest solution is to use the
[`.data`](https://rlang.r-lib.org/dev/reference/dot-data.md) and
[`.env`](https://rlang.r-lib.org/dev/reference/dot-data.md) pronouns to
disambiguate between data-variables and env-variables.

    df <- data.frame(x = 1, y = 2)
    x <- 100

    df %>% dplyr::mutate(y = .data$y / .env$x)
    #>   x    y
    #> 1 1 0.02

This is especially useful in functions because the data frame is not
known in advance and potentially contain masking columns for any of the
env-variables in scope in the function:

    my_rescale <- function(data, var, factor = 10) {
      data %>% dplyr::mutate("{{ var }}" := {{ var }} / factor)
    }

    # This works
    data.frame(value = 1) %>% my_rescale(value)
    #>   value
    #> 1   0.1

    # Oh no!
    data.frame(factor = 0, value = 1) %>% my_rescale(value)
    #>   factor value
    #> 1      0   Inf

Subsetting function arguments with `.env` ensures we never hit a masking
column:

    my_rescale <- function(data, var, factor = 10) {
      data %>% dplyr::mutate("{{ var }}" := {{ var }} / .env$factor)
    }

    # Yay!
    data.frame(factor = 0, value = 1) %>% my_rescale(value)
    #>   factor value
    #> 1      0   0.1

#### Subsetting `.data` with env-variables

The [`.data`](https://rlang.r-lib.org/dev/reference/dot-data.md) pronoun
may be used as a name-to-data-mask pattern (see [Data mask programming
patterns](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md)):

    var <- "cyl"
    mtcars %>% dplyr::summarise(mean = mean(.data[[var]]))
    #> # A tibble: 1 x 1
    #>    mean
    #>   <dbl>
    #> 1  6.19

In this example, the env-variable `var` is used inside the data mask to
subset the `.data` pronoun. Does this mean that `var` is at risk of a
column collision if the input data frame contains a column of the same
name? Fortunately not:

    var <- "cyl"

    mtcars2 <- mtcars
    mtcars2$var <- "wrong"

    mtcars2 %>% dplyr::summarise(mean = mean(.data[[var]]))
    #> # A tibble: 1 x 1
    #>    mean
    #>   <dbl>
    #> 1  6.19

The evaluation of `.data[[var]]` is set up in such a way that there is
no ambiguity. The `.data` pronoun can only be subsetted with
env-variables, not data-variables. Technically, this is because `[[`
behaves like an *injection operator* when applied to `.data`. It is
evaluated very early before the data mask is even created. See the `!!`
section below.

#### Injecting env-variables with `!!`

[Injection
operators](https://rlang.r-lib.org/dev/reference/topic-inject.md) such
as [`!!`](https://rlang.r-lib.org/dev/reference/injection-operator.md)
have interesting properties regarding the ambiguity problem. They modify
a piece of code early on by injecting objects or other expressions
before any data-masking logic comes into play. If you inject the *value*
of a variable, it becomes inlined in the expression. R no longer needs
to look up any variable to find the value.

Taking the earlier division example, let's use `!!` to inject the value
of the env-variable `x` inside the division expression:

    df <- data.frame(x = NA, y = 2)
    x <- 100

    df %>% dplyr::mutate(y = y / !!x)
    #>    x    y
    #> 1 NA 0.02

While injection solves issues of ambiguity, it is a bit heavy handed
compared to using the
[`.env`](https://rlang.r-lib.org/dev/reference/dot-data.md) pronoun. Big
objects inlined in expressions might cause issues in unexpected places,
for instance they might make the calls in a
[`traceback()`](https://rdrr.io/r/base/traceback.html) less readable.

### No ambiguity in tidy selections

[Tidy selection](https://tidyselect.r-lib.org/reference/language.html)
is a dialect of R that optimises column selection in tidyverse packages.
Examples of functions that use tidy selections are
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
and `tidyr::pivot_longer()`.

Unlike data masking, tidy selections do not suffer from ambiguity. The
selection language is designed in such a way that evaluation of
expressions is either scoped in the data mask only, or in the
environment only. Take this example:

    mtcars %>% dplyr::select(gear:ncol(mtcars))

`gear` is a symbol supplied to a selection operator `:` and thus scoped
in the data mask only. Any other kind of expression, such as
`ncol(mtcars)`, is evaluated as normal R code outside of any data
context. This is why there is no column collision here:

    data <- data.frame(x = 1, data = 1:3)

    data %>% dplyr::select(data:ncol(data))
    #>   data
    #> 1    1
    #> 2    2
    #> 3    3

It is useful to introduce two new terms. Tidy selections distinguish
data-expressions and env-expressions:

- `data` is a data-expression that refers to the data-variable.

- `ncol(data)` is an env-expression that refers to the env-variable.

To learn more about the difference between the two kinds of expressions,
see the [technical description of the tidy selection
syntax](https://tidyselect.r-lib.org/articles/syntax.html).

#### Names pattern with `all_of()`

`all_of()` is often used in functions as a [programming
pattern](https://rlang.r-lib.org/dev/reference/topic-data-mask-programming.md)
that connects column names to a data mask, similarly to the
[`.data`](https://rlang.r-lib.org/dev/reference/dot-data.md) pronoun. A
simple example is:

    my_group_by <- function(data, vars) {
      data %>% dplyr::group_by(across(all_of(vars)))
    }

If tidy selections were affected by the data mask ambiguity, this
function would be at risk of a column collision. It would break as soon
as the user supplies a data frame containing a `vars` column. However,
`all_of()` is an env-expression that is evaluated outside of the data
mask, so there is no possibility of collisions.
