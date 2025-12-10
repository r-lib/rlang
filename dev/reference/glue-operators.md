# Name injection with `"{"` and `"{{"`

[Dynamic dots](https://rlang.r-lib.org/dev/reference/dyn-dots.md) (and
[data-masked](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
dots which are dynamic by default) have built-in support for names
interpolation with the [glue package](https://glue.tidyverse.org/).

    tibble::tibble(foo = 1)
    #> # A tibble: 1 x 1
    #>     foo
    #>   <dbl>
    #> 1     1

    foo <- "name"
    tibble::tibble("{foo}" := 1)
    #> # A tibble: 1 x 1
    #>    name
    #>   <dbl>
    #> 1     1

Inside functions, embracing an argument with
[`{{`](https://rlang.r-lib.org/dev/reference/embrace-operator.md)
inserts the expression supplied as argument in the string. This gives an
indication on the variable or computation supplied as argument:

    tib <- function(x) {
      tibble::tibble("var: {{ x }}" := x)
    }

    tib(1 + 1)
    #> # A tibble: 1 x 1
    #>   `var: 1 + 1`
    #>          <dbl>
    #> 1            2

See also [`englue()`](https://rlang.r-lib.org/dev/reference/englue.md)
to string-embrace outside of dynamic dots.

    g <- function(x) {
      englue("var: {{ x }}")
    }

    g(1 + 1)
    #> [1] "var: 1 + 1"

Technically, `"{{"`
[defuses](https://rlang.r-lib.org/dev/reference/topic-defuse.md) a
function argument, calls
[`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md) on the
expression supplied as argument, and inserts the result in the string.

### `"{"` and `"{{"`

While [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
only supports `"{"`, dynamic dots support both `"{"` and `"{{"`. The
double brace variant is similar to the embrace operator
[`{{`](https://rlang.r-lib.org/dev/reference/embrace-operator.md)
available in
[data-masked](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
arguments.

In the following example, the embrace operator is used in a glue string
to name the result with a default name that represents the expression
supplied as argument:

    my_mean <- function(data, var) {
      data %>% dplyr::summarise("{{ var }}" := mean({{ var }}))
    }

    mtcars %>% my_mean(cyl)
    #> # A tibble: 1 x 1
    #>     cyl
    #>   <dbl>
    #> 1  6.19

    mtcars %>% my_mean(cyl * am)
    #> # A tibble: 1 x 1
    #>   `cyl * am`
    #>        <dbl>
    #> 1       2.06

`"{{"` is only meant for inserting an expression supplied as argument to
a function. The result of the expression is not inspected or used. To
interpolate a string stored in a variable, use the regular glue operator
`"{"` instead:

    my_mean <- function(data, var, name = "mean") {
      data %>% dplyr::summarise("{name}" := mean({{ var }}))
    }

    mtcars %>% my_mean(cyl)
    #> # A tibble: 1 x 1
    #>    mean
    #>   <dbl>
    #> 1  6.19

    mtcars %>% my_mean(cyl, name = "cyl")
    #> # A tibble: 1 x 1
    #>     cyl
    #>   <dbl>
    #> 1  6.19

Using the wrong operator causes unexpected results:

    x <- "name"

    list2("{{ x }}" := 1)
    #> $`"name"`
    #> [1] 1

    list2("{x}" := 1)
    #> $name
    #> [1] 1

Ideally, using `{{` on regular objects would be an error. However for
technical reasons it is not possible to make a distinction between
function arguments and ordinary variables. See [Does {{ work on regular
objects?](https://rlang.r-lib.org/dev/reference/topic-embrace-non-args.md)
for more information about this limitation.

### Allow overriding default names

The implementation of `my_mean()` in the previous section forces a
default name onto the result. But what if the caller wants to give it a
different name? In functions that take dots, it is possible to just
supply a named expression to override the default. In a function like
`my_mean()` that takes a named argument we need a different approach.

This is where
[`englue()`](https://rlang.r-lib.org/dev/reference/englue.md) becomes
useful. We can pull out the default name creation in another user-facing
argument like this:

    my_mean <- function(data, var, name = englue("{{ var }}")) {
      data %>% dplyr::summarise("{name}" := mean({{ var }}))
    }

Now the user may supply their own name if needed:

    mtcars %>% my_mean(cyl * am)
    #> # A tibble: 1 x 1
    #>   `cyl * am`
    #>        <dbl>
    #> 1       2.06

    mtcars %>% my_mean(cyl * am, name = "mean_cyl_am")
    #> # A tibble: 1 x 1
    #>   mean_cyl_am
    #>         <dbl>
    #> 1        2.06

### What's the deal with `:=`?

Name injection in dynamic dots was originally implemented with `:=`
instead of `=` to allow complex expressions on the LHS:

    x <- "name"
    list2(!!x := 1)
    #> $name
    #> [1] 1

Name-injection with glue operations was an extension of this existing
feature and so inherited the same interface. However, there is no
technical barrier to using glue strings on the LHS of `=`.

### Using glue syntax in packages

Since rlang does not depend directly on glue, you will have to ensure
that glue is installed by adding it to your `Imports:` section.

    usethis::use_package("glue", "Imports")
