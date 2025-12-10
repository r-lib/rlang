# What happens if I use injection operators out of context?

The [injection
operators](https://rlang.r-lib.org/reference/topic-inject.md)
[`{{`](https://rlang.r-lib.org/reference/embrace-operator.md),
[`!!`](https://rlang.r-lib.org/reference/injection-operator.md), and
[`!!!`](https://rlang.r-lib.org/reference/splice-operator.md) are an
extension of the R syntax developed for tidyverse packages. Because they
are not part of base R, they suffer from some limitations. In particular
no specific error is thrown when they are used in unexpected places.

### Using `{{` out of context

The embrace operator
[`{{`](https://rlang.r-lib.org/reference/embrace-operator.md) is a
feature available in
[data-masked](https://rlang.r-lib.org/reference/topic-data-mask.md)
arguments powered by tidy eval. If you use it elsewhere, it is
interpreted as a double `{` wrapping.

In the R language, `{` is like `(` but takes multiple expressions
instead of one:

    {
      1 # Discarded
      2
    }
    #> [1] 2

    list(
      { message("foo"); 2 }
    )
    #> foo
    #> [[1]]
    #> [1] 2

Just like you can wrap an expression in as many parentheses as you'd
like, you can wrap multiple times with braces:

    ((1))
    #> [1] 1

    {{ 2 }}
    #> [1] 2

So nothing prevents you from embracing a function argument in a context
where this operation is not implemented. R will just treat the braces
like a set of parentheses and silently return the result:

    f <- function(arg) list({{ arg }})
    f(1)
    #> [[1]]
    #> [1] 1

This sort of no-effect embracing should be avoided in real code because
it falsely suggests that the function supports the tidy eval operator
and that something special is happening.

However in many cases embracing is done to implement [data
masking](https://rlang.r-lib.org/reference/topic-data-mask.md). It is
likely that the function will be called with data-variables references
which R won't be able to resolve properly:

    my_mean <- function(data, var) {
      with(data, mean({{ var }}))
    }

    my_mean(mtcars, cyl)
    #> Error:
    #> ! object 'cyl' not found

Since [`with()`](https://rdrr.io/r/base/with.html) is a base
data-masking function that doesn't support tidy eval operators, the
embrace operator does not work and we get an object not found error.

### Using `!!` and `!!!` out of context

The injection operators
[`!!`](https://rlang.r-lib.org/reference/injection-operator.md) and
[`!!!`](https://rlang.r-lib.org/reference/splice-operator.md) are
implemented in
[data-masked](https://rlang.r-lib.org/reference/topic-data-mask.md)
arguments, [dynamic
dots](https://rlang.r-lib.org/reference/dyn-dots.md), and within
[`inject()`](https://rlang.r-lib.org/reference/inject.md). When used in
other contexts, they are interpreted by R as double and triple
*negations*.

Double negation can be used in ordinary code to convert an input to
logical:

    !!10
    #> [1] TRUE

    !!0
    #> [1] FALSE

Triple negation is essentially the same as simple negation:

    !10
    #> [1] FALSE

    !!!10
    #> [1] FALSE

This means that when injection operators are used in the wrong place,
they will be interpreted as negation. In the best case scenario you will
get a type error:

    !"foo"
    #> Error in `!"foo"`:
    #> ! invalid argument type

    !quote(foo)
    #> Error in `!quote(foo)`:
    #> ! invalid argument type

    !quote(foo())
    #> Error in `!quote(foo())`:
    #> ! invalid argument type

In the worst case, R will silently convert the input to logical.
Unfortunately there is no systematic way of checking for these errors.
