# Evaluate an expression with quosures and pronoun support

`eval_tidy()` is a variant of
[`base::eval()`](https://rdrr.io/r/base/eval.html) that powers the tidy
evaluation framework. Like [`eval()`](https://rdrr.io/r/base/eval.html)
it accepts user data as argument. Whereas
[`eval()`](https://rdrr.io/r/base/eval.html) simply transforms the data
to an environment, `eval_tidy()` transforms it to a [data
mask](https://rlang.r-lib.org/reference/topic-data-mask.md) with
[`as_data_mask()`](https://rlang.r-lib.org/reference/as_data_mask.md).
Evaluating in a data mask enables the following features:

- [Quosures](https://rlang.r-lib.org/reference/topic-quosure.md).
  Quosures are expressions bundled with an environment. If `data` is
  supplied, objects in the data mask always have precedence over the
  quosure environment, i.e. the data masks the environment.

- [Pronouns](https://rlang.r-lib.org/reference/dot-data.md). If `data`
  is supplied, the `.env` and `.data` pronouns are installed in the data
  mask. `.env` is a reference to the calling environment and `.data`
  refers to the `data` argument. These pronouns are an escape hatch for
  the [data mask
  ambiguity](https://rlang.r-lib.org/reference/topic-data-mask-ambiguity.md)
  problem.

## Usage

``` r
eval_tidy(expr, data = NULL, env = caller_env())
```

## Arguments

- expr:

  An [expression](https://rlang.r-lib.org/reference/topic-defuse.md) or
  [quosure](https://rlang.r-lib.org/reference/topic-quosure.md) to
  evaluate.

- data:

  A data frame, or named list or vector. Alternatively, a data mask
  created with
  [`as_data_mask()`](https://rlang.r-lib.org/reference/as_data_mask.md)
  or
  [`new_data_mask()`](https://rlang.r-lib.org/reference/as_data_mask.md).
  Objects in `data` have priority over those in `env`. See the section
  about data masking.

- env:

  The environment in which to evaluate `expr`. This environment is not
  applicable for quosures because they have their own environments.

## When should eval_tidy() be used instead of eval()?

[`base::eval()`](https://rdrr.io/r/base/eval.html) is sufficient for
simple evaluation. Use `eval_tidy()` when you'd like to support
expressions referring to the `.data` pronoun, or when you need to
support quosures.

If you're evaluating an expression captured with
[injection](https://rlang.r-lib.org/reference/topic-inject.md) support,
it is recommended to use `eval_tidy()` because users may inject
quosures.

Note that unwrapping a quosure with
[`quo_get_expr()`](https://rlang.r-lib.org/reference/quosure-tools.md)
does not guarantee that there is no quosures inside the expression.
Quosures might be unquoted anywhere in the expression tree. For
instance, the following does not work reliably in the presence of nested
quosures:

    my_quoting_fn <- function(x) {
      x <- enquo(x)
      expr <- quo_get_expr(x)
      env <- quo_get_env(x)
      eval(expr, env)
    }

    # Works:
    my_quoting_fn(toupper(letters))

    # Fails because of a nested quosure:
    my_quoting_fn(toupper(!!quo(letters)))

## Stack semantics of `eval_tidy()`

`eval_tidy()` always evaluates in a data mask, even when `data` is
`NULL`. Because of this, it has different stack semantics than
[`base::eval()`](https://rdrr.io/r/base/eval.html):

- Lexical side effects, such as assignment with `<-`, occur in the mask
  rather than `env`.

- Functions that require the evaluation environment to correspond to a
  frame on the call stack do not work. This is why
  [`return()`](https://rdrr.io/r/base/function.html) called from a
  quosure does not work.

- The mask environment creates a new branch in the tree representation
  of backtraces (which you can visualise in a
  [`browser()`](https://rdrr.io/r/base/browser.html) session with
  `lobstr::cst()`).

See also [`eval_bare()`](https://rlang.r-lib.org/reference/eval_bare.md)
for more information about these differences.

## See also

- [What is data-masking and why do I need
  {{?](https://rlang.r-lib.org/reference/topic-data-mask.md).

- [What are quosures and when are they
  needed?](https://rlang.r-lib.org/reference/topic-quosure.md).

- [Defusing R
  expressions](https://rlang.r-lib.org/reference/topic-defuse.md).

- [`new_data_mask()`](https://rlang.r-lib.org/reference/as_data_mask.md)
  and
  [`as_data_mask()`](https://rlang.r-lib.org/reference/as_data_mask.md)
  for manually creating data masks.

## Examples

``` r
# With simple defused expressions eval_tidy() works the same way as
# eval():
fruit <- "apple"
vegetable <- "potato"
expr <- quote(paste(fruit, vegetable, sep = " or "))
expr
#> paste(fruit, vegetable, sep = " or ")

eval(expr)
#> [1] "apple or potato"
eval_tidy(expr)
#> [1] "apple or potato"

# Both accept a data mask as argument:
data <- list(fruit = "banana", vegetable = "carrot")
eval(expr, data)
#> [1] "banana or carrot"
eval_tidy(expr, data)
#> [1] "banana or carrot"

# The main difference is that eval_tidy() supports quosures:
with_data <- function(data, expr) {
  quo <- enquo(expr)
  eval_tidy(quo, data)
}
with_data(NULL, fruit)
#> [1] "apple"
with_data(data, fruit)
#> [1] "banana"

# eval_tidy() installs the `.data` and `.env` pronouns to allow
# users to be explicit about variable references:
with_data(data, .data$fruit)
#> [1] "banana"
with_data(data, .env$fruit)
#> [1] "apple"
```
