# Create a data mask

A [data mask](https://rlang.r-lib.org/dev/reference/topic-data-mask.md)
is an environment (or possibly multiple environments forming an
ancestry) containing user-supplied objects. Objects in the mask have
precedence over objects in the environment (i.e. they mask those
objects). Many R functions evaluate quoted expressions in a data mask so
these expressions can refer to objects within the user data.

These functions let you construct a tidy eval data mask manually. They
are meant for developers of tidy eval interfaces rather than for end
users.

## Usage

``` r
as_data_mask(data)

as_data_pronoun(data)

new_data_mask(bottom, top = bottom)
```

## Arguments

- data:

  A data frame or named vector of masking data.

- bottom:

  The environment containing masking objects if the data mask is one
  environment deep. The bottom environment if the data mask comprises
  multiple environment.

  If you haven't supplied `top`, this **must** be an environment that
  you own, i.e. that you have created yourself.

- top:

  The last environment of the data mask. If the data mask is only one
  environment deep, `top` should be the same as `bottom`.

  This **must** be an environment that you own, i.e. that you have
  created yourself. The parent of `top` will be changed by the tidy eval
  engine and should be considered undetermined. Never make assumption
  about the parent of `top`.

## Value

A data mask that you can supply to
[`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md).

## Why build a data mask?

Most of the time you can just call
[`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md) with
a list or a data frame and the data mask will be constructed
automatically. There are three main use cases for manual creation of
data masks:

- When
  [`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md) is
  called with the same data in a tight loop. Because there is some
  overhead to creating tidy eval data masks, constructing the mask once
  and reusing it for subsequent evaluations may improve performance.

- When several expressions should be evaluated in the exact same
  environment because a quoted expression might create new objects that
  can be referred in other quoted expressions evaluated at a later time.
  One example of this is
  [`tibble::lst()`](https://tibble.tidyverse.org/reference/lst.html)
  where new columns can refer to previous ones.

- When your data mask requires special features. For instance the data
  frame columns in dplyr data masks are implemented with [active
  bindings](https://rdrr.io/r/base/delayedAssign.html).

## Building your own data mask

Unlike [`base::eval()`](https://rdrr.io/r/base/eval.html) which takes
any kind of environments as data mask,
[`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md) has
specific requirements in order to support
[quosures](https://rlang.r-lib.org/dev/reference/topic-defuse.md). For
this reason you can't supply bare environments.

There are two ways of constructing an rlang data mask manually:

- `as_data_mask()` transforms a list or data frame to a data mask. It
  automatically installs the data pronoun
  [`.data`](https://rlang.r-lib.org/dev/reference/dot-data.md).

- `new_data_mask()` is a bare bones data mask constructor for
  environments. You can supply a bottom and a top environment in case
  your data mask comprises multiple environments (see section below).

  Unlike `as_data_mask()` it does not install the `.data` pronoun so you
  need to provide one yourself. You can provide a pronoun constructed
  with `as_data_pronoun()` or your own pronoun class.

  `as_data_pronoun()` will create a pronoun from a list, an environment,
  or an rlang data mask. In the latter case, the whole ancestry is
  looked up from the bottom to the top of the mask. Functions stored in
  the mask are bypassed by the pronoun.

Once you have built a data mask, simply pass it to
[`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md) as
the `data` argument. You can repeat this as many times as needed. Note
that any objects created there (perhaps because of a call to `<-`) will
persist in subsequent evaluations.

## Top and bottom of data mask

In some cases you'll need several levels in your data mask. One good
reason is when you include functions in the mask. It's a good idea to
keep data objects one level lower than function objects, so that the
former cannot override the definitions of the latter (see examples).

In that case, set up all your environments and keep track of the bottom
child and the top parent. You'll need to pass both to `new_data_mask()`.

Note that the parent of the top environment is completely undetermined,
you shouldn't expect it to remain the same at all times. This parent is
replaced during evaluation by
[`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md) to
one of the following environments:

- The default environment passed as the `env` argument of
  [`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md).

- The environment of the current quosure being evaluated, if applicable.

Consequently, all masking data should be contained between the bottom
and top environment of the data mask.

## Examples

``` r
# Evaluating in a tidy evaluation environment enables all tidy
# features:
mask <- as_data_mask(mtcars)
eval_tidy(quo(letters), mask)
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"

# You can install new pronouns in the mask:
mask$.pronoun <- as_data_pronoun(list(foo = "bar", baz = "bam"))
eval_tidy(quo(.pronoun$foo), mask)
#> [1] "bar"

# In some cases the data mask can leak to the user, for example if
# a function or formula is created in the data mask environment:
cyl <- "user variable from the context"
fn <- eval_tidy(quote(function() cyl), mask)
fn()
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4

# If new objects are created in the mask, they persist in the
# subsequent calls:
eval_tidy(quote(new <- cyl + am), mask)
eval_tidy(quote(new * 2), mask)
#>  [1] 14 14 10 12 16 12 16  8  8 12 12 16 16 16 16 16 16 10 10 10  8 16
#> [23] 16 16 16 10 10 10 18 14 18 10


# In some cases your data mask is a whole chain of environments
# rather than a single environment. You'll have to use
# `new_data_mask()` and let it know about the bottom of the mask
# (the last child of the environment chain) and the topmost parent.

# A common situation where you'll want a multiple-environment mask
# is when you include functions in your mask. In that case you'll
# put functions in the top environment and data in the bottom. This
# will prevent the data from overwriting the functions.
top <- new_environment(list(`+` = base::paste, c = base::paste))

# Let's add a middle environment just for sport:
middle <- env(top)

# And finally the bottom environment containing data:
bottom <- env(middle, a = "a", b = "b", c = "c")

# We can now create a mask by supplying the top and bottom
# environments:
mask <- new_data_mask(bottom, top = top)

# This data mask can be passed to eval_tidy() instead of a list or
# data frame:
eval_tidy(quote(a + b + c), data = mask)
#> [1] "a b c"

# Note how the function `c()` and the object `c` are looked up
# properly because of the multi-level structure:
eval_tidy(quote(c(a, b, c)), data = mask)
#> [1] "a b c"

# new_data_mask() does not create data pronouns, but
# data pronouns can be added manually:
mask$.fns <- as_data_pronoun(top)

# The `.data` pronoun should generally be created from the
# mask. This will ensure data is looked up throughout the whole
# ancestry. Only non-function objects are looked up from this
# pronoun:
mask$.data <- as_data_pronoun(mask)
mask$.data$c
#> [1] "c"

# Now we can reference values with the pronouns:
eval_tidy(quote(c(.data$a, .data$b, .data$c)), data = mask)
#> [1] "a b c"
```
