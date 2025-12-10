# Create a new environment

These functions create new environments.

- `env()` creates a child of the current environment by default and
  takes a variable number of named objects to populate it.

- `new_environment()` creates a child of the empty environment by
  default and takes a named list of objects to populate it.

## Usage

``` r
env(...)

new_environment(data = list(), parent = empty_env())
```

## Arguments

- ..., data:

  \<[dynamic](https://rlang.r-lib.org/dev/reference/dyn-dots.md)\> Named
  values. You can supply one unnamed to specify a custom parent,
  otherwise it defaults to the current environment.

- parent:

  A parent environment.

## Environments as objects

Environments are containers of uniquely named objects. Their most common
use is to provide a scope for the evaluation of R expressions. Not all
languages have first class environments, i.e. can manipulate scope as
regular objects. Reification of scope is one of the most powerful
features of R as it allows you to change what objects a function or
expression sees when it is evaluated.

Environments also constitute a data structure in their own right. They
are a collection of uniquely named objects, subsettable by name and
modifiable by reference. This latter property (see section on reference
semantics) is especially useful for creating mutable OO systems (cf the
[R6 package](https://github.com/r-lib/R6) and the [ggproto
system](https://ggplot2.tidyverse.org/articles/extending-ggplot2.html)
for extending ggplot2).

## Inheritance

All R environments (except the [empty
environment](https://rlang.r-lib.org/dev/reference/empty_env.md)) are
defined with a parent environment. An environment and its grandparents
thus form a linear hierarchy that is the basis for [lexical
scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)) in R.
When R evaluates an expression, it looks up symbols in a given
environment. If it cannot find these symbols there, it keeps looking
them up in parent environments. This way, objects defined in child
environments have precedence over objects defined in parent
environments.

The ability of overriding specific definitions is used in the tidyeval
framework to create powerful domain-specific grammars. A common use of
masking is to put data frame columns in scope. See for example
[`as_data_mask()`](https://rlang.r-lib.org/dev/reference/as_data_mask.md).

## Reference semantics

Unlike regular objects such as vectors, environments are an
[uncopyable](https://rlang.r-lib.org/dev/reference/is_copyable.md)
object type. This means that if you have multiple references to a given
environment (by assigning the environment to another symbol with `<-` or
passing the environment as argument to a function), modifying the
bindings of one of those references changes all other references as
well.

## See also

[`env_has()`](https://rlang.r-lib.org/dev/reference/env_has.md),
[`env_bind()`](https://rlang.r-lib.org/dev/reference/env_bind.md).

## Examples

``` r
# env() creates a new environment that inherits from the current
# environment by default
env <- env(a = 1, b = "foo")
env$b
#> [1] "foo"
identical(env_parent(env), current_env())
#> [1] TRUE

# Supply one unnamed argument to inherit from another environment:
env <- env(base_env(), a = 1, b = "foo")
identical(env_parent(env), base_env())
#> [1] TRUE


# Both env() and child_env() support tidy dots features:
objs <- list(b = "foo", c = "bar")
env <- env(a = 1, !!! objs)
env$c
#> [1] "bar"

# You can also unquote names with the definition operator `:=`
var <- "a"
env <- env(!!var := "A")
env$a
#> [1] "A"


# Use new_environment() to create containers with the empty
# environment as parent:
env <- new_environment()
env_parent(env)
#> <environment: R_EmptyEnv>

# Like other new_ constructors, it takes an object rather than dots:
new_environment(list(a = "foo", b = "bar"))
#> <environment: 0x5570b0403bf0>
```
