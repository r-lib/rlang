# Get or set the environment of an object

These functions dispatch internally with methods for functions, formulas
and frames. If called with a missing argument, the environment of the
current evaluation frame is returned. If you call `get_env()` with an
environment, it acts as the identity function and the environment is
simply returned (this helps simplifying code when writing generic
functions for environments).

## Usage

``` r
get_env(env, default = NULL)

set_env(env, new_env = caller_env())

env_poke_parent(env, new_env)
```

## Arguments

- env:

  An environment.

- default:

  The default environment in case `env` does not wrap an environment. If
  `NULL` and no environment could be extracted, an error is issued.

- new_env:

  An environment to replace `env` with.

## Details

While `set_env()` returns a modified copy and does not have side
effects, `env_poke_parent()` operates changes the environment by side
effect. This is because environments are
[uncopyable](https://rlang.r-lib.org/dev/reference/is_copyable.md). Be
careful not to change environments that you don't own, e.g. a parent
environment of a function from a package.

## See also

[`quo_get_env()`](https://rlang.r-lib.org/dev/reference/quosure-tools.md)
and
[`quo_set_env()`](https://rlang.r-lib.org/dev/reference/quosure-tools.md)
for versions of `get_env()` and `set_env()` that only work on quosures.

## Examples

``` r
# Environment of closure functions:
fn <- function() "foo"
get_env(fn)
#> <environment: 0x55eb88d954a8>

# Or of quosures or formulas:
get_env(~foo)
#> <environment: 0x55eb88d954a8>
get_env(quo(foo))
#> <environment: 0x55eb88d954a8>


# Provide a default in case the object doesn't bundle an environment.
# Let's create an unevaluated formula:
f <- quote(~foo)

# The following line would fail if run because unevaluated formulas
# don't bundle an environment (they didn't have the chance to
# record one yet):
# get_env(f)

# It is often useful to provide a default when you're writing
# functions accepting formulas as input:
default <- env()
identical(get_env(f, default), default)
#> [1] TRUE

# set_env() can be used to set the enclosure of functions and
# formulas. Let's create a function with a particular environment:
env <- child_env("base")
fn <- set_env(function() NULL, env)

# That function now has `env` as enclosure:
identical(get_env(fn), env)
#> [1] TRUE
identical(get_env(fn), current_env())
#> [1] FALSE

# set_env() does not work by side effect. Setting a new environment
# for fn has no effect on the original function:
other_env <- child_env(NULL)
set_env(fn, other_env)
#> function () 
#> NULL
#> <environment: 0x55eb8a5e6768>
identical(get_env(fn), other_env)
#> [1] FALSE

# Since set_env() returns a new function with a different
# environment, you'll need to reassign the result:
fn <- set_env(fn, other_env)
identical(get_env(fn), other_env)
#> [1] TRUE
```
