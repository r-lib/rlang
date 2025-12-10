# Bind symbols to objects in an environment

These functions create bindings in an environment. The bindings are
supplied through `...` as pairs of names and values or expressions.
`env_bind()` is equivalent to evaluating a `<-` expression within the
given environment. This function should take care of the majority of use
cases but the other variants can be useful for specific problems.

- `env_bind()` takes named *values* which are bound in `.env`.
  `env_bind()` is equivalent to
  [`base::assign()`](https://rdrr.io/r/base/assign.html).

- `env_bind_active()` takes named *functions* and creates active
  bindings in `.env`. This is equivalent to
  [`base::makeActiveBinding()`](https://rdrr.io/r/base/bindenv.html). An
  active binding executes a function each time it is evaluated. The
  arguments are passed to
  [`as_function()`](https://rlang.r-lib.org/reference/as_function.md) so
  you can supply formulas instead of functions.

  Remember that functions are scoped in their own environment. These
  functions can thus refer to symbols from this enclosure that are not
  actually in scope in the dynamic environment where the active bindings
  are invoked. This allows creative solutions to difficult problems (see
  the implementations of
  [`dplyr::do()`](https://dplyr.tidyverse.org/reference/do.html) methods
  for an example).

- `env_bind_lazy()` takes named *expressions*. This is equivalent to
  [`base::delayedAssign()`](https://rdrr.io/r/base/delayedAssign.html).
  The arguments are captured with
  [`exprs()`](https://rlang.r-lib.org/reference/defusing-advanced.md)
  (and thus support call-splicing and unquoting) and assigned to symbols
  in `.env`. These expressions are not evaluated immediately but lazily.
  Once a symbol is evaluated, the corresponding expression is evaluated
  in turn and its value is bound to the symbol (the expressions are thus
  evaluated only once, if at all).

- `%<~%` is a shortcut for `env_bind_lazy()`. It works like `<-` but the
  RHS is evaluated lazily.

## Usage

``` r
env_bind(.env, ...)

env_bind_lazy(.env, ..., .eval_env = caller_env())

env_bind_active(.env, ...)

lhs %<~% rhs
```

## Arguments

- .env:

  An environment.

- ...:

  \<[dynamic](https://rlang.r-lib.org/reference/dyn-dots.md)\> Named
  objects (`env_bind()`), expressions `env_bind_lazy()`, or functions
  (`env_bind_active()`). Use
  [`zap()`](https://rlang.r-lib.org/reference/zap.md) to remove
  bindings.

- .eval_env:

  The environment where the expressions will be evaluated when the
  symbols are forced.

- lhs:

  The variable name to which `rhs` will be lazily assigned.

- rhs:

  An expression lazily evaluated and assigned to `lhs`.

## Value

The input object `.env`, with its associated environment modified in
place, invisibly.

## Side effects

Since environments have reference semantics (see relevant section in
[`env()`](https://rlang.r-lib.org/reference/env.md) documentation),
modifying the bindings of an environment produces effects in all other
references to that environment. In other words, `env_bind()` and its
variants have side effects.

Like other side-effecty functions like
[`par()`](https://rdrr.io/r/graphics/par.html) and
[`options()`](https://rdrr.io/r/base/options.html), `env_bind()` and
variants return the old values invisibly.

## See also

[`env_poke()`](https://rlang.r-lib.org/reference/env_poke.md) for
binding a single element.

## Examples

``` r
# env_bind() is a programmatic way of assigning values to symbols
# with `<-`. We can add bindings in the current environment:
env_bind(current_env(), foo = "bar")
foo
#> [1] "bar"

# Or modify those bindings:
bar <- "bar"
env_bind(current_env(), bar = "BAR")
bar
#> [1] "BAR"

# You can remove bindings by supplying zap sentinels:
env_bind(current_env(), foo = zap())
try(foo)
#> Error in eval(expr, envir) : object 'foo' not found

# Unquote-splice a named list of zaps
zaps <- rep_named(c("foo", "bar"), list(zap()))
env_bind(current_env(), !!!zaps)
try(bar)
#> Error in eval(expr, envir) : object 'bar' not found

# It is most useful to change other environments:
my_env <- env()
env_bind(my_env, foo = "foo")
my_env$foo
#> [1] "foo"

# A useful feature is to splice lists of named values:
vals <- list(a = 10, b = 20)
env_bind(my_env, !!!vals, c = 30)
my_env$b
#> [1] 20
my_env$c
#> [1] 30

# You can also unquote a variable referring to a symbol or a string
# as binding name:
var <- "baz"
env_bind(my_env, !!var := "BAZ")
my_env$baz
#> [1] "BAZ"


# The old values of the bindings are returned invisibly:
old <- env_bind(my_env, a = 1, b = 2, baz = "baz")
old
#> $a
#> [1] 10
#> 
#> $b
#> [1] 20
#> 
#> $baz
#> [1] "BAZ"
#> 

# You can restore the original environment state by supplying the
# old values back:
env_bind(my_env, !!!old)

# env_bind_lazy() assigns expressions lazily:
env <- env()
env_bind_lazy(env, name = { cat("forced!\n"); "value" })

# Referring to the binding will cause evaluation:
env$name
#> forced!
#> [1] "value"

# But only once, subsequent references yield the final value:
env$name
#> [1] "value"

# You can unquote expressions:
expr <- quote(message("forced!"))
env_bind_lazy(env, name = !!expr)
env$name
#> forced!
#> NULL


# By default the expressions are evaluated in the current
# environment. For instance we can create a local binding and refer
# to it, even though the variable is bound in a different
# environment:
who <- "mickey"
env_bind_lazy(env, name = paste(who, "mouse"))
env$name
#> [1] "mickey mouse"

# You can specify another evaluation environment with `.eval_env`:
eval_env <- env(who = "minnie")
env_bind_lazy(env, name = paste(who, "mouse"), .eval_env = eval_env)
env$name
#> [1] "minnie mouse"

# Or by unquoting a quosure:
quo <- local({
  who <- "fievel"
  quo(paste(who, "mouse"))
})
env_bind_lazy(env, name = !!quo)
env$name
#> [1] "fievel mouse"

# You can create active bindings with env_bind_active(). Active
# bindings execute a function each time they are evaluated:
fn <- function() {
  cat("I have been called\n")
  rnorm(1)
}

env <- env()
env_bind_active(env, symbol = fn)

# `fn` is executed each time `symbol` is evaluated or retrieved:
env$symbol
#> I have been called
#> [1] -0.0499649
env$symbol
#> I have been called
#> [1] -0.2514834
eval_bare(quote(symbol), env)
#> I have been called
#> [1] 0.4447971
eval_bare(quote(symbol), env)
#> I have been called
#> [1] 2.755418

# All arguments are passed to as_function() so you can use the
# formula shortcut:
env_bind_active(env, foo = ~ runif(1))
env$foo
#> [1] 0.5185566
env$foo
#> [1] 0.8461201
```
