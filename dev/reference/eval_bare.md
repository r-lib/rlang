# Evaluate an expression in an environment

`eval_bare()` is a lower-level version of function
[`base::eval()`](https://rdrr.io/r/base/eval.html). Technically, it is a
simple wrapper around the C function `Rf_eval()`. You generally don't
need to use `eval_bare()` instead of
[`eval()`](https://rdrr.io/r/base/eval.html). Its main advantage is that
it handles stack-sensitive calls (such as
[`return()`](https://rdrr.io/r/base/function.html),
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) or
[`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)) more
consistently when you pass an enviroment of a frame on the call stack.

## Usage

``` r
eval_bare(expr, env = parent.frame())
```

## Arguments

- expr:

  An expression to evaluate.

- env:

  The environment in which to evaluate the expression.

## Details

These semantics are possible because `eval_bare()` creates only one
frame on the call stack whereas
[`eval()`](https://rdrr.io/r/base/eval.html) creates two frames, the
second of which has the user-supplied environment as frame environment.
When you supply an existing frame environment to
[`base::eval()`](https://rdrr.io/r/base/eval.html) there will be two
frames on the stack with the same frame environment. Stack-sensitive
functions only detect the topmost of these frames. We call these
evaluation semantics "stack inconsistent".

Evaluating expressions in the actual frame environment has useful
practical implications for `eval_bare()`:

- [`return()`](https://rdrr.io/r/base/function.html) calls are evaluated
  in frame environments that might be buried deep in the call stack.
  This causes a long return that unwinds multiple frames (triggering the
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html) event for each
  frame). By contrast [`eval()`](https://rdrr.io/r/base/eval.html) only
  returns from the [`eval()`](https://rdrr.io/r/base/eval.html) call,
  one level up.

- [`on.exit()`](https://rdrr.io/r/base/on.exit.html),
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html),
  [`sys.call()`](https://rdrr.io/r/base/sys.parent.html), and generally
  all the stack inspection functions `sys.xxx()` are evaluated in the
  correct frame environment. This is similar to how this type of calls
  can be evaluated deep in the call stack because of lazy evaluation,
  when you force an argument that has been passed around several times.

The flip side of the semantics of `eval_bare()` is that it can't
evaluate [`break`](https://rdrr.io/r/base/Control.html) or
[`next`](https://rdrr.io/r/base/Control.html) expressions even if called
within a loop.

## See also

[`eval_tidy()`](https://rlang.r-lib.org/dev/reference/eval_tidy.md) for
evaluation with data mask and quosure support.

## Examples

``` r
# eval_bare() works just like base::eval() but you have to create
# the evaluation environment yourself:
eval_bare(quote(foo), env(foo = "bar"))
#> [1] "bar"

# eval() has different evaluation semantics than eval_bare(). It
# can return from the supplied environment even if its an
# environment that is not on the call stack (i.e. because you've
# created it yourself). The following would trigger an error with
# eval_bare():
ret <- quote(return("foo"))
eval(ret, env())
#> [1] "foo"
# eval_bare(ret, env())  # "no function to return from" error

# Another feature of eval() is that you can control surround loops:
bail <- quote(break)
while (TRUE) {
  eval(bail)
  # eval_bare(bail)  # "no loop for break/next" error
}

# To explore the consequences of stack inconsistent semantics, let's
# create a function that evaluates `parent.frame()` deep in the call
# stack, in an environment corresponding to a frame in the middle of
# the stack. For consistency with R's lazy evaluation semantics, we'd
# expect to get the caller of that frame as result:
fn <- function(eval_fn) {
  list(
    returned_env = middle(eval_fn),
    actual_env = current_env()
  )
}
middle <- function(eval_fn) {
  deep(eval_fn, current_env())
}
deep <- function(eval_fn, eval_env) {
  expr <- quote(parent.frame())
  eval_fn(expr, eval_env)
}

# With eval_bare(), we do get the expected environment:
fn(rlang::eval_bare)
#> $returned_env
#> <environment: 0x55f2f6f30848>
#> 
#> $actual_env
#> <environment: 0x55f2f6f30848>
#> 

# But that's not the case with base::eval():
fn(base::eval)
#> $returned_env
#> <environment: 0x55f2f6ed03a0>
#> 
#> $actual_env
#> <environment: 0x55f2f6ecfd80>
#> 
```
