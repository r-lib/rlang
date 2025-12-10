# Is an object callable?

A callable object is an object that can appear in the function position
of a call (as opposed to argument position). This includes [symbolic
objects](https://rlang.r-lib.org/dev/reference/is_expression.md) that
evaluate to a function or literal functions embedded in the call.

## Usage

``` r
is_callable(x)
```

## Arguments

- x:

  An object to test.

## Details

Note that strings may look like callable objects because expressions of
the form [`"list"()`](https://rdrr.io/r/base/list.html) are valid R
code. However, that's only because the R parser transforms strings to
symbols. It is not legal to manually set language heads to strings.

## Examples

``` r
# Symbolic objects and functions are callable:
is_callable(quote(foo))
#> [1] TRUE
is_callable(base::identity)
#> [1] TRUE

# node_poke_car() lets you modify calls without any checking:
lang <- quote(foo(10))
node_poke_car(lang, current_env())

# Use is_callable() to check an input object is safe to put as CAR:
obj <- base::identity

if (is_callable(obj)) {
  lang <- node_poke_car(lang, obj)
} else {
  abort("`obj` must be callable")
}

eval_bare(lang)
#> [1] 10
```
