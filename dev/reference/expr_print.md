# Print an expression

`expr_print()`, powered by `expr_deparse()`, is an alternative printer
for R expressions with a few improvements over the base R printer.

- It colourises
  [quosures](https://rlang.r-lib.org/dev/reference/topic-defuse.md)
  according to their environment. Quosures from the global environment
  are printed normally while quosures from local environments are
  printed in unique colour (or in italic when all colours are taken).

- It wraps inlined objects in angular brackets. For instance, an integer
  vector unquoted in a function call (e.g. `expr(foo(!!(1:3)))`) is
  printed like this: `foo(<int: 1L, 2L, 3L>)` while by default R prints
  the code to create that vector: `foo(1:3)` which is ambiguous.

- It respects the width boundary (from the global option `width`) in
  more cases.

## Usage

``` r
expr_print(x, ...)

expr_deparse(x, ..., width = peek_option("width"))
```

## Arguments

- x:

  An object or expression to print.

- ...:

  Arguments passed to `expr_deparse()`.

- width:

  The width of the deparsed or printed expression. Defaults to the
  global option `width`.

## Value

`expr_deparse()` returns a character vector of lines. `expr_print()`
returns its input invisibly.

## Examples

``` r
# It supports any object. Non-symbolic objects are always printed
# within angular brackets:
expr_print(1:3)
#> <int: 1L, 2L, 3L>
expr_print(function() NULL)
#> <function() NULL>

# Contrast this to how the code to create these objects is printed:
expr_print(quote(1:3))
#> 1:3
expr_print(quote(function() NULL))
#> function() NULL

# The main cause of non-symbolic objects in expressions is
# quasiquotation:
expr_print(expr(foo(!!(1:3))))
#> foo(<int: 1L, 2L, 3L>)


# Quosures from the global environment are printed normally:
expr_print(quo(foo))
#> ^foo
expr_print(quo(foo(!!quo(bar))))
#> ^foo(^bar)

# Quosures from local environments are colourised according to
# their environments (if you have crayon installed):
local_quo <- local(quo(foo))
expr_print(local_quo)
#> ^foo

wrapper_quo <- local(quo(bar(!!local_quo, baz)))
expr_print(wrapper_quo)
#> ^bar(^foo, baz)
```
