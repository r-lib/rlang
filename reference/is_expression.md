# Is an object an expression?

In rlang, an *expression* is the return type of
[`parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.md), the
set of objects that can be obtained from parsing R code. Under this
definition expressions include numbers, strings, `NULL`, symbols, and
function calls. These objects can be classified as:

- Symbolic objects, i.e. symbols and function calls (for which
  `is_symbolic()` returns `TRUE`)

- Syntactic literals, i.e. scalar atomic objects and `NULL` (testable
  with `is_syntactic_literal()`)

`is_expression()` returns `TRUE` if the input is either a symbolic
object or a syntactic literal. If a call, the elements of the call must
all be expressions as well. Unparsable calls are not considered
expressions in this narrow definition.

Note that in base R, there exists
[`expression()`](https://rdrr.io/r/base/expression.html) vectors, a data
type similar to a list that supports special attributes created by the
parser called source references. This data type is not supported in
rlang.

## Usage

``` r
is_expression(x)

is_syntactic_literal(x)

is_symbolic(x)
```

## Arguments

- x:

  An object to test.

## Details

`is_symbolic()` returns `TRUE` for symbols and calls (objects with type
`language`). Symbolic objects are replaced by their value during
evaluation. Literals are the complement of symbolic objects. They are
their own value and return themselves during evaluation.

`is_syntactic_literal()` is a predicate that returns `TRUE` for the
subset of literals that are created by R when parsing text (see
[`parse_expr()`](https://rlang.r-lib.org/reference/parse_expr.md)):
numbers, strings and `NULL`. Along with symbols, these literals are the
terminating nodes in an AST.

Note that in the most general sense, a literal is any R object that
evaluates to itself and that can be evaluated in the empty environment.
For instance, `quote(c(1, 2))` is not a literal, it is a call. However,
the result of evaluating it in
[`base_env()`](https://rlang.r-lib.org/reference/search_envs.md) is a
literal(in this case an atomic vector).

As the data structure for function arguments, pairlists are also a kind
of language objects. However, since they are mostly an internal data
structure and can't be returned as is by the parser, `is_expression()`
returns `FALSE` for pairlists.

## See also

[`is_call()`](https://rlang.r-lib.org/reference/is_call.md) for a call
predicate.

## Examples

``` r
q1 <- quote(1)
is_expression(q1)
#> [1] TRUE
is_syntactic_literal(q1)
#> [1] TRUE

q2 <- quote(x)
is_expression(q2)
#> [1] TRUE
is_symbol(q2)
#> [1] TRUE

q3 <- quote(x + 1)
is_expression(q3)
#> [1] TRUE
is_call(q3)
#> [1] TRUE


# Atomic expressions are the terminating nodes of a call tree:
# NULL or a scalar atomic vector:
is_syntactic_literal("string")
#> [1] TRUE
is_syntactic_literal(NULL)
#> [1] TRUE

is_syntactic_literal(letters)
#> [1] FALSE
is_syntactic_literal(quote(call()))
#> [1] FALSE

# Parsable literals have the property of being self-quoting:
identical("foo", quote("foo"))
#> [1] TRUE
identical(1L, quote(1L))
#> [1] TRUE
identical(NULL, quote(NULL))
#> [1] TRUE

# Like any literals, they can be evaluated within the empty
# environment:
eval_bare(quote(1L), empty_env())
#> [1] 1

# Whereas it would fail for symbolic expressions:
# eval_bare(quote(c(1L, 2L)), empty_env())


# Pairlists are also language objects representing argument lists.
# You will usually encounter them with extracted formals:
fmls <- formals(is_expression)
typeof(fmls)
#> [1] "pairlist"

# Since they are mostly an internal data structure, is_expression()
# returns FALSE for pairlists, so you will have to check explicitly
# for them:
is_expression(fmls)
#> [1] FALSE
is_pairlist(fmls)
#> [1] TRUE
```
