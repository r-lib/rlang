# Coerce an object to a base type

**\[deprecated\]**

These are equivalent to the base functions (e.g.
[`as.logical()`](https://rdrr.io/r/base/logical.html),
[`as.list()`](https://rdrr.io/r/base/list.html), etc), but perform
coercion rather than conversion. This means they are not generic and
will not call S3 conversion methods. They only attempt to coerce the
base type of their input. In addition, they have stricter implicit
coercion rules and will never attempt any kind of parsing. E.g. they
will not try to figure out if a character vector represents integers or
booleans. Finally, they treat attributes consistently, unlike the base R
functions: all attributes except names are removed.

## Usage

``` r
as_logical(x)

as_integer(x)

as_double(x)

as_complex(x)

as_character(x, encoding = NULL)

as_list(x)
```

## Arguments

- x:

  An object to coerce to a base type.

- encoding:

  If non-null, set an encoding mark. This is only declarative, no
  encoding conversion is performed.

## Lifecycle

These functions are deprecated in favour of
[`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).

## Coercion to logical and numeric atomic vectors

- To logical vectors: Integer and integerish double vectors. See
  [`is_integerish()`](https://rlang.r-lib.org/dev/reference/is_integerish.md).

- To integer vectors: Logical and integerish double vectors.

- To double vectors: Logical and integer vectors.

- To complex vectors: Logical, integer and double vectors.

## Coercion to character vectors

`as_character()` and
[`as_string()`](https://rlang.r-lib.org/dev/reference/as_string.md) have
an optional `encoding` argument to specify the encoding. R uses this
information for internal handling of strings and character vectors. Note
that this is only declarative, no encoding conversion is attempted.

Note that only
[`as_string()`](https://rlang.r-lib.org/dev/reference/as_string.md) can
coerce symbols to a scalar character vector. This makes the code more
explicit and adds an extra type check.

## Coercion to lists

`as_list()` only coerces vector and dictionary types (environments are
an example of dictionary type). Unlike
[`base::as.list()`](https://rdrr.io/r/base/list.html), `as_list()`
removes all attributes except names.

## Effects of removing attributes

A technical side-effect of removing the attributes of the input is that
the underlying objects has to be copied. This has no performance
implications in the case of lists because this is a shallow copy: only
the list structure is copied, not the contents (see
[`duplicate()`](https://rlang.r-lib.org/dev/reference/duplicate.md)).
However, be aware that atomic vectors containing large amounts of data
will have to be copied.

In general, any attribute modification creates a copy, which is why it
is better to avoid using attributes with heavy atomic vectors.
Uncopyable objects like environments and symbols are an exception to
this rule: in this case, attributes modification happens in place and
has side-effects.

## Examples

``` r
# Coercing atomic vectors removes attributes with both base R and rlang:
x <- structure(TRUE, class = "foo", bar = "baz")
as.logical(x)
#> [1] TRUE

# But coercing lists preserves attributes in base R but not rlang:
l <- structure(list(TRUE), class = "foo", bar = "baz")
as.list(l)
#> [[1]]
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "foo"
#> attr(,"bar")
#> [1] "baz"
as_list(l)
#> Warning: `as_list()` is deprecated as of rlang 0.4.0
#> Please use `vctrs::vec_cast()` instead.
#> This warning is displayed once every 8 hours.
#> [[1]]
#> [1] TRUE
#> 

# Implicit conversions are performed in base R but not rlang:
as.logical(l)
#> [1] TRUE
if (FALSE) { # \dontrun{
as_logical(l)
} # }

# Conversion methods are bypassed, making the result of the
# coercion more predictable:
as.list.foo <- function(x) "wrong"
as.list(l)
#> [1] "wrong"
as_list(l)
#> [[1]]
#> [1] TRUE
#> 

# The input is never parsed. E.g. character vectors of numbers are
# not converted to numeric types:
as.integer("33")
#> [1] 33
if (FALSE) { # \dontrun{
as_integer("33")
} # }


# With base R tools there is no way to convert an environment to a
# list without either triggering method dispatch, or changing the
# original environment. as_list() makes it easy:
x <- structure(as_environment(mtcars[1:2]), class = "foobar")
as.list.foobar <- function(x) abort("don't call me")
as_list(x)
#> $cyl
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4
#> 
#> $mpg
#>  [1] 21.0 21.0 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 17.8 16.4 17.3
#> [14] 15.2 10.4 10.4 14.7 32.4 30.4 33.9 21.5 15.5 15.2 13.3 19.2 27.3
#> [27] 26.0 30.4 15.8 19.7 15.0 21.4
#> 
```
