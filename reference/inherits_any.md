# Does an object inherit from a set of classes?

- `inherits_any()` is like
  [`base::inherits()`](https://rdrr.io/r/base/class.html) but is more
  explicit about its behaviour with multiple classes. If `classes`
  contains several elements and the object inherits from at least one of
  them, `inherits_any()` returns `TRUE`.

- `inherits_all()` tests that an object inherits from all of the classes
  in the supplied order. This is usually the best way to test for
  inheritance of multiple classes.

- `inherits_only()` tests that the class vectors are identical. It is a
  shortcut for `identical(class(x), class)`.

## Usage

``` r
inherits_any(x, class)

inherits_all(x, class)

inherits_only(x, class)
```

## Arguments

- x:

  An object to test for inheritance.

- class:

  A character vector of classes.

## Examples

``` r
obj <- structure(list(), class = c("foo", "bar", "baz"))

# With the _any variant only one class must match:
inherits_any(obj, c("foobar", "bazbaz"))
#> [1] FALSE
inherits_any(obj, c("foo", "bazbaz"))
#> [1] TRUE

# With the _all variant all classes must match:
inherits_all(obj, c("foo", "bazbaz"))
#> [1] FALSE
inherits_all(obj, c("foo", "baz"))
#> [1] TRUE

# The order of classes must match as well:
inherits_all(obj, c("baz", "foo"))
#> [1] FALSE

# inherits_only() checks that the class vectors are identical:
inherits_only(obj, c("foo", "baz"))
#> [1] FALSE
inherits_only(obj, c("foo", "bar", "baz"))
#> [1] TRUE
```
