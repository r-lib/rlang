# Is an object referencing another?

There are typically two situations where two symbols may refer to the
same object.

- R objects usually have copy-on-write semantics. This is an
  optimisation that ensures that objects are only copied if needed. When
  you copy a vector, no memory is actually copied until you modify
  either the original object or the copy is modified.

  Note that the copy-on-write optimisation is an implementation detail
  that is not guaranteed by the specification of the R language.

- Assigning an
  [uncopyable](https://rlang.r-lib.org/reference/is_copyable.md) object
  (like an environment) creates a reference. These objects are never
  copied even if you modify one of the references.

## Usage

``` r
is_reference(x, y)
```

## Arguments

- x, y:

  R objects.

## Examples

``` r
# Reassigning an uncopyable object such as an environment creates a
# reference:
env <- env()
ref <- env
is_reference(ref, env)
#> [1] TRUE

# Due to copy-on-write optimisation, a copied vector can
# temporarily reference the original vector:
vec <- 1:10
copy <- vec
is_reference(copy, vec)
#> [1] TRUE

# Once you modify on of them, the copy is triggered in the
# background and the objects cease to reference each other:
vec[[1]] <- 100
is_reference(copy, vec)
#> [1] FALSE
```
