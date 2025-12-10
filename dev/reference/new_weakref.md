# Create a weak reference

A weak reference is a special R object which makes it possible to keep a
reference to an object without preventing garbage collection of that
object. It can also be used to keep data about an object without
preventing GC of the object, similar to WeakMaps in JavaScript.

Objects in R are considered *reachable* if they can be accessed by
following a chain of references, starting from a *root node*; root nodes
are specially-designated R objects, and include the global environment
and base environment. As long as the key is reachable, the value will
not be garbage collected. This is true even if the weak reference object
becomes unreachable. The key effectively prevents the weak reference and
its value from being collected, according to the following chain of
ownership: `weakref <- key -> value`.

When the key becomes unreachable, the key and value in the weak
reference object are replaced by `NULL`, and the finalizer is scheduled
to execute.

## Usage

``` r
new_weakref(key, value = NULL, finalizer = NULL, on_quit = FALSE)
```

## Arguments

- key:

  The key for the weak reference. Must be a reference object – that is,
  an environment or external pointer.

- value:

  The value for the weak reference. This can be `NULL`, if you want to
  use the weak reference like a weak pointer.

- finalizer:

  A function that is run after the key becomes unreachable.

- on_quit:

  Should the finalizer be run when R exits?

## See also

[`is_weakref()`](https://rlang.r-lib.org/dev/reference/is_weakref.md),
[`wref_key()`](https://rlang.r-lib.org/dev/reference/wref_key.md) and
[`wref_value()`](https://rlang.r-lib.org/dev/reference/wref_key.md).

## Examples

``` r
e <- env()

# Create a weak reference to e
w <- new_weakref(e, finalizer = function(e) message("finalized"))

# Get the key object from the weak reference
identical(wref_key(w), e)
#> [1] TRUE

# When the regular reference (the `e` binding) is removed and a GC occurs,
# the weak reference will not keep the object alive.
rm(e)
gc()
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1197325   64    2177654 116.3  2177654 116.3
#> Vcells 2219421   17    8388608  64.0  7260712  55.4
identical(wref_key(w), NULL)
#> [1] TRUE


# A weak reference with a key and value. The value contains data about the
# key.
k <- env()
v <- list(1, 2, 3)
w <- new_weakref(k, v)

identical(wref_key(w), k)
#> [1] TRUE
identical(wref_value(w), v)
#> [1] TRUE

# When v is removed, the weak ref keeps it alive because k is still reachable.
rm(v)
gc()
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1197332   64    2177654 116.3  2177654 116.3
#> Vcells 2219252   17    8388608  64.0  7260712  55.4
identical(wref_value(w), list(1, 2, 3))
#> [1] TRUE

# When k is removed, the weak ref does not keep k or v alive.
rm(k)
gc()
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1197354   64    2177654 116.3  2177654 116.3
#> Vcells 2219290   17    8388608  64.0  7260712  55.4
identical(wref_key(w), NULL)
#> [1] TRUE
identical(wref_value(w), NULL)
#> [1] TRUE
```
