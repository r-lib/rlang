# Is an object copyable?

When an object is modified, R generally copies it (sometimes lazily) to
enforce [value
semantics](https://en.wikipedia.org/wiki/Value_semantics). However, some
internal types are uncopyable. If you try to copy them, either with `<-`
or by argument passing, you actually create references to the original
object rather than actual copies. Modifying these references can thus
have far reaching side effects.

## Usage

``` r
is_copyable(x)
```

## Arguments

- x:

  An object to test.

## Examples

``` r
# Let's add attributes with structure() to uncopyable types. Since
# they are not copied, the attributes are changed in place:
env <- env()
structure(env, foo = "bar")
#> <environment: 0x560f97631550>
#> attr(,"foo")
#> [1] "bar"
env
#> <environment: 0x560f97631550>
#> attr(,"foo")
#> [1] "bar"

# These objects that can only be changed with side effect are not
# copyable:
is_copyable(env)
#> [1] FALSE
```
