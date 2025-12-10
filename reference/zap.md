# Create zap objects

`zap()` creates a sentinel object that indicates that an object should
be removed. For instance, named zaps instruct
[`env_bind()`](https://rlang.r-lib.org/reference/env_bind.md) and
[`call_modify()`](https://rlang.r-lib.org/reference/call_modify.md) to
remove those objects from the environment or the call.

The advantage of zap objects is that they unambiguously signal the
intent of removing an object. Sentinels like `NULL` or
[`missing_arg()`](https://rlang.r-lib.org/reference/missing_arg.md) are
ambiguous because they represent valid R objects.

## Usage

``` r
zap()

is_zap(x)
```

## Arguments

- x:

  An object to test.

## Examples

``` r
# Create one zap object:
zap()
#> <zap>

# Create a list of zaps:
rep(list(zap()), 3)
#> [[1]]
#> <zap>
#> 
#> [[2]]
#> <zap>
#> 
#> [[3]]
#> <zap>
#> 
rep_named(c("foo", "bar"), list(zap()))
#> $foo
#> <zap>
#> 
#> $bar
#> <zap>
#> 
```
