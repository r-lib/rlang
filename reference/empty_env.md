# Get the empty environment

The empty environment is the only one that does not have a parent. It is
always used as the tail of an environment chain such as the search path
(see
[`search_envs()`](https://rlang.r-lib.org/reference/search_envs.md)).

## Usage

``` r
empty_env()
```

## Examples

``` r
# Create environments with nothing in scope:
child_env(empty_env())
#> <environment: 0x55779faea470>
```
