# Return the namespace registry env

Note that the namespace registry does not behave like a normal
environment because the parent is `NULL` instead of the empty
environment. This is exported for expert usage in development tools
only.

## Usage

``` r
ns_registry_env()
```
