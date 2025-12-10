# Search path environments

The search path is a chain of environments containing exported functions
of attached packages.

The API includes:

- [`base::search()`](https://rdrr.io/r/base/search.html) to get the
  names of environments attached to the search path.

- `search_envs()` returns the environments on the search path as a list.

- `pkg_env_name()` takes a bare package name and prefixes it with
  `"package:"`. Attached package environments have search names of the
  form `package:name`.

- `pkg_env()` takes a bare package name and returns the scoped
  environment of packages if they are attached to the search path, and
  throws an error otherwise. It is a shortcut for
  `search_env(pkg_env_name("pkgname"))`.

- `global_env()` and `base_env()` (simple aliases for
  [`globalenv()`](https://rdrr.io/r/base/environment.html) and
  [`baseenv()`](https://rdrr.io/r/base/environment.html)). These are
  respectively the first and last environments of the search path.

- `is_attached()` returns `TRUE` when its argument (a search name or a
  package environment) is attached to the search path.

## Usage

``` r
search_envs()

search_env(name)

pkg_env(pkg)

pkg_env_name(pkg)

is_attached(x)

base_env()

global_env()
```

## Arguments

- name:

  The name of an environment attached to the search path. Call
  [`base::search()`](https://rdrr.io/r/base/search.html) to get the
  names of environments currently attached to the search path. Note that
  the search name of a package environment is prefixed with
  `"package:"`.

- pkg:

  The name of a package.

- x:

  An environment or a search name.

## The search path

This chain of environments determines what objects are visible from the
global workspace. It contains the following elements:

- The chain always starts with `global_env()` and finishes with
  `base_env()` which inherits from the terminal environment
  [`empty_env()`](https://rlang.r-lib.org/reference/empty_env.md).

- Each [`base::library()`](https://rdrr.io/r/base/library.html) call
  attaches a new package environment to the search path. Attached
  packages are associated with a [search
  name](https://rlang.r-lib.org/reference/env_name.md).

- In addition, any list, data frame, or environment can be attached to
  the search path with
  [`base::attach()`](https://rdrr.io/r/base/attach.html).

## Examples

``` r
# List the search names of environments attached to the search path:
search()
#>  [1] ".GlobalEnv"        "package:rlang"     "package:stats"    
#>  [4] "package:graphics"  "package:grDevices" "package:utils"    
#>  [7] "package:datasets"  "package:methods"   "Autoloads"        
#> [10] "package:base"     

# Get the corresponding environments:
search_envs()
#>  [[1]] $ <env: global>
#>  [[2]] $ <env: package:rlang>
#>  [[3]] $ <env: package:stats>
#>  [[4]] $ <env: package:graphics>
#>  [[5]] $ <env: package:grDevices>
#>  [[6]] $ <env: package:utils>
#>  [[7]] $ <env: package:datasets>
#>  [[8]] $ <env: package:methods>
#>  [[9]] $ <env: Autoloads>
#> [[10]] $ <env: package:base>

# The global environment and the base package are always first and
# last in the chain, respectively:
envs <- search_envs()
envs[[1]]
#> <environment: R_GlobalEnv>
envs[[length(envs)]]
#> <environment: base>

# These two environments have their own shortcuts:
global_env()
#> <environment: R_GlobalEnv>
base_env()
#> <environment: base>

# Packages appear in the search path with a special name. Use
# pkg_env_name() to create that name:
pkg_env_name("rlang")
#> [1] "package:rlang"
search_env(pkg_env_name("rlang"))
#> <environment: package:rlang>
#> attr(,"name")
#> [1] "package:rlang"
#> attr(,"path")
#> [1] "/home/runner/work/_temp/Library/rlang"

# Alternatively, get the scoped environment of a package with
# pkg_env():
pkg_env("utils")
#> <environment: package:utils>
#> attr(,"name")
#> [1] "package:utils"
#> attr(,"path")
#> [1] "/opt/R/4.5.2/lib/R/library/utils"
```
