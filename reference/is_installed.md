# Are packages installed in any of the libraries?

These functions check that packages are installed with minimal side
effects. If installed, the packages will be loaded but not attached.

- `is_installed()` doesn't interact with the user. It simply returns
  `TRUE` or `FALSE` depending on whether the packages are installed.

- In interactive sessions, `check_installed()` asks the user whether to
  install missing packages. If the user accepts, the packages are
  installed with
  [`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.html)
  if available, or
  [`utils::install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  otherwise. If the session is non interactive or if the user chooses
  not to install the packages, the current evaluation is aborted.

You can disable the prompt by setting the
`rlib_restart_package_not_found` global option to `FALSE`. In that case,
missing packages always cause an error.

## Usage

``` r
is_installed(pkg, ..., version = NULL, compare = NULL)

check_installed(
  pkg,
  reason = NULL,
  ...,
  version = NULL,
  compare = NULL,
  action = NULL,
  call = caller_env()
)
```

## Arguments

- pkg:

  The package names. Can include version requirements, e.g.
  `"pkg (>= 1.0.0)"`.

- ...:

  These dots must be empty.

- version:

  Minimum versions for `pkg`. If supplied, must be the same length as
  `pkg`. `NA` elements stand for any versions.

- compare:

  A character vector of comparison operators to use for `version`. If
  supplied, must be the same length as `version`. If `NULL`, `>=` is
  used as default for all elements. `NA` elements in `compare` are also
  set to `>=` by default.

- reason:

  Optional string indicating why is `pkg` needed. Appears in error
  messages (if non-interactive) and user prompts (if interactive).

- action:

  An optional function taking `pkg` and `...` arguments. It is called by
  `check_installed()` when the user chooses to update outdated packages.
  The function is passed the missing and outdated packages as a
  character vector of names.

- call:

  The execution environment of a currently running function, e.g.
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md). The
  function will be mentioned in error messages as the source of the
  error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.md) for more
  information.

## Value

`is_installed()` returns `TRUE` if *all* package names provided in `pkg`
are installed, `FALSE` otherwise. `check_installed()` either doesn't
return or returns `NULL`.

## Handling package not found errors

`check_installed()` signals error conditions of class
`rlib_error_package_not_found`. The error includes `pkg` and `version`
fields. They are vectorised and may include several packages.

The error is signalled with a `rlib_restart_package_not_found` restart
on the stack to allow handlers to install the required packages. To do
so, add a [calling handler](https://rdrr.io/r/base/conditions.html) for
`rlib_error_package_not_found`, install the required packages, and
invoke the restart without arguments. This restarts the check from
scratch.

The condition is not signalled in non-interactive sessions, in the
restarting case, or if the `rlib_restart_package_not_found` user option
is set to `FALSE`.

## Examples

``` r
is_installed("utils")
#> [1] TRUE
is_installed(c("base", "ggplot5"))
#> [1] FALSE
is_installed(c("base", "ggplot5"), version = c(NA, "5.1.0"))
#> [1] FALSE
```
