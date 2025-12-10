# Change global options

- `local_options()` changes options for the duration of a stack frame
  (by default the current one). Options are set back to their old values
  when the frame returns.

- `with_options()` changes options while an expression is evaluated.
  Options are restored when the expression returns.

- `push_options()` adds or changes options permanently.

- `peek_option()` and `peek_options()` return option values. The former
  returns the option directly while the latter returns a list.

## Usage

``` r
local_options(..., .frame = caller_env())

with_options(.expr, ...)

push_options(...)

peek_options(...)

peek_option(name)
```

## Arguments

- ...:

  For `local_options()` and `push_options()`, named values defining new
  option values. For `peek_options()`, strings or character vectors of
  option names.

- .frame:

  The environment of a stack frame which defines the scope of the
  temporary options. When the frame returns, the options are set back to
  their original values.

- .expr:

  An expression to evaluate with temporary options.

- name:

  An option name as string.

## Value

For `local_options()` and `push_options()`, the old option values.
`peek_option()` returns the current value of an option while the plural
`peek_options()` returns a list of current option values.

## Life cycle

These functions are experimental.

## Examples

``` r
# Store and retrieve a global option:
push_options(my_option = 10)
peek_option("my_option")
#> [1] 10

# Change the option temporarily:
with_options(my_option = 100, peek_option("my_option"))
#> [1] 100
peek_option("my_option")
#> [1] 10

# The scoped variant is useful within functions:
fn <- function() {
  local_options(my_option = 100)
  peek_option("my_option")
}
fn()
#> [1] 100
peek_option("my_option")
#> [1] 10

# The plural peek returns a named list:
peek_options("my_option")
#> $my_option
#> [1] 10
#> 
peek_options("my_option", "digits")
#> $my_option
#> [1] 10
#> 
#> $digits
#> [1] 7
#> 
```
