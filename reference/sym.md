# Create a symbol or list of symbols

Symbols are a kind of [defused
expression](https://rlang.r-lib.org/reference/topic-defuse.md) that
represent objects in environments.

- `sym()` and `syms()` take strings as input and turn them into symbols.

- `data_sym()` and `data_syms()` create calls of the form `.data$foo`
  instead of symbols. Subsetting the
  [`.data`](https://rlang.r-lib.org/reference/dot-data.md) pronoun is
  more robust when you expect a data-variable. See [The data mask
  ambiguity](https://rlang.r-lib.org/reference/topic-data-mask-ambiguity.md).

Only tidy eval APIs support the
[`.data`](https://rlang.r-lib.org/reference/dot-data.md) pronoun. With
base R functions, use simple symbols created with `sym()` or `syms()`.

## Usage

``` r
sym(x)

syms(x)

data_sym(x)

data_syms(x)
```

## Arguments

- x:

  For `sym()` and `data_sym()`, a string. For `syms()` and
  `data_syms()`, a list of strings.

## Value

For `sym()` and `syms()`, a symbol or list of symbols. For `data_sym()`
and `data_syms()`, calls of the form `.data$foo`.

## See also

- [Defusing R
  expressions](https://rlang.r-lib.org/reference/topic-defuse.md)

- [Metaprogramming
  patterns](https://rlang.r-lib.org/reference/topic-metaprogramming.md)

## Examples

``` r
# Create a symbol
sym("cyl")
#> cyl

# Create a list of symbols
syms(c("cyl", "am"))
#> [[1]]
#> cyl
#> 
#> [[2]]
#> am
#> 

# Symbolised names refer to variables
eval(sym("cyl"), mtcars)
#>  [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4

# Beware of scoping issues
Cyl <- "wrong"
eval(sym("Cyl"), mtcars)
#> [1] "wrong"

# Data symbols are explicitly scoped in the data mask
try(eval_tidy(data_sym("Cyl"), mtcars))
#> Error in .data$Cyl : Column `Cyl` not found in `.data`.

# These can only be used with tidy eval functions
try(eval(data_sym("Cyl"), mtcars))
#> Error in eval(data_sym("Cyl"), mtcars) : 
#>   Can't subset `.data` outside of a data mask context.

# The empty string returns the missing argument:
sym("")
#> 

# This way sym() and as_string() are inverse of each other:
as_string(missing_arg())
#> [1] ""
sym(as_string(missing_arg()))
#> 
```
