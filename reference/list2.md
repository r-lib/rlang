# Collect dynamic dots in a list

`list2(...)` is equivalent to `list(...)` with a few additional
features, collectively called [dynamic
dots](https://rlang.r-lib.org/reference/dyn-dots.md). While `list2()`
hard-code these features, `dots_list()` is a lower-level version that
offers more control.

## Usage

``` r
list2(...)

dots_list(
  ...,
  .named = FALSE,
  .ignore_empty = c("trailing", "none", "all"),
  .preserve_empty = FALSE,
  .homonyms = c("keep", "first", "last", "error"),
  .check_assign = FALSE
)
```

## Arguments

- ...:

  Arguments to collect in a list. These dots are
  [dynamic](https://rlang.r-lib.org/reference/dyn-dots.md).

- .named:

  If `TRUE`, unnamed inputs are automatically named with
  [`as_label()`](https://rlang.r-lib.org/reference/as_label.md). This is
  equivalent to applying
  [`exprs_auto_name()`](https://rlang.r-lib.org/reference/exprs_auto_name.md)
  on the result. If `FALSE`, unnamed elements are left as is and, if
  fully unnamed, the list is given minimal names (a vector of `""`). If
  `NULL`, fully unnamed results are left with `NULL` names.

- .ignore_empty:

  Whether to ignore empty arguments. Can be one of `"trailing"`,
  `"none"`, `"all"`. If `"trailing"`, only the last argument is ignored
  if it is empty.

- .preserve_empty:

  Whether to preserve the empty arguments that were not ignored. If
  `TRUE`, empty arguments are stored with
  [`missing_arg()`](https://rlang.r-lib.org/reference/missing_arg.md)
  values. If `FALSE` (the default) an error is thrown when an empty
  argument is detected.

- .homonyms:

  How to treat arguments with the same name. The default, `"keep"`,
  preserves these arguments. Set `.homonyms` to `"first"` to only keep
  the first occurrences, to `"last"` to keep the last occurrences, and
  to `"error"` to raise an informative error and indicate what arguments
  have duplicated names.

- .check_assign:

  Whether to check for `<-` calls. When `TRUE` a warning recommends
  users to use `=` if they meant to match a function parameter or wrap
  the `<-` call in curly braces otherwise. This ensures assignments are
  explicit.

## Value

A list containing the `...` inputs.

## Details

For historical reasons, `dots_list()` creates a named list by default.
By comparison `list2()` implements the preferred behaviour of only
creating a names vector when a name is supplied.

## Examples

``` r
# Let's create a function that takes a variable number of arguments:
numeric <- function(...) {
  dots <- list2(...)
  num <- as.numeric(dots)
  set_names(num, names(dots))
}
numeric(1, 2, 3)
#> [1] 1 2 3

# The main difference with list(...) is that list2(...) enables
# the `!!!` syntax to splice lists:
x <- list(2, 3)
numeric(1, !!! x, 4)
#> [1] 1 2 3 4

# As well as unquoting of names:
nm <- "yup!"
numeric(!!nm := 1)
#> yup! 
#>    1 


# One useful application of splicing is to work around exact and
# partial matching of arguments. Let's create a function taking
# named arguments and dots:
fn <- function(data, ...) {
  list2(...)
}

# You normally cannot pass an argument named `data` through the dots
# as it will match `fn`'s `data` argument. The splicing syntax
# provides a workaround:
fn("wrong!", data = letters)  # exact matching of `data`
#> [[1]]
#> [1] "wrong!"
#> 
fn("wrong!", dat = letters)   # partial matching of `data`
#> [[1]]
#> [1] "wrong!"
#> 
fn(some_data, !!!list(data = letters))  # no matching
#> $data
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
#> [17] "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
#> 

# Empty trailing arguments are allowed:
list2(1, )
#> [[1]]
#> [1] 1
#> 

# But non-trailing empty arguments cause an error:
try(list2(1, , ))
#> Error in list2(1, , ) : Argument 2 can't be empty.

# Use the more configurable `dots_list()` function to preserve all
# empty arguments:
list3 <- function(...) dots_list(..., .preserve_empty = TRUE)

# Note how the last empty argument is still ignored because
# `.ignore_empty` defaults to "trailing":
list3(1, , )
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> 
#> 

# The list with preserved empty arguments is equivalent to:
list(1, missing_arg())
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> 
#> 


# Arguments with duplicated names are kept by default:
list2(a = 1, a = 2, b = 3, b = 4, 5, 6)
#> $a
#> [1] 1
#> 
#> $a
#> [1] 2
#> 
#> $b
#> [1] 3
#> 
#> $b
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 
#> [[6]]
#> [1] 6
#> 

# Use the `.homonyms` argument to keep only the first of these:
dots_list(a = 1, a = 2, b = 3, b = 4, 5, 6, .homonyms = "first")
#> $a
#> [1] 1
#> 
#> $b
#> [1] 3
#> 
#> [[3]]
#> [1] 5
#> 
#> [[4]]
#> [1] 6
#> 

# Or the last:
dots_list(a = 1, a = 2, b = 3, b = 4, 5, 6, .homonyms = "last")
#> $a
#> [1] 2
#> 
#> $b
#> [1] 4
#> 
#> [[3]]
#> [1] 5
#> 
#> [[4]]
#> [1] 6
#> 

# Or raise an informative error:
try(dots_list(a = 1, a = 2, b = 3, b = 4, 5, 6, .homonyms = "error"))
#> Error in eval(expr, envir) : 
#>   Arguments in `...` must have unique names.
#> ✖ Multiple arguments named `a` at positions 1 and 2.
#> ✖ Multiple arguments named `b` at positions 3 and 4.


# dots_list() can be configured to warn when a `<-` call is
# detected:
my_list <- function(...) dots_list(..., .check_assign = TRUE)
my_list(a <- 1)
#> Warning: Using `<-` as argument is often a mistake.
#> Do you need to use `=` to match an argument?
#> 
#> If you really want to use `<-`, please wrap in braces:
#> 
#>   # Bad:
#>   fn(a <- 1)
#> 
#>   # Good:
#>   fn(a = 1)       # Match 1 to parameter `a`
#>   fn({ a <- 1 })  # Assign 1 to variable `a`
#> [[1]]
#> [1] 1
#> 

# There is no warning if the assignment is wrapped in braces.
# This requires users to be explicit about their intent:
my_list({ a <- 1 })
#> [[1]]
#> [1] 1
#> 
```
