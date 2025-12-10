# Evaluate dots with preliminary splicing

This is a tool for advanced users. It captures dots, processes unquoting
and splicing operators, and evaluates them. Unlike
[`dots_list()`](https://rlang.r-lib.org/reference/list2.md), it does not
flatten spliced objects, instead they are attributed a `spliced` class
(see [`splice()`](https://rlang.r-lib.org/reference/splice.md)). You can
process spliced objects manually, perhaps with a custom predicate (see
[`flatten_if()`](https://rlang.r-lib.org/reference/flatten.md)).

## Usage

``` r
dots_values(
  ...,
  .ignore_empty = c("trailing", "none", "all"),
  .preserve_empty = FALSE,
  .homonyms = c("keep", "first", "last", "error"),
  .check_assign = FALSE
)
```

## Arguments

- ...:

  Arguments to evaluate and process splicing operators.

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

## Examples

``` r
dots <- dots_values(!!! list(1, 2), 3)
dots
#> [[1]]
#> <spliced>
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> 
#> [[2]]
#> [1] 3
#> 

# Flatten the objects marked as spliced:
flatten_if(dots, is_spliced)
#> Warning: `flatten_if()` is deprecated as of rlang 1.1.0.
#> This warning is displayed once every 8 hours.
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
```
