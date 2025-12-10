# Ensure that all elements of a list of expressions are named

This gives default names to unnamed elements of a list of expressions
(or expression wrappers such as formulas or quosures), deparsed with
[`as_label()`](https://rlang.r-lib.org/dev/reference/as_label.md).

## Usage

``` r
exprs_auto_name(
  exprs,
  ...,
  repair_auto = c("minimal", "unique"),
  repair_quiet = FALSE
)

quos_auto_name(quos)
```

## Arguments

- exprs:

  A list of expressions.

- ...:

  These dots are for future extensions and must be empty.

- repair_auto:

  Whether to repair the automatic names. By default, minimal names are
  returned. See
  [`?vctrs::vec_as_names`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for information about name repairing.

- repair_quiet:

  Whether to inform user about repaired names.

- quos:

  A list of quosures.
