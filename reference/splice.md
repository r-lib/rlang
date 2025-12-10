# Splice values at dots collection time

`splice()` is an advanced feature of dynamic dots. It is rarely needed
but can solve performance issues in edge cases.

The splicing operator `!!!` operates both in values contexts like
[`list2()`](https://rlang.r-lib.org/reference/list2.md) and
[`dots_list()`](https://rlang.r-lib.org/reference/list2.md), and in
metaprogramming contexts like
[`expr()`](https://rlang.r-lib.org/reference/expr.md),
[`enquos()`](https://rlang.r-lib.org/reference/enquo.md), or
[`inject()`](https://rlang.r-lib.org/reference/inject.md). While the end
result looks the same, the implementation is different and much more
efficient in the value cases. This difference in implementation may
cause performance issues for instance when going from:

    xs <- list(2, 3)
    list2(1, !!!xs, 4)

to:

    inject(list2(1, !!!xs, 4))

In the former case, the performant value-splicing is used. In the latter
case, the slow metaprogramming splicing is used.

A common practical case where this may occur is when code is wrapped
inside a tidyeval context like
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).
In this case, the metaprogramming operator `!!!` will take over the
value-splicing operator, causing an unexpected slowdown.

To avoid this in performance-critical code, use `splice()` instead of
`!!!`:

    # These both use the fast splicing:
    list2(1, splice(xs), 4)
    inject(list2(1, splice(xs), 4))

Note that `splice()` behaves differently than `!!!`. The splicing
happens later and is processed by
[`list2()`](https://rlang.r-lib.org/reference/list2.md) or
[`dots_list()`](https://rlang.r-lib.org/reference/list2.md). It does not
work in any other tidyeval context than these list collectors.

## Usage

``` r
splice(x)

is_spliced(x)

is_spliced_bare(x)
```

## Arguments

- x:

  A list or vector to splice non-eagerly.
