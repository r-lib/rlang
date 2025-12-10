# Browse environments

**\[defunct\]**

- `env_browse(env)` is equivalent to evaluating
  [`browser()`](https://rdrr.io/r/base/browser.html) in `env`. It
  persistently sets the environment for step-debugging. Supply
  `value = FALSE` to disable browsing.

- `env_is_browsed()` is a predicate that inspects whether an environment
  is being browsed.

## Usage

``` r
env_browse(env, value = TRUE)

env_is_browsed(env)
```

## Arguments

- env:

  An environment.

- value:

  Whether to browse `env`.

## Value

`env_browse()` returns the previous value of `env_is_browsed()` (a
logical), invisibly.
