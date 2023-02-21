# dplyr

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidyverse/dplyr
* Source code: https://github.com/cran/dplyr
* Date/Publication: 2023-01-29 22:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "dplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("dplyr")
      [ FAIL 1 | WARN 0 | SKIP 311 | PASS 2743 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • Can't set a non-UTF-8 encoding (4)
      • Can't use 'en_US' locale (2)
      • On CRAN (305)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-join-by.R:236'): nicely catches missing arguments when wrapped ──
      `fn(a)` did not throw the expected error.
      
      [ FAIL 1 | WARN 0 | SKIP 311 | PASS 2743 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# ggpackets

<details>

* Version: 0.2.1
* GitHub: https://github.com/dgkf/ggpackets
* Source code: https://github.com/cran/ggpackets
* Date/Publication: 2022-10-10 23:30:02 UTC
* Number of recursive dependencies: 76

Run `revdepcheck::cloud_details(, "ggpackets")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpackets-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggpackets-package
    > ### Title: ggpackets: Package Plot Layers for Easier Portability and
    > ###   Modularization
    > ### Aliases: ggpackets ggpackets-package
    > 
    > ### ** Examples
    > 
    ...
     11.                   └─ggpackets:::handle_reset_mapping(do.call(ggplot2::aes, m1))
     12.                     ├─...[]
     13.                     ├─ggplot2:::`[.uneval`(...)
     14.                     │ └─ggplot2:::new_aes(NextMethod())
     15.                     ├─base::NextMethod()
     16.                     └─base::vapply(...)
     17.                       └─ggpackets (local) FUN(X[[i]], ...)
     18.                         └─rlang:::Ops.quosure(rlang::quo_squash(ai), quote(..reset..))
     19.                           └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       10.           └─base::Reduce(...)
       11.             └─ggpackets (local) f(init, x[[i]])
       12.               └─ggpackets:::handle_reset_mapping(do.call(ggplot2::aes, m1))
       13.                 ├─...[]
       14.                 ├─ggplot2:::`[.uneval`(...)
       15.                 │ └─ggplot2:::new_aes(NextMethod())
       16.                 ├─base::NextMethod()
       17.                 └─base::vapply(...)
       18.                   └─ggpackets (local) FUN(X[[i]], ...)
       19.                     └─rlang:::Ops.quosure(rlang::quo_squash(ai), quote(..reset..))
       20.                       └─rlang::abort(...)
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 41 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘composing-functions.Rmd’ using rmarkdown
    --- finished re-building ‘composing-functions.Rmd’
    
    --- re-building ‘ggpackets.Rmd’ using rmarkdown
    --- finished re-building ‘ggpackets.Rmd’
    
    --- re-building ‘miscellaneous-examples.Rmd’ using rmarkdown
    Quitting from lines 98-107 (miscellaneous-examples.Rmd) 
    ...
    # Bad: myquosure == rhs
    
    # Good: !!myquosure == rhs
    --- failed re-building ‘miscellaneous-examples.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘miscellaneous-examples.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# htmltools

<details>

* Version: 0.5.4
* GitHub: https://github.com/rstudio/htmltools
* Source code: https://github.com/cran/htmltools
* Date/Publication: 2022-12-07 20:50:06 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "htmltools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Actual message: "Argument 1 can't be empty."
      Backtrace:
          ▆
       1. ├─testthat::expect_error(as.character(div(, "one", )), "is empty") at test-tags.r:31:2
       2. │ └─testthat:::quasi_capture(...)
       3. │   ├─testthat (local) .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. ├─htmltools::div(, "one", )
       7. │ └─rlang::dots_list(...)
       8. └─rlang::abort(message = message)
      
      [ FAIL 2 | WARN 1 | SKIP 5 | PASS 2049 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘knitr’
    ```

# linne

<details>

* Version: 0.0.2
* GitHub: https://github.com/JohnCoene/linne
* Source code: https://github.com/cran/linne
* Date/Publication: 2020-10-26 09:20:10 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "linne")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘linne-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Linne
    > ### Title: Linne
    > ### Aliases: Linne
    > 
    > ### ** Examples
    > 
    > 
    ...
     11. │             ├─purrr:::with_indexed_errors(...)
     12. │             │ └─base::withCallingHandlers(...)
     13. │             ├─purrr:::call_with_cleanup(...)
     14. │             └─linne (local) .f(.x[[i]], ...)
     15. │               └─base::stop(...)
     16. └─base::.handleSimpleError(...)
     17.   └─purrr (local) h(simpleError(msg, call))
     18.     └─cli::cli_abort(...)
     19.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 29 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-class.R:8'): Generated CSS ─────────────────────────────────────
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `purrr::map(private$.rules, chg2css, private$.definitions)`: ℹ In index: 1.
      Caused by error in `purrr::map()`:
      ℹ In index: 1.
      ℹ With name: fontSize.
      Caused by error:
      ! Cannot find `~size`, did you `define` it?
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 29 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘R6’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# plotly

<details>

* Version: 4.10.1
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2022-11-07 07:30:03 UTC
* Number of recursive dependencies: 166

Run `revdepcheck::cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure ('test-ggplot-ggplotly.R:13'): can filter data returned by ggplotly ──
      `dat` not equivalent to subset(p$data, city == "Houston").
      Length mismatch: comparison on first 9 components
      ── Failure ('test-ggplot-ggplotly.R:14'): can filter data returned by ggplotly ──
      as.character(dplyr::groups(dat)) not equivalent to "city".
      1/1 mismatches
      x[1]: "`~city`"
      y[1]: "city"
      
      [ FAIL 4 | WARN 15 | SKIP 71 | PASS 1407 ]
      Deleting unused snapshots:
      • ggplot-contour/contour.svg
      • ggplot-heatmap/heatmap.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        htmlwidgets   4.0Mb
    ```

