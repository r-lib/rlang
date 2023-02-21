# arena2r

<details>

* Version: 1.0.0
* GitHub: https://github.com/pedroliman/arena2r
* Source code: https://github.com/cran/arena2r
* Date/Publication: 2018-10-19 15:30:03 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "arena2r")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘arena2r-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plot_confint
    > ### Title: Confidence Interval Plot
    > ### Aliases: plot_confint
    > 
    > ### ** Examples
    > 
    > library(arena2r)
    ...
     42. │     └─rlang::warn(format_warning(message, .envir = .envir), ...)
     43. │       └─base::warning(cnd)
     44. │         └─base::withRestarts(...)
     45. │           └─base (local) withOneRestart(expr, restarts[[1L]])
     46. │             └─base (local) doWithOneRestart(return(expr), restart)
     47. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     48.   └─handlers[[1L]](cnd)
     49.     └─cli::cli_abort(...)
     50.       └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘arena2r-vignette.Rmd’ using rmarkdown
    Quitting from lines 79-84 (arena2r-vignette.Rmd) 
    Error: processing vignette 'arena2r-vignette.Rmd' failed with diagnostics:
    Problem while computing stat.
    ℹ Error occurred in the 2nd layer.
    Caused by error in `fun.data()`:
    ! The package "Hmisc" is required.
    --- failed re-building ‘arena2r-vignette.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘arena2r-vignette.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘purrr’ ‘shinyBS’ ‘shinydashboard’ ‘shinyjs’
      All declared Imports should be used.
    ```

# camcorder

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/camcorder
* Date/Publication: 2022-10-03 07:40:05 UTC
* Number of recursive dependencies: 90

Run `revdepcheck::cloud_details(, "camcorder")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘camcorder_record.Rmd’ using rmarkdown
    Quitting from lines 48-128 (camcorder_record.Rmd) 
    Error: processing vignette 'camcorder_record.Rmd' failed with diagnostics:
    Problem while computing stat.
    ℹ Error occurred in the 1st layer.
    Caused by error in `compute_group()`:
    ! The package "hexbin" is required for `stat_binhex()`
    --- failed re-building ‘camcorder_record.Rmd’
    ...
    --- finished re-building ‘camcorder_view.Rmd’
    
    --- re-building ‘pdf_fonts.Rmd’ using rmarkdown
    --- finished re-building ‘pdf_fonts.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘camcorder_record.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# directlabels

<details>

* Version: 2021.1.13
* GitHub: https://github.com/tdhock/directlabels
* Source code: https://github.com/cran/directlabels
* Date/Publication: 2021-01-16 02:10:12 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "directlabels")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘examples.Rmd’ using knitr
    Quitting from lines 83-109 (examples.Rmd) 
    Error: processing vignette 'examples.Rmd' failed with diagnostics:
    Problem while computing stat.
    ℹ Error occurred in the 3rd layer.
    Caused by error in `get()`:
    ! object 'last.qp' of mode 'function' was not found
    --- failed re-building ‘examples.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘examples.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# dlookr

<details>

* Version: 0.6.1
* GitHub: https://github.com/choonghyunryu/dlookr
* Source code: https://github.com/cran/dlookr
* Date/Publication: 2022-11-08 05:10:02 UTC
* Number of recursive dependencies: 180

Run `revdepcheck::cloud_details(, "dlookr")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘EDA.Rmd’ using rmarkdown
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Roboto Condensed Light' not found, will use 'sans' instead
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Roboto Condensed Light' not found, will use 'sans' instead
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Roboto Condensed Light' not found, will use 'sans' instead
    Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  :
      font family 'Roboto Condensed Light' not found, will use 'sans' instead
    ...
    --- finished re-building ‘introduce.Rmd’
    
    --- re-building ‘transformation.Rmd’ using rmarkdown
    --- finished re-building ‘transformation.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘EDA.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

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

# GDAtools

<details>

* Version: 1.7.2
* GitHub: https://github.com/nicolas-robette/GDAtools
* Source code: https://github.com/cran/GDAtools
* Date/Publication: 2022-02-22 14:40:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "GDAtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘GDAtools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggcloud_indiv
    > ### Title: Plots MCA cloud of individuals with ggplot2
    > ### Aliases: ggcloud_indiv
    > ### Keywords: aplot multivariate
    > 
    > ### ** Examples
    > 
    ...
     38. │     └─rlang::warn(format_warning(message, .envir = .envir), ...)
     39. │       └─base::warning(cnd)
     40. │         └─base::withRestarts(...)
     41. │           └─base (local) withOneRestart(expr, restarts[[1L]])
     42. │             └─base (local) doWithOneRestart(return(expr), restart)
     43. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     44.   └─handlers[[1L]](cnd)
     45.     └─cli::cli_abort(...)
     46.       └─rlang::abort(...)
    Execution halted
    ```

# ggdist

<details>

* Version: 3.2.1
* GitHub: https://github.com/mjskay/ggdist
* Source code: https://github.com/cran/ggdist
* Date/Publication: 2023-01-18 09:00:06 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "ggdist")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • test.stat_sample_slabinterval/cdfintervalh-log-scale-transform.svg
      • test.stat_sample_slabinterval/constant-dist-on-ccdf-with-n-1.svg
      • test.stat_sample_slabinterval/constant-dist-on-ccdf.svg
      • test.stat_sample_slabinterval/constant-dist-on-halfeye-with-n-1.svg
      • test.stat_sample_slabinterval/constant-dist-on-histinterval.svg
      • test.stat_sample_slabinterval/fill-type-gradient-with-two-groups-h.svg
      • test.stat_sample_slabinterval/gradientintervalh-with-two-groups.svg
      • test.stat_sample_slabinterval/histinterval-with-outlines-bw-bars.svg
      • test.stat_sample_slabinterval/histintervalh-log-scale-transform.svg
      • test.stat_sample_slabinterval/histintervalh-with-outline.svg
      • test.stat_sample_slabinterval/nas-with-na-rm-true.svg
      • test.stat_sample_slabinterval/slab-with-outline.svg
      • test.theme_ggdist/facet-titles-on-left.svg
      Error: Test failures
      Execution halted
    ```

# ggforce

<details>

* Version: 0.4.1
* GitHub: https://github.com/thomasp85/ggforce
* Source code: https://github.com/cran/ggforce
* Date/Publication: 2022-10-04 09:50:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "ggforce")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggforce-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_autodensity
    > ### Title: A distribution geoms that fills the panel and works with
    > ###   discrete and continuous data
    > ### Aliases: geom_autodensity geom_autohistogram
    > 
    > ### ** Examples
    > 
    ...
     40. │       └─rlang::warn(format_warning(message, .envir = .envir), ...)
     41. │         └─base::warning(cnd)
     42. │           └─base::withRestarts(...)
     43. │             └─base (local) withOneRestart(expr, restarts[[1L]])
     44. │               └─base (local) doWithOneRestart(return(expr), restart)
     45. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     46.   └─handlers[[1L]](cnd)
     47.     └─cli::cli_abort(...)
     48.       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 28.3Mb
      sub-directories of 1Mb or more:
        help   1.2Mb
        libs  26.4Mb
    ```

# ggformula

<details>

* Version: 0.10.2
* GitHub: https://github.com/ProjectMOSAIC/ggformula
* Source code: https://github.com/cran/ggformula
* Date/Publication: 2022-09-01 00:00:02 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "ggformula")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggformula-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gf_hex
    > ### Title: Formula interface to geom_hex()
    > ### Aliases: gf_hex
    > 
    > ### ** Examples
    > 
    > gf_hex(avg_drinks ~ age, data = mosaicData::HELPrct, bins = 15) %>%
    ...
     40. │     └─rlang::warn(format_warning(message, .envir = .envir), ...)
     41. │       └─base::warning(cnd)
     42. │         └─base::withRestarts(...)
     43. │           └─base (local) withOneRestart(expr, restarts[[1L]])
     44. │             └─base (local) doWithOneRestart(return(expr), restart)
     45. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     46.   └─handlers[[1L]](cnd)
     47.     └─cli::cli_abort(...)
     48.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       43. │ └─handlers[[1L]](cnd)
       44. │   └─cli::cli_warn(...)
       45. │     └─rlang::warn(format_warning(message, .envir = .envir), ...)
       46. │       └─base::warning(cnd)
       47. │         └─base::withRestarts(...)
       48. │           └─base (local) withOneRestart(expr, restarts[[1L]])
       49. │             └─base (local) doWithOneRestart(return(expr), restart)
       50. └─rlang (local) `<fn>`(`<rlng_wrn>`)
       51.   └─handlers[[1L]](cnd)
       52.     └─cli::cli_abort(...)
       53.       └─rlang::abort(...)
      
      [ FAIL 2 | WARN 7 | SKIP 42 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘interp’, ‘quantreg’
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

# ggplot2

<details>

* Version: 3.4.1
* GitHub: https://github.com/tidyverse/ggplot2
* Source code: https://github.com/cran/ggplot2
* Date/Publication: 2023-02-10 13:20:06 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "ggplot2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggplot2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: geom_quantile
    > ### Title: Quantile regression
    > ### Aliases: geom_quantile stat_quantile
    > 
    > ### ** Examples
    > 
    > m <-
    ...
     45. │       └─rlang::warn(format_warning(message, .envir = .envir), ...)
     46. │         └─base::warning(cnd)
     47. │           └─base::withRestarts(...)
     48. │             └─base (local) withOneRestart(expr, restarts[[1L]])
     49. │               └─base (local) doWithOneRestart(return(expr), restart)
     50. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     51.   └─handlers[[1L]](cnd)
     52.     └─cli::cli_abort(...)
     53.       └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • theme/theme-gray-large.svg
      • theme/theme-gray.svg
      • theme/theme-light-large.svg
      • theme/theme-light.svg
      • theme/theme-linedraw-large.svg
      • theme/theme-linedraw.svg
      • theme/theme-minimal-large.svg
      • theme/theme-minimal.svg
      • theme/theme-void-large.svg
      • theme/theme-void.svg
      • theme/ticks-length.svg
      • theme/titles-aligned-to-entire-plot.svg
      • theme/width-is-3-times-height.svg
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        R     1.2Mb
        doc   1.9Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘mgcv’
      All declared Imports should be used.
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘akima’
    ```

# ggpmisc

<details>

* Version: 0.5.2
* GitHub: https://github.com/aphalo/ggpmisc
* Source code: https://github.com/cran/ggpmisc
* Date/Publication: 2022-12-17 23:10:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::cloud_details(, "ggpmisc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ggpmisc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: ggpmisc-package
    > ### Title: ggpmisc: Miscellaneous Extensions to 'ggplot2'
    > ### Aliases: ggpmisc ggpmisc-package
    > 
    > ### ** Examples
    > 
    > library(tibble)
    ...
     35. │       └─rlang::warn(format_warning(message, .envir = .envir), ...)
     36. │         └─base::warning(cnd)
     37. │           └─base::withRestarts(...)
     38. │             └─base (local) withOneRestart(expr, restarts[[1L]])
     39. │               └─base (local) doWithOneRestart(return(expr), restart)
     40. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     41.   └─handlers[[1L]](cnd)
     42.     └─cli::cli_abort(...)
     43.       └─rlang::abort(...)
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

# ordr

<details>

* Version: 0.1.1
* GitHub: https://github.com/corybrunson/ordr
* Source code: https://github.com/cran/ordr
* Date/Publication: 2022-10-20 20:52:35 UTC
* Number of recursive dependencies: 98

Run `revdepcheck::cloud_details(, "ordr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ordr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: draw-key
    > ### Title: Biplot key drawing functions
    > ### Aliases: draw-key draw_key_line draw_key_crosslines draw_key_crosspoint
    > 
    > ### ** Examples
    > 
    > # scaled PCA of Anderson iris data with ranges and confidence intervals
    ...
     42. │     └─rlang::warn(format_warning(message, .envir = .envir), ...)
     43. │       └─base::warning(cnd)
     44. │         └─base::withRestarts(...)
     45. │           └─base (local) withOneRestart(expr, restarts[[1L]])
     46. │             └─base (local) doWithOneRestart(return(expr), restart)
     47. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     48.   └─handlers[[1L]](cnd)
     49.     └─cli::cli_abort(...)
     50.       └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# pammtools

<details>

* Version: 0.5.8
* GitHub: https://github.com/adibender/pammtools
* Source code: https://github.com/cran/pammtools
* Date/Publication: 2022-01-09 03:32:43 UTC
* Number of recursive dependencies: 127

Run `revdepcheck::cloud_details(, "pammtools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘pammtools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gg_re
    > ### Title: Plot Normal QQ plots for random effects
    > ### Aliases: gg_re
    > 
    > ### ** Examples
    > 
    > library(pammtools)
    ...
     38. │       └─rlang::warn(format_warning(message, .envir = .envir), ...)
     39. │         └─base::warning(cnd)
     40. │           └─base::withRestarts(...)
     41. │             └─base (local) withOneRestart(expr, restarts[[1L]])
     42. │               └─base (local) doWithOneRestart(return(expr), restart)
     43. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     44.   └─handlers[[1L]](cnd)
     45.     └─cli::cli_abort(...)
     46.       └─rlang::abort(...)
    Execution halted
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

# rsimsum

<details>

* Version: 0.11.3
* GitHub: https://github.com/ellessenne/rsimsum
* Source code: https://github.com/cran/rsimsum
* Date/Publication: 2022-08-17 09:10:02 UTC
* Number of recursive dependencies: 130

Run `revdepcheck::cloud_details(, "rsimsum")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘rsimsum-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: autoplot.simsum
    > ### Title: autoplot method for simsum objects
    > ### Aliases: autoplot.simsum
    > 
    > ### ** Examples
    > 
    > data("MIsim", package = "rsimsum")
    ...
     40. │     └─rlang::warn(format_warning(message, .envir = .envir), ...)
     41. │       └─base::warning(cnd)
     42. │         └─base::withRestarts(...)
     43. │           └─base (local) withOneRestart(expr, restarts[[1L]])
     44. │             └─base (local) doWithOneRestart(return(expr), restart)
     45. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     46.   └─handlers[[1L]](cnd)
     47.     └─cli::cli_abort(...)
     48.       └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘A-introduction.Rmd’ using rmarkdown
    --- finished re-building ‘A-introduction.Rmd’
    
    --- re-building ‘B-relhaz.Rmd’ using rmarkdown
    --- finished re-building ‘B-relhaz.Rmd’
    
    --- re-building ‘C-plotting.Rmd’ using rmarkdown
    Quitting from lines 205-206 (C-plotting.Rmd) 
    Error: processing vignette 'C-plotting.Rmd' failed with diagnostics:
    ...
    --- finished re-building ‘E-custom-inputs.Rmd’
    
    --- re-building ‘F-rsimsumtidyverse.Rmd’ using rmarkdown
    --- finished re-building ‘F-rsimsumtidyverse.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘C-plotting.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# sherlock

<details>

* Version: 0.6.0
* GitHub: https://github.com/gaboraszabo/sherlock
* Source code: https://github.com/cran/sherlock
* Date/Publication: 2023-02-10 13:20:02 UTC
* Number of recursive dependencies: 93

Run `revdepcheck::cloud_details(, "sherlock")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sherlock-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: draw_youden_plot
    > ### Title: Draw Youden Plot
    > ### Aliases: draw_youden_plot
    > 
    > ### ** Examples
    > 
    > youden_plot_data %>%
    ...
     38. │     └─rlang::warn(format_warning(message, .envir = .envir), ...)
     39. │       └─base::warning(cnd)
     40. │         └─base::withRestarts(...)
     41. │           └─base (local) withOneRestart(expr, restarts[[1L]])
     42. │             └─base (local) doWithOneRestart(return(expr), restart)
     43. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     44.   └─handlers[[1L]](cnd)
     45.     └─cli::cli_abort(...)
     46.       └─rlang::abort(...)
    Execution halted
    ```

# sistmr

<details>

* Version: 0.1.1
* GitHub: NA
* Source code: https://github.com/cran/sistmr
* Date/Publication: 2022-03-24 08:30:02 UTC
* Number of recursive dependencies: 37

Run `revdepcheck::cloud_details(, "sistmr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sistmr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: BlandAltmanPlot
    > ### Title: Bland-Altman plot function
    > ### Aliases: BlandAltmanPlot
    > 
    > ### ** Examples
    > 
    >  
    ...
     38. │     └─rlang::warn(format_warning(message, .envir = .envir), ...)
     39. │       └─base::warning(cnd)
     40. │         └─base::withRestarts(...)
     41. │           └─base (local) withOneRestart(expr, restarts[[1L]])
     42. │             └─base (local) doWithOneRestart(return(expr), restart)
     43. └─rlang (local) `<fn>`(`<rlng_wrn>`)
     44.   └─handlers[[1L]](cnd)
     45.     └─cli::cli_abort(...)
     46.       └─rlang::abort(...)
    Execution halted
    ```

# tidyterra

<details>

* Version: 0.3.1
* GitHub: https://github.com/dieghernan/tidyterra
* Source code: https://github.com/cran/tidyterra
* Date/Publication: 2022-11-09 12:40:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "tidyterra")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-geom_spatraster_1lyr.R:23'): geom_spatraster one layer with CRS ──
      Error in `geom_spatraster(data = r)`: Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `reproject_raster_on_stat()`:
      ! geom_spatraster_*() on SpatRasters with crs must be used with coord_sf().
      ── Error ('test-geom_spatraster_3lyr.R:25'): geom_spatraster several layer with CRS ──
      Error in `geom_spatraster(data = r)`: Problem while computing stat.
      i Error occurred in the 1st layer.
      Caused by error in `reproject_raster_on_stat()`:
      ! geom_spatraster_*() on SpatRasters with crs must be used with coord_sf().
      
      [ FAIL 2 | WARN 0 | SKIP 16 | PASS 951 ]
      Error: Test failures
      Execution halted
    ```

