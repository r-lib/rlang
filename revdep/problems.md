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
      • Can't use 'en_US' locale (2)
      • On CRAN (305)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-filter.R:301'): hybrid function row_number does not trigger warning in filter (#3750) ──
      `out` is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      ── Failure ('test-join-by.R:236'): nicely catches missing arguments when wrapped ──
      `fn(a)` did not throw the expected error.
      
      [ FAIL 2 | WARN 270 | SKIP 311 | PASS 2742 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# exDE

<details>

* Version: 1.0.0
* GitHub: https://github.com/dd-harp/exDE
* Source code: https://github.com/cran/exDE
* Date/Publication: 2022-11-18 10:00:04 UTC
* Number of recursive dependencies: 67

Run `revdepcheck::cloud_details(, "exDE")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `expected`: 249 17
      ── Failure ('test-equilibrium-RM-basic.R:121'): test equilibrium with RM adults (ODE), basic competition ──
      as.vector(out[2, params$Y_ix + 1]) (`actual`) not equal to as.vector(Y) (`expected`).
      
        `actual`: 78 16
      `expected`: 77 11
      ── Failure ('test-equilibrium-RM-basic.R:122'): test equilibrium with RM adults (ODE), basic competition ──
      as.vector(out[2, params$Z_ix + 1]) (`actual`) not equal to as.vector(Z) (`expected`).
      
        `actual`: 24.0 7.4
      `expected`: 23.5 5.8
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 46 ]
      Error: Test failures
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

# hyperSpec

<details>

* Version: 0.100.0
* GitHub: https://github.com/r-hyperspec/hyperSpec
* Source code: https://github.com/cran/hyperSpec
* Date/Publication: 2021-09-13 13:00:02 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::cloud_details(, "hyperSpec")` for more info

</details>

## Newly broken

*   checking running R code from vignettes ... ERROR
    ```
    Errors in running code in vignettes:
    when running code in ‘plotting.Rnw’
      ...
      'x' values are not equispaced; output may be wrong
    Warning in (function (x, y, z, subscripts, at = pretty(z), ..., col.regions = regions$col,  :
      'y' values are not equispaced; output may be wrong
    
    > plotvoronoi(uneven)
    Warning in (function (x, y, z, subscripts = TRUE, at = pretty(z), points = TRUE,  :
      The 'use.tripack' argument is deprecated and ignored. See ?panel.voronoi
    ...
    
    ... incomplete output.  Crash?
    
      ‘chondro.pdf.asis’ using ‘UTF-8’... OK
      ‘fileio.pdf.asis’ using ‘UTF-8’... OK
      ‘baseline.Rnw’ using ‘UTF-8’... OK
      ‘flu.Rnw’ using ‘UTF-8’... OK
      ‘hyperspec.Rnw’ using ‘UTF-8’... OK
      ‘laser.Rnw’ using ‘UTF-8’... OK
      ‘plotting.Rnw’ using ‘UTF-8’... failed to complete the test
    ```

## In both

*   checking re-building of vignette outputs ... NOTE
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘chondro.pdf.asis’ using asis
    --- finished re-building ‘chondro.pdf.asis’
    
    --- re-building ‘fileio.pdf.asis’ using asis
    --- finished re-building ‘fileio.pdf.asis’
    
    --- re-building ‘baseline.Rnw’ using Sweave
    Loading required package: lattice
    Loading required package: grid
    ...
    Warning in (function (x, y, z, subscripts, at = pretty(z), ..., col.regions = regions$col,  :
      'y' values are not equispaced; output may be wrong
    Warning in (function (x, y, z, subscripts = TRUE, at = pretty(z), points = TRUE,  :
      The 'use.tripack' argument is deprecated and ignored. See ?panel.voronoi
    Killed
    SUMMARY: processing the following files failed:
      ‘baseline.Rnw’ ‘flu.Rnw’ ‘hyperspec.Rnw’ ‘laser.Rnw’ ‘plotting.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# photosynthesis

<details>

* Version: 2.1.1
* GitHub: https://github.com/cdmuir/photosynthesis
* Source code: https://github.com/cran/photosynthesis
* Date/Publication: 2022-11-19 19:40:09 UTC
* Number of recursive dependencies: 135

Run `revdepcheck::cloud_details(, "photosynthesis")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected `{ ... }` to run without any conditions.
      ℹ Actually got a <lifecycle_stage>:
        Condition:
        `flatten()` is deprecated as of rlang 1.1.0. ℹ Please use
        `purrr::list_flatten()` or `purrr::list_c()`.
      ── Failure ('test-fit_aq_response2.R:44'): .vars argument renames variables ────
      Expected `{ ... }` to run without any conditions.
      ℹ Actually got a <lifecycle_stage>:
        Condition:
        `flatten()` is deprecated as of rlang 1.1.0. ℹ Please use
        `purrr::list_flatten()` or `purrr::list_c()`.
      
      [ FAIL 6 | WARN 2 | SKIP 0 | PASS 320 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.1Mb
      sub-directories of 1Mb or more:
        doc   6.1Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 13 marked UTF-8 strings
    ```

# portalr

<details>

* Version: 0.3.11
* GitHub: https://github.com/weecology/portalr
* Source code: https://github.com/cran/portalr
* Date/Publication: 2022-12-01 17:40:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "portalr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─portalr::bait_presence_absence(path = portal_data_path, level = "plot") at test-10-summarize_ants.R:49:2
       2. │ ├─compute_presence(bait, level) %>% as.data.frame()
       3. │ └─portalr:::compute_presence(bait, level)
       4. │   └─... %>% ...
       5. ├─base::as.data.frame(.)
       6. ├─tidyr::complete(., !!!grouping, fill = list(presence = 0))
       7. ├─dplyr::mutate(., presence = 1)
       8. ├─dplyr::distinct(.)
       9. └─dplyr::select(., !!!grouping)
      
      [ FAIL 12 | WARN 43 | SKIP 42 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

# targets

<details>

* Version: 0.14.2
* GitHub: https://github.com/ropensci/targets
* Source code: https://github.com/cran/targets
* Date/Publication: 2023-01-06 14:50:02 UTC
* Number of recursive dependencies: 174

Run `revdepcheck::cloud_details(, "targets")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       13.   ├─targets:::store_validate(target$store)
       14.   └─targets:::store_validate.default(target$store)
       15.     └─targets:::store_validate_packages(store)
       16.       └─targets::tar_assert_package(store_get_packages(store))
       17.         └─base::tryCatch(...)
       18.           └─base (local) tryCatchList(expr, classes, parentenv, handlers)
       19.             └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       20.               └─value[[3L]](cond)
       21.                 └─targets::tar_throw_validate(conditionMessage(e))
       22.                   └─targets::tar_error(...)
       23.                     └─rlang::abort(message = message, class = class, call = tar_empty_envir)
      
      [ FAIL 18 | WARN 0 | SKIP 408 | PASS 2548 ]
      Error: Test failures
      Execution halted
    ```

