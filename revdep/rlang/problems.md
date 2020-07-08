# abjutils

<details>

* Version: 0.2.3
* Source code: https://github.com/cran/abjutils
* URL: https://github.com/abjur/abjutils
* Date/Publication: 2019-02-07 21:43:35 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "abjutils")` for more info

</details>

## Newly broken

*   checking whether package ‘abjutils’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/abjutils/new/abjutils.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘httr’ ‘progress’
      All declared Imports should be used.
    Missing or unexported object: ‘devtools::use_package’
    ```

## Installation

### Devel

```
* installing *source* package ‘abjutils’ ...
** package ‘abjutils’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in envlist(e) : attempt to use zero-length variable name
Calls: <Anonymous> ... <Anonymous> -> lazyLoadDBinsertValue -> <Anonymous> -> envlist
Execution halted
ERROR: lazy loading failed for package ‘abjutils’
* removing ‘/tmp/workdir/abjutils/new/abjutils.Rcheck/abjutils’

```
### CRAN

```
* installing *source* package ‘abjutils’ ...
** package ‘abjutils’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (abjutils)

```
# cattonum

<details>

* Version: 0.0.5
* Source code: https://github.com/cran/cattonum
* URL: https://github.com/bfgray3/cattonum
* BugReports: https://github.com/bfgray3/cattonum/issues
* Date/Publication: 2020-06-15 04:50:06 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "cattonum")` for more info

</details>

## Newly broken

*   checking whether package ‘cattonum’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/cattonum/new/cattonum.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘cattonum’ ...
** package ‘cattonum’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/opt/R/3.6.3/lib/R/library/Rcpp/include" -I/usr/local/include  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/opt/R/3.6.3/lib/R/library/Rcpp/include" -I/usr/local/include  -fpic  -g -O2  -c mean.cpp -o mean.o
g++ -std=gnu++11 -shared -L/opt/R/3.6.3/lib/R/lib -L/usr/local/lib -o cattonum.so RcppExports.o mean.o -L/opt/R/3.6.3/lib/R/lib -lR
installing to /tmp/workdir/cattonum/new/cattonum.Rcheck/00LOCK-cattonum/00new/cattonum/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error in envlist(e) : attempt to use zero-length variable name
Calls: <Anonymous> ... <Anonymous> -> lazyLoadDBinsertValue -> <Anonymous> -> envlist
Execution halted
ERROR: lazy loading failed for package ‘cattonum’
* removing ‘/tmp/workdir/cattonum/new/cattonum.Rcheck/cattonum’

```
### CRAN

```
* installing *source* package ‘cattonum’ ...
** package ‘cattonum’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
g++ -std=gnu++11 -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/opt/R/3.6.3/lib/R/library/Rcpp/include" -I/usr/local/include  -fpic  -g -O2  -c RcppExports.cpp -o RcppExports.o
g++ -std=gnu++11 -I"/opt/R/3.6.3/lib/R/include" -DNDEBUG  -I"/opt/R/3.6.3/lib/R/library/Rcpp/include" -I/usr/local/include  -fpic  -g -O2  -c mean.cpp -o mean.o
g++ -std=gnu++11 -shared -L/opt/R/3.6.3/lib/R/lib -L/usr/local/lib -o cattonum.so RcppExports.o mean.o -L/opt/R/3.6.3/lib/R/lib -lR
installing to /tmp/workdir/cattonum/old/cattonum.Rcheck/00LOCK-cattonum/00new/cattonum/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (cattonum)

```
# dbplyr

<details>

* Version: 1.4.4
* Source code: https://github.com/cran/dbplyr
* URL: https://dbplyr.tidyverse.org/, https://github.com/tidyverse/dbplyr
* BugReports: https://github.com/tidyverse/dbplyr/issues
* Date/Publication: 2020-05-27 05:30:05 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "dbplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
        1. dbplyr::translate_sql(...)
        7. dbplyr:::case_when(x == 1L ~ "yes", x == 0L ~ "no", TRUE ~ "undefined")
        8. dbplyr:::sql_case_when(...)
       10. rlang::eval_bare(f[[2]], env)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 642 | SKIPPED: 16 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: Special ifelse and case_when cases return the correct queries (@test-backend-mssql.R#162) 
      2. Error: case_when converted to CASE WHEN (@test-translate-sql-conditional.R#4) 
      3. Error: even inside mutate (@test-translate-sql-conditional.R#11) 
      4. Error: case_when translates correctly to ELSE when TRUE ~ is used 2 (@test-translate-sql-conditional.R#22) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘magrittr’
      All declared Imports should be used.
    ```

# DeclareDesign

<details>

* Version: 0.22.0
* Source code: https://github.com/cran/DeclareDesign
* URL: https://declaredesign.org, https://github.com/DeclareDesign/DeclareDesign
* BugReports: https://github.com/DeclareDesign/DeclareDesign/issues
* Date/Publication: 2020-03-24 07:40:10 UTC
* Number of recursive dependencies: 132

Run `cloud_details(, "DeclareDesign")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       8. base::list2env(x, envir = NULL, parent = y, hash = TRUE)
      
      ── 2. Error: Simulate Design works x2 (@test-simulate-design.R#43)  ────────────
      attempt to use zero-length variable name
      Backtrace:
       1. DeclareDesign::simulate_design(...)
       8. base::list2env(x, envir = NULL, parent = y, hash = TRUE)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 491 | SKIPPED: 5 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: correct fan out (@test-fanout.R#160) 
      2. Error: Simulate Design works x2 (@test-simulate-design.R#43) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# hms

<details>

* Version: 0.5.3
* Source code: https://github.com/cran/hms
* URL: https://hms.tidyverse.org/, https://github.com/tidyverse/hms
* BugReports: https://github.com/tidyverse/hms/issues
* Date/Publication: 2020-01-08 23:01:22 UTC
* Number of recursive dependencies: 31

Run `cloud_details(, "hms")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected class: hms_lossy_cast
      Actual class:   lifecycle_soft_deprecated/condition
      Message:        The `values` argument to `arg_match()` is deprecated as of rlang 0.4.7. Use `arg_match0()` instead.
      Backtrace:
        1. testthat::expect_condition(hms::as_hms(x), class = "hms_lossy_cast")
       15. base::withRestarts(rlang_muffle = function() NULL, signalCondition(x))
       16. base:::withOneRestart(expr, restarts[[1L]])
       17. base:::doWithOneRestart(return(expr), restart)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 193 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: coercion in (@test-coercion.R#28) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# PAutilities

<details>

* Version: 1.0.1
* Source code: https://github.com/cran/PAutilities
* URL: https://github.com/paulhibbing/PAutilities
* BugReports: https://github.com/paulhibbing/PAutilities/issues
* Date/Publication: 2020-05-17 05:00:27 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "PAutilities")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > # Reduce the number of data points (for illustration purposes) by isolating
    > # the 150 largest cases
    > 
    > illustration_threshold <-
    +     quantile(ex_data$Axis1, probs = 1 - (150 / nrow(ex_data)))
    > ex_data <- ex_data[ex_data$Axis1 > illustration_threshold, ]
    > 
    > # Generate the plot
    > my_ba <- ba_plot(
    +     ex_data,
    +     "(Axis1 + Axis3) / 2",
    +     "Axis1 - Axis3",
    +     "mean(Axis1, Axis3)",
    +     "Axis1 - Axis3"
    + )
    > 
    > my_ba
    Error in list2env(as.list(x, all.names = TRUE), parent = parent.env(x)) : 
      attempt to use zero-length variable name
    Calls: <Anonymous> ... <Anonymous> -> eval_expr -> complain -> clone_env -> list2env
    Execution halted
    ```

# plotly

<details>

* Version: 4.9.2.1
* Source code: https://github.com/cran/plotly
* URL: https://plotly-r.com, https://github.com/ropensci/plotly#readme, https://plot.ly/r
* BugReports: https://github.com/ropensci/plotly/issues
* Date/Publication: 2020-04-04 19:50:02 UTC
* Number of recursive dependencies: 150

Run `cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    Warning: `filter_()` is deprecated as of dplyr 0.7.0.
    Please use `filter()` instead.
    See vignette('programming') for more help
    This warning is displayed once every 8 hours.
    Call `lifecycle::last_warnings()` to see where this warning was generated.
    > plotly_data(p)
    # A tibble: 1 x 7
      date         pce    pop psavert uempmed unemploy   rate
      <date>     <dbl>  <dbl>   <dbl>   <dbl>    <dbl>  <dbl>
    1 1982-12-01 2162. 233160    10.9    10.2    12051 0.0517
    > add_markers(p)
    > layout(p, annotations = list(x = ~date, y = ~rate, text = "peak"))
    > 
    > # use group_by() + do() + subplot() for trellis displays 
    > d <- group_by(mpg, drv)
    > plots <- do(d, p = plot_ly(., x = ~cty, name = ~drv))
    > subplot(plots[["p"]], nrows = 3, shareX = TRUE)
    Error in list2env(as.list(x, all.names = TRUE), parent = parent.env(x)) : 
      attempt to use zero-length variable name
    Calls: subplot ... <Anonymous> -> eval_expr -> complain -> clone_env -> list2env
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.7Mb
    ```

# pointblank

<details>

* Version: 0.4.0
* Source code: https://github.com/cran/pointblank
* URL: https://github.com/rich-iannone/pointblank
* BugReports: https://github.com/rich-iannone/pointblank/issues
* Date/Publication: 2020-06-22 17:20:02 UTC
* Number of recursive dependencies: 100

Run `cloud_details(, "pointblank")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. pointblank::interrogate(.)
       46. dbplyr:::sql_case_when(...)
       48. rlang::eval_bare(f[[2]], env)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 2151 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: Interrogating for valid row values (@test-interrogate_with_agent_db.R#208) 
      2. Error: Interrogating with an agent incorporates the `na_pass` option (@test-interrogate_with_agent_db.R#627) 
      3. Error: The validations with sets can include NA values (@test-interrogate_with_agent_db.R#870) 
      4. Error: (unknown) (@test-sundering.R#38) 
      
      Error: testthat unit tests failed
      In addition: Warning message:
      call dbDisconnect() when finished working with a connection 
      Execution halted
    ```

# purrr

<details>

* Version: 0.3.4
* Source code: https://github.com/cran/purrr
* URL: http://purrr.tidyverse.org, https://github.com/tidyverse/purrr
* BugReports: https://github.com/tidyverse/purrr/issues
* Date/Publication: 2020-04-17 12:10:07 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "purrr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── 6. Failure: rate_backoff() backs off (@test-rate.R#78)  ─────────────────────
      msg$length not identical to 0.08.
      target is NULL, current is numeric
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 764 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 6 ]
      1. Failure: rate_delay() delays (@test-rate.R#44) 
      2. Failure: rate_delay() delays (@test-rate.R#45) 
      3. Failure: rate_delay() delays (@test-rate.R#48) 
      4. Failure: rate_backoff() backs off (@test-rate.R#74) 
      5. Failure: rate_backoff() backs off (@test-rate.R#75) 
      6. Failure: rate_backoff() backs off (@test-rate.R#78) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# reproducible

<details>

* Version: 1.1.1
* Source code: https://github.com/cran/reproducible
* URL: https://reproducible.predictiveecology.org, https://github.com/PredictiveEcology/reproducible
* BugReports: https://github.com/PredictiveEcology/reproducible/issues
* Date/Publication: 2020-05-20 05:10:05 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "reproducible")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      3      readRDS   Saving to repo 0.1246225834  secs
      4      readRDS Whole Cache call 0.1392412186  secs
        objectNames hashElements             hash objSize
      1        file         file f725e84a09034205   24121
      2        .FUN         .FUN 7a8f2865ef4bc06d    1256
        functionName         component  elapsedTime units
      1      readRDS           Hashing 0.0009574890  secs
      2      readRDS Loading from repo 0.0006072521  secs
      3      readRDS  Whole Cache call 0.0790381432  secs
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 400 | SKIPPED: 71 | WARNINGS: 9 | FAILED: 1 ]
      1. Failure: test file-backed raster caching (@test-cache.R#164) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘usethis’
      All declared Imports should be used.
    ```

# styler

<details>

* Version: 1.3.2
* Source code: https://github.com/cran/styler
* URL: https://github.com/r-lib/styler
* BugReports: https://github.com/r-lib/styler/issues
* Date/Publication: 2020-02-23 05:50:02 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "styler")` for more info

</details>

## Newly broken

*   checking whether package ‘styler’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/styler/new/styler.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘styler’ ...
** package ‘styler’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in envlist(e) : attempt to use zero-length variable name
Calls: <Anonymous> ... <Anonymous> -> lazyLoadDBinsertValue -> <Anonymous> -> envlist
Execution halted
ERROR: lazy loading failed for package ‘styler’
* removing ‘/tmp/workdir/styler/new/styler.Rcheck/styler’

```
### CRAN

```
* installing *source* package ‘styler’ ...
** package ‘styler’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (styler)

```
# tibble

<details>

* Version: 3.0.2
* Source code: https://github.com/cran/tibble
* URL: https://tibble.tidyverse.org/, https://github.com/tidyverse/tibble
* BugReports: https://github.com/tidyverse/tibble/issues
* Date/Publication: 2020-07-07 13:00:02 UTC
* Number of recursive dependencies: 68

Run `cloud_details(, "tibble")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("tibble")
      Loading required package: tibble
      ── 1. Failure: as_tibble.matrix() supports .name_repair (@test-as_tibble.R#210) 
      `as_tibble(x)` did not produce any warnings.
      
      ── 2. Failure: supports compat col names (@test-matrix.R#44)  ──────────────────
      `out <- as_tibble(x)` did not produce any warnings.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1413 | SKIPPED: 108 | WARNINGS: 0 | FAILED: 2 ]
      1. Failure: as_tibble.matrix() supports .name_repair (@test-as_tibble.R#210) 
      2. Failure: supports compat col names (@test-matrix.R#44) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# yardstick

<details>

* Version: 0.0.6
* Source code: https://github.com/cran/yardstick
* URL: https://github.com/tidymodels/yardstick
* BugReports: https://github.com/tidymodels/yardstick/issues
* Date/Publication: 2020-03-17 18:50:02 UTC
* Number of recursive dependencies: 83

Run `cloud_details(, "yardstick")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 507 | SKIPPED: 1 | WARNINGS: 19 | FAILED: 28 ]
      1. Failure: Binary `f_meas()` returns `NA` with a warning when recall is undefined (tp + fn = 0) (#98) (@test-class-f_meas.R#73) 
      2. Failure: Binary `f_meas()` returns `NA` with a warning when recall is undefined (tp + fn = 0) (#98) (@test-class-f_meas.R#74) 
      3. Failure: Binary `f_meas()` returns `NA` with a warning when precision is undefined (tp + fp = 0) (#98) (@test-class-f_meas.R#90) 
      4. Failure: Binary `f_meas()` returns `NA` with a warning when precision is undefined (tp + fp = 0) (#98) (@test-class-f_meas.R#91) 
      5. Failure: Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when recall is undefined (tp + fn = 0) (#98) (@test-class-f_meas.R#102) 
      6. Failure: Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when recall is undefined (tp + fn = 0) (#98) (@test-class-f_meas.R#103) 
      7. Failure: Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when precision is undefined (tp + fn = 0) (#98) (@test-class-f_meas.R#114) 
      8. Failure: Multiclass `f_meas()` returns averaged value with `NA`s removed + a warning when precision is undefined (tp + fn = 0) (#98) (@test-class-f_meas.R#115) 
      9. Failure: Binary `precision()` returns `NA` with a warning when undefined (tp + fp = 0) (#98) (@test-class-precision.R#38) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

