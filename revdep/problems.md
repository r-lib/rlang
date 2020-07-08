# dplyr.teradata

<details>

* Version: 0.3.2
* Source code: https://github.com/cran/dplyr.teradata
* URL: https://github.com/hoxo-m/dplyr.teradata
* BugReports: https://github.com/hoxo-m/dplyr.teradata/issues
* Date/Publication: 2019-05-10 12:00:07 UTC
* Number of recursive dependencies: 63

Run `cloud_details(, "dplyr.teradata")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("dplyr.teradata")
      ── 1. Error: custom scalar translated correctly (@test-translate-teradata.R#9)  
      subscript out of bounds
      Backtrace:
        1. testthat::expect_equal(...)
       11. dbplyr:::case_when(x == 1L ~ 1L, x == 2L ~ 2L, TRUE ~ 3L)
       12. dbplyr:::sql_case_when(...)
       14. rlang::eval_bare(f[[2]], env)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: custom scalar translated correctly (@test-translate-teradata.R#9) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘bit64’ ‘rstudioapi’
      All declared Imports should be used.
    ```

# kuniezu

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/kuniezu
* URL: https://uribo.github.io/kuniezu/, https://github.com/uribo/kuniezu
* BugReports: https://github.com/uribo/kuniezu/issues
* Date/Publication: 2020-05-23 04:50:02 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "kuniezu")` for more info

</details>

## Newly broken

*   checking whether package ‘kuniezu’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/kuniezu/new/kuniezu.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘leaflet’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 95 marked UTF-8 strings
    ```

## Installation

### Devel

```
* installing *source* package ‘kuniezu’ ...
** package ‘kuniezu’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
Error in envlist(e) : attempt to use zero-length variable name
Calls: <Anonymous> ... <Anonymous> -> lazyLoadDBinsertValue -> <Anonymous> -> envlist
Execution halted
ERROR: lazy loading failed for package ‘kuniezu’
* removing ‘/tmp/workdir/kuniezu/new/kuniezu.Rcheck/kuniezu’

```
### CRAN

```
* installing *source* package ‘kuniezu’ ...
** package ‘kuniezu’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (kuniezu)

```
