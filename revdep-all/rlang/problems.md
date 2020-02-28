# ggplot2

<details>

* Version: 3.2.1
* Source code: https://github.com/cran/ggplot2
* URL: http://ggplot2.tidyverse.org, https://github.com/tidyverse/ggplot2
* BugReports: https://github.com/tidyverse/ggplot2/issues
* Date/Publication: 2019-08-10 22:30:13 UTC
* Number of recursive dependencies: 150

Run `revdep_details(,"ggplot2")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(ggplot2)
      > 
      > test_check("ggplot2")
      ── 1. Failure: as_facets_list() coerces lists (@test-facet-.r#40)  ─────────────
      `out` not identical to `exp`.
      Component 3: Attributes: < Component "class": Lengths (2, 1) differ (string compare on first 1) >
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 1097 | SKIPPED: 105 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: as_facets_list() coerces lists (@test-facet-.r#40) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        doc   3.8Mb
        R     2.1Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘mgcv’ ‘reshape2’ ‘viridisLite’
      All declared Imports should be used.
    ```

# RxODE

<details>

* Version: 0.9.1-9
* Source code: https://github.com/cran/RxODE
* URL: https://nlmixrdevelopment.github.io/RxODE/
* BugReports: https://github.com/nlmixrdevelopment/RxODE/issues
* Date/Publication: 2020-01-10 23:20:06 UTC
* Number of recursive dependencies: 121

Run `revdep_details(,"RxODE")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(testthat)
      > library(RxODE)
      > test_check("RxODE")
      ── 1. Failure: Issue #56 (@test-issue-56.R#11)  ────────────────────────────────
      dir.exists("/tmp/m1.d") isn't true.
      
      [====|====|====|====|====|====|====|====|====|====] 0:00:00 
      
      [====|====|====|====|====|====|====|====|====|====] 0:00:00 
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 876 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Failure: Issue #56 (@test-issue-56.R#11) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Packages suggested but not available for checking:
      'SnakeCharmR', 'installr'
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        libs   4.1Mb
        R      2.0Mb
    ```

