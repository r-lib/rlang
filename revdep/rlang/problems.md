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
# anglr

<details>

* Version: 0.6.0
* Source code: https://github.com/cran/anglr
* URL: https://github.com/hypertidy/anglr
* BugReports: https://github.com/hypertidy/anglr/issues
* Date/Publication: 2020-05-13 23:40:12 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "anglr")` for more info

</details>

## Newly broken

*   checking whether package ‘anglr’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: 'rgl.init' failed, running with 'rgl.useNULL = TRUE'.
    See ‘/tmp/workdir/anglr/new/anglr.Rcheck/00install.out’ for details.
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

