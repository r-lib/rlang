# catchr

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/catchr
* Date/Publication: 2019-02-05 05:53:30 UTC
* Number of recursive dependencies: 41

Run `revdep_details(,"catchr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      27: abort(sprintf("`fn` must be an R function, not %s", friendly_type_of(x))) at /private/var/folders/b9/1vbq6rn93_1fk71sn95dqb8r0000gn/T/RtmpHbPH9o/R.INSTALL10261d354bde/rlang/R/fn.R:143
      
      simpleWarning in `force(expr)`: internal
      simpleWarning in `force(expr)`: internal
      ── 2. Failure: Ordered handlers respects order when with_handlers doesn't (@test
      `test_val` not equal to NULL.
      Types not compatible: character is not NULL
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 123 SKIPPED: 0 WARNINGS: 0 FAILED: 2
      1. Error: Explictly package-named functions (@test-testing.R#103) 
      2. Failure: Ordered handlers respects order when with_handlers doesn't (@test-testing.R#293) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# gestalt

<details>

* Version: 0.1.7
* Source code: https://github.com/cran/gestalt
* URL: https://github.com/egnha/gestalt
* BugReports: https://github.com/egnha/gestalt/issues
* Date/Publication: 2019-05-10 05:50:06 UTC
* Number of recursive dependencies: 39

Run `revdep_details(,"gestalt")` for more info

</details>

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: Passing an environment as data mask is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
    ```

# INDperform

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/INDperform
* URL: https://github.com/saskiaotto/INDperform
* BugReports: https://github.com/SaskiaAOtto/INDperform/issues
* Date/Publication: 2019-02-10 03:53:24 UTC
* Number of recursive dependencies: 73

Run `revdep_details(,"INDperform")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.3Mb
      sub-directories of 1Mb or more:
        data   3.6Mb
        help   1.1Mb
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      The conditions set in the crit_scores table for sub-criterion
      C9_1
      are not unique, i.e. conditions are met multiple times!
      Please correct your crit_score table before you continue.  variable required_data_type
      1      edf            numeric
        variable required_data_type
      1     r_sq            numeric
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 618 SKIPPED: 0 WARNINGS: 12 FAILED: 3
      1. Failure: check gams under different distributions (@test_calc_deriv.R#82) 
      2. Failure: test sample_boot (@test_cond_boot.R#69) 
      3. Failure: test sample_boot (@test_cond_boot.R#70) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘lazyeval’
      All declared Imports should be used.
    ```

# mice

<details>

* Version: 3.5.0
* Source code: https://github.com/cran/mice
* URL: http://stefvanbuuren.github.io/mice/ , http://www.stefvanbuuren.name , http://www.stefvanbuuren.name/fimd/
* BugReports: https://github.com/stefvanbuuren/mice/issues
* Date/Publication: 2019-05-13 21:20:20 UTC
* Number of recursive dependencies: 124

Run `revdep_details(,"mice")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
          cbind, rbind
      
      > 
      > test_check("mice")
      ── 1. Error: all arguments work (@test-ampute.R#64)  ───────────────────────────
      missing value where TRUE/FALSE needed
      1: ampute(data = dich.data, mech = "MNAR") at testthat/test-ampute.R:64
      2: ampute.continuous(P = P, scores = scores, prop = round(prop, 3), type = type) at /Users/lionel/Desktop/rlang/revdep/checks.noindex/mice/new/mice.Rcheck/00_pkg_src/mice/R/Ampute.R:499
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 271 SKIPPED: 0 WARNINGS: 0 FAILED: 1
      1. Error: all arguments work (@test-ampute.R#64) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# perturbatr

<details>

* Version: 1.2.1
* Source code: https://github.com/cran/perturbatr
* URL: https://github.com/cbg-ethz/perturbatr
* BugReports: https://github.com/cbg-ethz/perturbatr/issues
* Date/Publication: 2019-01-04
* Number of recursive dependencies: 81

Run `revdep_details(,"perturbatr")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.0Mb
      sub-directories of 1Mb or more:
        data   3.1Mb
    ```

# pmatch

<details>

* Version: 0.1.4
* Source code: https://github.com/cran/pmatch
* URL: https://github.com/mailund/pmatch
* BugReports: https://github.com/mailund/pmatch/issues
* Date/Publication: 2018-10-19 15:20:02 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"pmatch")` for more info

</details>

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: Passing an environment as data mask is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
    ```

## In both

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(pmatch)
      > 
      > test_check("pmatch")
      ── 1. Failure: we can transform a function that contains a call to cases (@test-
      tailr::can_loop_transform(llength) isn't true.
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 164 SKIPPED: 0 WARNINGS: 7 FAILED: 1
      1. Failure: we can transform a function that contains a call to cases (@test-transforms.R#62) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# rclimateca

<details>

* Version: 1.0.2
* Source code: https://github.com/cran/rclimateca
* URL: https://github.com/paleolimbot/rclimateca
* BugReports: https://github.com/paleolimbot/rclimateca/issues
* Date/Publication: 2018-06-11 16:41:23 UTC
* Number of recursive dependencies: 71

Run `revdep_details(,"rclimateca")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
                 site$stationid, theyear, err)
             message(msg)
             error_action(msg)
         }) at testthat/test-deprecated_functions.R:62
      7: tryCatchList(expr, classes, parentenv, handlers)
      8: tryCatchOne(expr, names, parentenv, handlers[[1L]])
      9: value[[3L]](cond)
      10: error_action(msg) at testthat/test-deprecated_functions.R:74
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      OK: 185 SKIPPED: 0 WARNINGS: 2 FAILED: 1
      1. Error: all timeframes and output types of data work for a random location (@test-deprecated_functions.R#90) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 24 marked UTF-8 strings
    ```

# recipes

<details>

* Version: 0.1.5
* Source code: https://github.com/cran/recipes
* URL: https://github.com/tidymodels/recipes
* BugReports: https://github.com/tidymodels/recipes/issues
* Date/Publication: 2019-03-21 08:50:03 UTC
* Number of recursive dependencies: 110

Run `revdep_details(,"recipes")` for more info

</details>

## Newly broken

*   checking whether package ‘recipes’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: Passing an environment wrapper like a function is deprecated.
    See ‘/Users/lionel/Desktop/rlang/revdep/checks.noindex/recipes/new/recipes.Rcheck/00install.out’ for details.
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘RcppRoll’
      All declared Imports should be used.
    ```

# tidytidbits

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/tidytidbits
* Date/Publication: 2019-02-11 14:20:02 UTC
* Number of recursive dependencies: 33

Run `revdep_details(,"tidytidbits")` for more info

</details>

## Newly broken

*   checking examples ... WARNING
    ```
    Found the following significant warnings:
    
      Warning: Passing an environment wrapper like a `quosure/formula` object is deprecated.
    Deprecated functions may be defunct as soon as of the next release
    of R.
    See ?Deprecated.
    ```

