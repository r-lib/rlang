# coursekata

<details>

* Version: 0.19.2
* GitHub: https://github.com/coursekata/coursekata-r
* Source code: https://github.com/cran/coursekata
* Date/Publication: 2026-03-10 17:10:13 UTC
* Number of recursive dependencies: 133

Run `revdepcheck::cloud_details(, "coursekata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘coursekata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: middle
    > ### Title: Find a percentage of a distribution
    > ### Aliases: middle tails outer lower upper
    > 
    > ### ** Examples
    > 
    > 
    ...
     22. │   └─base::data.frame(x = x, original_pos = seq_along(x))
     23. │     ├─base::as.data.frame(x[[i]], optional = TRUE)
     24. │     └─base::as.data.frame.default(x[[i]], optional = TRUE)
     25. │       └─base::stop(...)
     26. └─base::.handleSimpleError(...)
     27.   └─rlang (local) h(simpleError(msg, call))
     28.     └─handlers[[1L]](cnd)
     29.       └─cli::cli_abort(...)
     30.         └─rlang::abort(...)
    Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘gf-squareplot.Rmd’ using rmarkdown
    
    Quitting from gf-squareplot.Rmd:106-114 [dgp]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error in `gf_squareplot()`:
    ! `x` must be numeric.
    ---
    ...
    
    Error: processing vignette 'gf-squareplot.Rmd' failed with diagnostics:
    `x` must be numeric.
    --- failed re-building ‘gf-squareplot.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘gf-squareplot.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# DeclareDesign

<details>

* Version: 1.1.0
* GitHub: https://github.com/DeclareDesign/DeclareDesign
* Source code: https://github.com/cran/DeclareDesign
* Date/Publication: 2025-10-15 08:30:15 UTC
* Number of recursive dependencies: 190

Run `revdepcheck::cloud_details(, "DeclareDesign")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(DeclareDesign)
      Loading required package: randomizr
      Loading required package: fabricatr
      Loading required package: estimatr
      > 
      > library(AER)
    ...
        │ 
        │ estimator <- declare_estimator(Y ~ Z, inquiry = my_inquiry) 
        │ 
        │ my_design <- construct_design(steps = steps) 
        │ 
      
      [ FAIL 9 | WARN 0 | SKIP 7 | PASS 638 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# ebvcube

<details>

* Version: 0.5.2
* GitHub: https://github.com/EBVcube/ebvcube
* Source code: https://github.com/cran/ebvcube
* Date/Publication: 2025-07-29 19:20:15 UTC
* Number of recursive dependencies: 114

Run `revdepcheck::cloud_details(, "ebvcube")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/tests.html
      > # * https://testthat.r-lib.org/reference/test_package.html#special-files
    ...
       3.     └─jsonlite:::parse_and_simplify(...)
       4.       └─jsonlite:::parseJSON(txt, bigint_as_char)
       5.         └─jsonlite:::parse_con(txt, bigint_as_char)
       6.           ├─base::open(con, "rb")
       7.           └─base::open.connection(con, "rb")
      
      [ FAIL 1 | WARN 11 | SKIP 0 | PASS 239 ]
      Error:
      ! Test failures.
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

# ggplot2

<details>

* Version: 4.0.2
* GitHub: https://github.com/tidyverse/ggplot2
* Source code: https://github.com/cran/ggplot2
* Date/Publication: 2026-02-03 08:50:23 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "ggplot2")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(ggplot2)
      > 
      > test_check("ggplot2")
      Saving _problems/test-plot-4.R
      [ FAIL 1 | WARN 0 | SKIP 270 | PASS 1777 ]
      
    ...
      'geom-dotplot/bin-y-dodged-coord-flip.svg', 'geom-dotplot/bin-y-dodged.svg',
      'geom-dotplot/bin-y-dodging-3-stackgroups-histodot.svg',
      'geom-dotplot/bin-y-three-x-groups-bins-aligned-across-groups.svg',
      'geom-dotplot/bin-y-three-x-groups-bins-aligned-coord-flip.svg',
      'geom-dotplot/bin-y-three-x-groups-fill-and-dodge.svg', …,
      'theme/vertical-legends-placed-apart.svg', and
      'theme/width-is-3-times-height.svg'
      Error:
      ! Test failures.
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘sp’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is 14.6Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        data   1.5Mb
        doc    7.6Mb
        help   1.7Mb
    ```

# MedDataSets

<details>

* Version: 0.1.0
* GitHub: https://github.com/lightbluetitan/meddatasets
* Source code: https://github.com/cran/MedDataSets
* Date/Publication: 2024-10-24 14:20:02 UTC
* Number of recursive dependencies: 107

Run `revdepcheck::cloud_details(, "MedDataSets")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  7.4Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        data   3.5Mb
    ```

# SmarterPoland

<details>

* Version: 1.8.1
* GitHub: NA
* Source code: https://github.com/cran/SmarterPoland
* Date/Publication: 2023-08-20 21:22:35 UTC
* Number of recursive dependencies: 29

Run `revdepcheck::cloud_details(, "SmarterPoland")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is 12.6Mb
      sub-directories of 1Mb or more:
        data  12.5Mb
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) cities_lon_lat.Rd:6: Lost braces
         6 | A subset of world.cities{maps}. Extracted in order to shink number of dependencies. 
           |                         ^
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 1122 marked UTF-8 strings
    ```

