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

# mosaic

<details>

* Version: 1.9.2
* GitHub: https://github.com/ProjectMOSAIC/mosaic
* Source code: https://github.com/cran/mosaic
* Date/Publication: 2025-07-30 17:20:02 UTC
* Number of recursive dependencies: 143

Run `revdepcheck::cloud_details(, "mosaic")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mosaic)
      Registered S3 method overwritten by 'mosaic':
        method                           from   
        fortify.SpatialPolygonsDataFrame ggplot2
      
      The 'mosaic' package masks several functions from core packages in order to add 
    ...
      Expected `names(do(2) * var(~cesd, data = mosaicData::HELPrct))` to equal "var".
      Differences:
      `actual`:   "result"
      `expected`: "var"   
      
      
      [ FAIL 2 | WARN 3 | SKIP 18 | PASS 246 ]
      Error:
      ! Test failures.
      Execution halted
    ```

## In both

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘Resampling.Rmd’ using rmarkdown
    
    Quitting from Resampling.Rmd:271-274 [hist]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    <error/rlang_error>
    Error:
    ! Problem while computing aesthetics.
    ℹ Error occurred in the 1st layer.
    Caused by error:
    ...
    l.71 \pagestyle
                   {fancy}^^M
    !  ==> Fatal error occurred, no output PDF file produced!
    --- failed re-building ‘MinimalRgg.Rnw’
    
    SUMMARY: processing the following files failed:
      ‘Resampling.Rmd’ ‘MinimalRgg.Rnw’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘manipulate’
    ```

*   checking installed package size ... NOTE
    ```
      installed size is  6.9Mb
      sub-directories of 1Mb or more:
        R     5.0Mb
        doc   1.2Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘cubature’
    ```

# pkggraph

<details>

* Version: 0.3.0
* GitHub: https://github.com/talegari/pkggraph
* Source code: https://github.com/cran/pkggraph
* Date/Publication: 2026-02-23 14:30:02 UTC
* Number of recursive dependencies: 69

Run `revdepcheck::cloud_details(, "pkggraph")` for more info

</details>

## Newly broken

*   checking whether package ‘pkggraph’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘checkmate::check_data_frame’ by ‘rlang::check_data_frame’ when loading ‘pkggraph’
      Warning: replacing previous import ‘checkmate::check_string’ by ‘rlang::check_string’ when loading ‘pkggraph’
    See ‘/tmp/workdir/pkggraph/new/pkggraph.Rcheck/00install.out’ for details.
    ```

# prt

<details>

* Version: 0.2.1
* GitHub: https://github.com/nbenn/prt
* Source code: https://github.com/cran/prt
* Date/Publication: 2025-09-03 21:50:16 UTC
* Number of recursive dependencies: 63

Run `revdepcheck::cloud_details(, "prt")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(prt)
      > 
      > if (requireNamespace("xml2")) {
      +   test_check("prt", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
      + } else {
      +   test_check("prt")
    ...
        9.     └─prt:::prt_lapply(...)
       10.       └─base::lapply(unclass(x), ...)
       11.         └─prt (local) FUN(X[[i]], ...)
       12.           └─prt:::fst_read(x, rows = rows, columns = j)
       13.             └─fst::read_fst(...)
      
      [ FAIL 1 | WARN 0 | SKIP 1 | PASS 255 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# rsimsum

<details>

* Version: 0.13.0
* GitHub: https://github.com/ellessenne/rsimsum
* Source code: https://github.com/cran/rsimsum
* Date/Publication: 2024-03-03 09:40:02 UTC
* Number of recursive dependencies: 128

Run `revdepcheck::cloud_details(, "rsimsum")` for more info

</details>

## Newly broken

*   checking whether package ‘rsimsum’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘checkmate::check_data_frame’ by ‘rlang::check_data_frame’ when loading ‘rsimsum’
      Warning: replacing previous import ‘checkmate::check_string’ by ‘rlang::check_string’ when loading ‘rsimsum’
    See ‘/tmp/workdir/rsimsum/new/rsimsum.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.5Mb
      sub-directories of 1Mb or more:
        data   2.0Mb
        doc    1.9Mb
        help   1.3Mb
    ```

# tf

<details>

* Version: 0.4.0
* GitHub: https://github.com/tidyfun/tf
* Source code: https://github.com/cran/tf
* Date/Publication: 2026-03-17 17:00:02 UTC
* Number of recursive dependencies: 136

Run `revdepcheck::cloud_details(, "tf")` for more info

</details>

## Newly broken

*   checking whether package ‘tf’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘checkmate::check_data_frame’ by ‘rlang::check_data_frame’ when loading ‘tf’
      Warning: replacing previous import ‘checkmate::check_string’ by ‘rlang::check_string’ when loading ‘tf’
    See ‘/tmp/workdir/tf/new/tf.Rcheck/00install.out’ for details.
    ```

# tidynorm

<details>

* Version: 0.4.0
* GitHub: https://github.com/JoFrhwld/tidynorm
* Source code: https://github.com/cran/tidynorm
* Date/Publication: 2025-10-26 19:50:02 UTC
* Number of recursive dependencies: 81

Run `revdepcheck::cloud_details(, "tidynorm")` for more info

</details>

## Newly broken

*   checking whether package ‘tidynorm’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘checkmate::check_data_frame’ by ‘rlang::check_data_frame’ when loading ‘tidynorm’
      Warning: replacing previous import ‘checkmate::check_string’ by ‘rlang::check_string’ when loading ‘tidynorm’
    See ‘/tmp/workdir/tidynorm/new/tidynorm.Rcheck/00install.out’ for details.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.2Mb
      sub-directories of 1Mb or more:
        doc    1.3Mb
        libs   2.2Mb
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 6748 marked UTF-8 strings
    ```

# uteals

<details>

* Version: 0.0.2
* GitHub: https://github.com/phuse-org/uteals
* Source code: https://github.com/cran/uteals
* Date/Publication: 2026-02-17 15:40:02 UTC
* Number of recursive dependencies: 270

Run `revdepcheck::cloud_details(, "uteals")` for more info

</details>

## Newly broken

*   checking whether package ‘uteals’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘checkmate::check_data_frame’ by ‘rlang::check_data_frame’ when loading ‘uteals’
      Warning: replacing previous import ‘checkmate::check_string’ by ‘rlang::check_string’ when loading ‘uteals’
    See ‘/tmp/workdir/uteals/new/uteals.Rcheck/00install.out’ for details.
    ```

