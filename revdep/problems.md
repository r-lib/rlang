# eiCompare

<details>

* Version: 3.0.4
* GitHub: https://github.com/RPVote/eiCompare
* Source code: https://github.com/cran/eiCompare
* Date/Publication: 2023-08-31 13:30:02 UTC
* Number of recursive dependencies: 147

Run `revdepcheck::cloud_details(, "eiCompare")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(eiCompare)
      Loading required package: eiPack
      Loading required package: ei
      Loading required package: wru
      > 
      > test_check("eiCompare")
    ...
      } else {
          stop()
      }`: argument is of length zero
      Backtrace:
          ▆
       1. └─eiCompare::wru_predict_race_wrapper(...) at test_wru_predict_race_wrapper.R:19:3
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 75 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘bisg.Rmd’ using rmarkdown
    
    Quitting from lines 164-175 [unnamed-chunk-16] (bisg.Rmd)
    Error: processing vignette 'bisg.Rmd' failed with diagnostics:
    no applicable method for 'tbl_vars' applied to an object of class "NULL"
    --- failed re-building ‘bisg.Rmd’
    
    --- re-building ‘ei.Rmd’ using rmarkdown
    --- finished re-building ‘ei.Rmd’
    ...
    Quitting from lines 235-263 [performance_analysis] (performance_analysis.Rmd)
    Error: processing vignette 'performance_analysis.Rmd' failed with diagnostics:
    No columns selected for aggregation.
    --- failed re-building ‘performance_analysis.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘bisg.Rmd’ ‘performance_analysis.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

