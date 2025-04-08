# codebookr

<details>

* Version: 0.1.8
* GitHub: https://github.com/brad-cannell/codebookr
* Source code: https://github.com/cran/codebookr
* Date/Publication: 2024-02-19 08:20:08 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "codebookr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(codebookr)
      > 
      > test_check("codebookr")
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 103 ]
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-cb_add_summary_stats.R:338:3'): The num_to_cat argument is working as expected ──
      class(cb_add_summary_stats(study, "three_cats", num_to_cat = 2)) not equal to c("summary_numeric", "tbl_df", "tbl", "data.frame").
      1/4 mismatches
      x[1]: "summary_few_cats"
      y[1]: "summary_numeric"
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 103 ]
      Error: Test failures
      Execution halted
    ```

# Haplin

<details>

* Version: 7.3.2
* GitHub: NA
* Source code: https://github.com/cran/Haplin
* Date/Publication: 2024-08-20 14:30:14 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "Haplin")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        R         1.5Mb
        extdata   3.0Mb
    ```

# keras

<details>

* Version: 2.15.0
* GitHub: https://github.com/rstudio/keras
* Source code: https://github.com/cran/keras
* Date/Publication: 2024-04-20 05:42:42 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "keras")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.4Mb
      sub-directories of 1Mb or more:
        R      1.5Mb
        doc    2.0Mb
        help   1.5Mb
    ```

# pivottabler

<details>

* Version: 1.5.5
* GitHub: https://github.com/cbailiss/pivottabler
* Source code: https://github.com/cran/pivottabler
* Date/Publication: 2023-10-01 16:20:02 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::cloud_details(, "pivottabler")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  7.2Mb
      sub-directories of 1Mb or more:
        R      2.0Mb
        data   4.0Mb
    ```

## In both

*   checking Rd files ... NOTE
    ```
    checkRd: (-1) PivotDataGroup.Rd:78: Lost braces; missing escapes or markup?
        78 | for the data group caption, default "{values}".}
           |                                      ^
    checkRd: (-1) PivotDataGroup.Rd:244: Lost braces; missing escapes or markup?
       244 | for the data group caption, default "{values}".}
           |                                      ^
    checkRd: (-1) PivotDataGroup.Rd:581: Lost braces; missing escapes or markup?
       581 | for the data group caption, default "{values}".}
           |                                      ^
    checkRd: (-1) PivotDataGroup.Rd:764: Lost braces; missing escapes or markup?
    ...
           |          ^
    checkRd: (-1) PivotTable.Rd:1060: Lost braces; missing escapes or markup?
      1060 | for the data group caption, default "{values}".}
           |                                      ^
    checkRd: (-1) PivotTable.Rd:1189: Lost braces; missing escapes or markup?
      1189 | default "{value}".}
           |          ^
    checkRd: (-1) PivotTable.Rd:1808: Lost braces; missing escapes or markup?
      1808 | `function(v, cell) { if(isTRUE(v>0.8)) return("green") }`.\cr
           |                    ^
    ```

