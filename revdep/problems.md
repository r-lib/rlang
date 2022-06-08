# gghighlight

<details>

* Version: 0.3.3
* GitHub: https://github.com/yutannihilation/gghighlight
* Source code: https://github.com/cran/gghighlight
* Date/Publication: 2022-06-06 20:10:11 UTC
* Number of recursive dependencies: 81

Run `cloud_details(, "gghighlight")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. └─gghighlight:::expect_equal_layers(...) at test-gghighlight.R:260:2
       2.   └─purrr::walk2(x, y, expect_equal_layer) at tests/testthat/helpers.R:14:2
       3.     └─purrr::map2(.x, .y, .f, ...)
       4.       └─gghighlight (local) .f(.x[[i]], .y[[i]], ...)
       5.         └─testthat::expect_equal(as.list(x), as.list(y), ignore_formula_env = TRUE) at tests/testthat/helpers.R:10:2
      
      [ FAIL 38 | WARN 0 | SKIP 1 | PASS 153 ]
      Deleting unused snapshots:
      • vdiffr/simple-bar-chart-with-facet.svg
      • vdiffr/simple-line-chart.svg
      • vdiffr/simple-point-chart.svg
      Error: Test failures
      Execution halted
    ```

