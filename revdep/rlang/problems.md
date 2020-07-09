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

