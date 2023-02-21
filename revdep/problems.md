# dplyr

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidyverse/dplyr
* Source code: https://github.com/cran/dplyr
* Date/Publication: 2023-01-29 22:50:02 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "dplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("dplyr")
      [ FAIL 1 | WARN 0 | SKIP 311 | PASS 2743 ]
      
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • Can't set a non-UTF-8 encoding (4)
      • Can't use 'en_US' locale (2)
      • On CRAN (305)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-join-by.R:236'): nicely catches missing arguments when wrapped ──
      `fn(a)` did not throw the expected error.
      
      [ FAIL 1 | WARN 0 | SKIP 311 | PASS 2743 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 4 marked UTF-8 strings
    ```

# htmltools

<details>

* Version: 0.5.4
* GitHub: https://github.com/rstudio/htmltools
* Source code: https://github.com/cran/htmltools
* Date/Publication: 2022-12-07 20:50:06 UTC
* Number of recursive dependencies: 56

Run `revdepcheck::cloud_details(, "htmltools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test-all.R’
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      Actual message: "Argument 1 can't be empty."
      Backtrace:
          ▆
       1. ├─testthat::expect_error(as.character(div(, "one", )), "is empty") at test-tags.r:31:2
       2. │ └─testthat:::quasi_capture(...)
       3. │   ├─testthat (local) .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. ├─htmltools::div(, "one", )
       7. │ └─rlang::dots_list(...)
       8. └─rlang::abort(message = message)
      
      [ FAIL 2 | WARN 1 | SKIP 5 | PASS 2049 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking package dependencies ... NOTE
    ```
    Package which this enhances but not available for checking: ‘knitr’
    ```

# portalr

<details>

* Version: 0.3.11
* GitHub: https://github.com/weecology/portalr
* Source code: https://github.com/cran/portalr
* Date/Publication: 2022-12-01 17:40:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::cloud_details(, "portalr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─portalr::bait_presence_absence(path = portal_data_path, level = "plot") at test-10-summarize_ants.R:49:2
       2. │ ├─compute_presence(bait, level) %>% as.data.frame()
       3. │ └─portalr:::compute_presence(bait, level)
       4. │   └─... %>% ...
       5. ├─base::as.data.frame(.)
       6. ├─tidyr::complete(., !!!grouping, fill = list(presence = 0))
       7. ├─dplyr::mutate(., presence = 1)
       8. ├─dplyr::distinct(.)
       9. └─dplyr::select(., !!!grouping)
      
      [ FAIL 12 | WARN 43 | SKIP 42 | PASS 17 ]
      Error: Test failures
      Execution halted
    ```

# rapbase

<details>

* Version: 1.23.0
* GitHub: https://github.com/Rapporteket/rapbase
* Source code: https://github.com/cran/rapbase
* Date/Publication: 2022-08-17 14:20:02 UTC
* Number of recursive dependencies: 110

Run `revdepcheck::cloud_details(, "rapbase")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure ('test-github.R:6'): contributors are provided ──────────────────────
      class(getGithub("contributors", "rapbase")) not equal to "character".
      1/1 mismatches
      x[1]: "NULL"
      y[1]: "character"
      ── Failure ('test-github.R:10'): key can be provided ───────────────────────────
      grepl("ssh-rsa", getGithub("keys", "areedv")) is not TRUE
      
      `actual`:       
      `expected`: TRUE
      
      [ FAIL 2 | WARN 0 | SKIP 25 | PASS 190 ]
      Error: Test failures
      Execution halted
    ```

