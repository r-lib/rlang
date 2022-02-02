# galah

<details>

* Version: 1.4.0
* GitHub: https://github.com/AtlasOfLivingAustralia/galah
* Source code: https://github.com/cran/galah
* Date/Publication: 2022-01-24 10:52:46 UTC
* Number of recursive dependencies: 163

Run `cloud_details(, "galah")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • On CRAN (32)
      • Slow test (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-galah_select.R:17:3): galah_select returns requested columns ──
      names(query) not equal to c("year", "basisOfRecord").
      Lengths differ: 9 is not 2
      ── Failure (test-galah_select.R:18:3): galah_select returns requested columns ──
      names(query) not equal to selected_columns[[1]].
      Lengths differ: 9 is not 2
      
      [ FAIL 2 | WARN 0 | SKIP 33 | PASS 228 ]
      Error: Test failures
      Execution halted
    ```

