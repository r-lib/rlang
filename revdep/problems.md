# atrrr

<details>

* Version: 0.1.1
* GitHub: https://github.com/JBGruber/atrrr
* Source code: https://github.com/cran/atrrr
* Date/Publication: 2025-07-22 14:03:30 UTC
* Number of recursive dependencies: 96

Run `revdepcheck::cloud_details(, "atrrr")` for more info

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
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
      > # * https://testthat.r-lib.org/articles/special-files.html
    ...
      ── Error ('test-lists.R:28:3'): test getting list feed ─────────────────────────
      <purrr_error_indexed/rlang_error/error/condition>
      Error in `purrr::map_chr(link, function(l) {     if (is_at(l))          return(l)     http_info <- parse_http_url(l)     if (is.na(http_info$repo) | is.na(http_info$rkey))          return(NA_character_)     if (!is_did(http_info$repo)) {         http_info$repo <- resolve_handle(http_info$repo, .token = .token)     }     glue::glue_data(http_info, "at://{repo}/{collection}/{rkey}") })`: i In index: 1.
      Caused by error in `mock()`:
      ! file recorded_responses/com.atproto.identity.resolveHandle_3a422.rds does not exist and no token exists
      
      [ FAIL 19 | WARN 0 | SKIP 2 | PASS 12 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# plotly

<details>

* Version: 4.12.0
* GitHub: https://github.com/plotly/plotly.R
* Source code: https://github.com/cran/plotly
* Date/Publication: 2026-01-24 07:50:02 UTC
* Number of recursive dependencies: 134

Run `revdepcheck::cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library("testthat")
      > library("plotly")
      Loading required package: ggplot2
      
      Attaching package: 'plotly'
      
      The following object is masked from 'package:ggplot2':
    ...
      Expected `legend_title` to match regexp "factor\\(cyl\\)$".
      Actual text:
      ✖ │ factor(cyl)<br />factor(vs)
      
      [ FAIL 2 | WARN 23 | SKIP 63 | PASS 1449 ]
      Deleting unused snapshots: 'ggplot-contour/contour.svg' and
      'ggplot-heatmap/heatmap.svg'
      Error:
      ! Test failures.
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.7Mb
      sub-directories of 1Mb or more:
        R             1.1Mb
        examples      1.3Mb
        htmlwidgets   3.9Mb
    ```

# rollama

<details>

* Version: 0.3.0
* GitHub: https://github.com/JBGruber/rollama
* Source code: https://github.com/cran/rollama
* Date/Publication: 2026-03-25 20:30:02 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::cloud_details(, "rollama")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > # This file is part of the standard setup for testthat.
      > # It is recommended that you do not modify it.
      > #
      > # Where should you do additional test configuration?
      > # Learn more about the roles of various files in:
      > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
    ...
      Expected `req_hash(make_fake_req())` to equal "0227ca5af8ae565c3383d248a8b1010a".
      Differences:
      `actual`:   "aa8987c18b69ccd868e451b5426aac70"
      `expected`: "0227ca5af8ae565c3383d248a8b1010a"
      
      
      [ FAIL 1 | WARN 0 | SKIP 27 | PASS 26 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# saros

<details>

* Version: 1.6.1
* GitHub: https://github.com/NIFU-NO/saros
* Source code: https://github.com/cran/saros
* Date/Publication: 2026-01-28 03:50:02 UTC
* Number of recursive dependencies: 120

Run `revdepcheck::cloud_details(, "saros")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(saros)
      > 
      > testthat::test_check("saros")
      Starting 2 test processes.
      > test-crowd_plots_as_tabset.R: dev.new(): using pdf(file="Rplots1.pdf")
    ...
      Expected `grepl(x = as.character(result), "\\[download figure data\\]\\(.+/d0487363db4e6cc64fdb740cb6617fc0\\.rds\\)$")` to be TRUE.
      Differences:
      `actual`:   FALSE
      `expected`: TRUE 
      
      
      [ FAIL 3 | WARN 0 | SKIP 21 | PASS 1021 ]
      Error:
      ! Test failures.
      Execution halted
    ```

