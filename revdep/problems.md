# bridgr

<details>

* Version: 0.1.2
* GitHub: https://github.com/marcburri/bridgr
* Source code: https://github.com/cran/bridgr
* Date/Publication: 2026-02-18 18:50:02 UTC
* Number of recursive dependencies: 79

Run `revdepcheck::cloud_details(, "bridgr")` for more info

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
          ▆
       1. └─bridgr::bridge(...) at test-bridge.R:62:3
       2.   └─stats::optim(...)
       3.     └─stats (local) `<fn>`(`<dbl>`)
       4.       └─bridgr (local) fn(par, ...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 13 ]
      Error:
      ! Test failures.
      Execution halted
    ```

# meta

<details>

* Version: 8.5-0
* GitHub: https://github.com/guido-s/meta
* Source code: https://github.com/cran/meta
* Date/Publication: 2026-05-25 05:10:02 UTC
* Number of recursive dependencies: 141

Run `revdepcheck::cloud_details(, "meta")` for more info

</details>

## Newly broken

*   checking installed package size ... NOTE
    ```
      installed size is  5.6Mb
      sub-directories of 1Mb or more:
        R      3.5Mb
        help   1.5Mb
    ```

## In both

*   checking Rd cross-references ... NOTE
    ```
    Packages unavailable to check Rd xrefs: ‘metasens’, ‘robumeta’
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
* Number of recursive dependencies: 66

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

