# gompertztrunc

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/gompertztrunc
* Date/Publication: 2022-08-17 07:10:06 UTC
* Number of recursive dependencies: 121

Run `cloud_details(, "gompertztrunc")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gompertztrunc-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: diagnostic_plot
    > ### Title: Create diagnostic plots
    > ### Aliases: diagnostic_plot
    > 
    > ### ** Examples
    > 
    > # Create a single-cohort data set
    ...
    > gradient <- gompertztrunc::gompertz_mle(formula = death_age ~ finished_hs,
    + left_trunc = 1988, right_trunc = 2005, data = numident_c1920)
    > 
    > # Create diagnostic histogram plot using model outcome
    > gompertztrunc::diagnostic_plot(object = gradient, data = numident_c1920,
    + covar = "finished_hs", xlim = c(60, 95))
    Error in .shallow(x, cols = cols, retain.key = TRUE) : 
      attempt to set index 0/0 in SET_VECTOR_ELT
    Calls: <Anonymous> ... names<- -> names<-.data.table -> shallow -> .shallow
    Execution halted
    ```

# polished

<details>

* Version: 0.7.0
* GitHub: https://github.com/tychobra/polished
* Source code: https://github.com/cran/polished
* Date/Publication: 2022-08-25 11:20:02 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "polished")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: `app_uid` cannot be missing from request body
      Backtrace:
          ▆
       1. └─polished::update_app(...) at test-api-01-apps.R:57:2
       2.   └─polished::polished_api_res(resp)
      ── Error (test-api-01-apps.R:70:3): can delete an app ──────────────────────────
      Error: 
      Backtrace:
          ▆
       1. └─polished::delete_app(app_uid = test_app_info$uid) at test-api-01-apps.R:70:2
       2.   └─polished::polished_api_res(resp)
      
      [ FAIL 5 | WARN 0 | SKIP 0 | PASS 92 ]
      Error: Test failures
      Execution halted
    ```

