# autostats

<details>

* Version: 0.2.0
* GitHub: https://github.com/Harrison4192/autostats
* Source code: https://github.com/cran/autostats
* Date/Publication: 2021-12-10 09:30:02 UTC
* Number of recursive dependencies: 214

Run `cloud_details(, "autostats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autostats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: tidy_xgboost
    > ### Title: tidy xgboost
    > ### Aliases: tidy_xgboost
    > 
    > ### ** Examples
    > 
    > 
    ...
    Error:
    ! In metric: `roc_auc`
    No valid variables provided to `...`.
    Backtrace:
     1. iris_preds1 %>% ...
     2. autostats::eval_preds(., softprob_model = "xgb2_prob", yardstick::average_precision)
     7. yardstick eval_func(...)
     8. base::mapply(...)
     9. yardstick `<fn>`(dots[[1L]][[1L]], dots[[2L]][[1L]])
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘BBmisc’ ‘Ckmeans.1d.dp’ ‘Matrix’ ‘broom.mixed’ ‘ggstance’ ‘glmnet’
      ‘hardhat’
      All declared Imports should be used.
    ```

# catchr

<details>

* Version: 0.2.31
* GitHub: NA
* Source code: https://github.com/cran/catchr
* Date/Publication: 2021-09-23 16:40:05 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "catchr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        9. ├─catchr:::check_and_clean_input(d1 = base::acosh, spec_names = "acosh")
       10. │ ├─catchr:::warn_of_specials(get_used_specials(akw$kwargs, spec_names))
       11. │ └─catchr:::get_used_specials(akw$kwargs, spec_names)
       12. │   └─... %>% unique()
       13. ├─base::unique(.)
       14. ├─base::unlist(.)
       15. └─purrr::map(...)
       16.   └─catchr .f(.x[[i]], ...)
       17.     └─catchr:::find_used_symbols(!!get_expr(q), names_to_check, ...)
       18.       ├─base::withCallingHandlers(...)
       19.       └─catchr:::check_nodes(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 147 ]
      Error: Test failures
      Execution halted
    ```

# dm

<details>

* Version: 0.2.6
* GitHub: https://github.com/cynkra/dm
* Source code: https://github.com/cran/dm
* Date/Publication: 2021-11-21 15:40:02 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "dm")` for more info

</details>

## Newly broken

*   checking R code for possible problems ... NOTE
    ```
    dm_get_filtered_table: no visible binding for global variable ‘node’
    get_all_filtered_connected: no visible binding for global variable
      ‘node’
    Undefined global functions or variables:
      node
    ```

# dtplyr

<details>

* Version: 1.2.0
* GitHub: https://github.com/tidyverse/dtplyr
* Source code: https://github.com/cran/dtplyr
* Date/Publication: 2021-12-05 14:20:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "dtplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        5. │     │ └─base::withCallingHandlers(...)
        6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. ├─dplyr::slice_head(dt, 5)
        8. └─dtplyr:::slice_head.dtplyr_step(dt, 5)
        9.   └─ellipsis::check_dots_empty()
       10.     └─rlang:::action_dots(...)
       11.       ├─base try_dots(...)
       12.       └─rlang action(...)
      
      [ FAIL 1 | WARN 0 | SKIP 25 | PASS 603 ]
      Error: Test failures
      In addition: Warning messages:
      1: package 'dplyr' was built under R version 4.0.5 
      2: package 'tidyr' was built under R version 4.0.5 
      Execution halted
    ```

# equatiomatic

<details>

* Version: 0.3.0
* GitHub: https://github.com/datalorax/equatiomatic
* Source code: https://github.com/cran/equatiomatic
* Date/Publication: 2021-09-27 18:40:02 UTC
* Number of recursive dependencies: 139

Run `cloud_details(, "equatiomatic")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running massageExamples to create ‘equatiomatic-Ex.R’ failed
    Error:  object 'lifecycle' not found
    Execution halted
     NONE
    ```

*   checking Rd files ... WARNING
    ```
     object 'lifecycle' not found
     object 'lifecycle' not found
     object 'lifecycle' not found
    problems found in ‘extract_eq.Rd’, ‘print.equation.Rd’, ‘renderEq.Rd’
    ```

*   checking for unstated dependencies in examples ... WARNING
    ```
    Error:  object 'lifecycle' not found
    Execution halted
    ```

# fabricatr

<details>

* Version: 0.14.0
* GitHub: https://github.com/DeclareDesign/fabricatr
* Source code: https://github.com/cran/fabricatr
* Date/Publication: 2021-02-09 06:10:23 UTC
* Number of recursive dependencies: 38

Run `cloud_details(, "fabricatr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       5. └─fabricatr::fabricate(datasets::BOD, dd = demand * 2, ID_label = "Time")
       6.   └─fabricatr:::check_all_levels(dots)
       7.     └─base::vapply(options[is_function], call_name, "")
      ── Error (test-fabrication.R:266:3): nest_level call when there was no data to nest ──
      Error in `vapply(options[is_function], call_name, "")`: values must be length 1,
       but FUN(X[[1]]) result is length 0
      Backtrace:
          ▆
       1. └─fabricatr::fabricate(...) at test-fabrication.R:266:2
       2.   └─fabricatr:::check_all_levels(dots)
       3.     └─base::vapply(options[is_function], call_name, "")
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 383 ]
      Error: Test failures
      Execution halted
    ```

# gestalt

<details>

* Version: 0.1.8
* GitHub: https://github.com/egnha/gestalt
* Source code: https://github.com/cran/gestalt
* Date/Publication: 2019-06-27 08:20:03 UTC
* Number of recursive dependencies: 47

Run `cloud_details(, "gestalt")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-functions.R:65:3): body can be a closure ────────────────────────
      Error: Body must be an expression or closure
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(foo, fn(x ~ function(y) x + y)) at test-functions.R:65:2
       2. │ └─testthat::quasi_label(enquo(expected), expected.label, arg = "expected")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─gestalt::fn(x ~ function(y) x + y)
       5.   └─gestalt make_fn(fun$args, fun$body, ..env)
       6.     └─is_expression(body) %because% ...
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1040 ]
      Error: Test failures
      Execution halted
    ```

# nofrills

<details>

* Version: 0.3.1
* GitHub: https://github.com/egnha/nofrills
* Source code: https://github.com/cran/nofrills
* Date/Publication: 2021-01-08 19:50:05 UTC
* Number of recursive dependencies: 35

Run `cloud_details(, "nofrills")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘nofrills-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: as_fn
    > ### Title: Abbreviated functional arguments
    > ### Aliases: as_fn
    > 
    > ### ** Examples
    > 
    > call_fn <- function(.f, x) {
    ...
        ▆
     1. ├─global call_fn(.(. ~ (!!f)(.)^2), 1)
     2. │ └─nofrills::as_fn(.f)
     3. │   └─nofrills:::interpret_fn(x, match.fun(.f), parent.frame(2))
     4. │     └─base::eval(x, env)
     5. │       └─base::eval(x, env)
     6. └─nofrills `<fn>`(. ~ (!!f)(.)^2)
     7.   └─nofrills function_(d$args, d$body, ..env)
     8.     └─rlang::abort("Body must be an expression or closure.")
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       12.       └─rlang::abort(msg, call = call)
      ── Error (test-functions.R:48:3): body can be a closure ────────────────────────
      Error in `function_(d$args, d$body, ..env)`: Body must be an expression or closure.
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(fn(x ~ function(y) x + y), foo) at test-functions.R:48:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─nofrills::fn(x ~ function(y) x + y)
       5.   └─nofrills function_(d$args, d$body, ..env)
       6.     └─rlang::abort("Body must be an expression or closure.")
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 106 ]
      Error: Test failures
      Execution halted
    ```

# PVplr

<details>

* Version: 0.1.0
* GitHub: NA
* Source code: https://github.com/cran/PVplr
* Date/Publication: 2020-10-07 12:00:20 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "PVplr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘PVplr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: plr_weighted_regression
    > ### Title: Weighted Regression
    > ### Aliases: plr_weighted_regression
    > 
    > ### ** Examples
    > 
    > # build var_list
    ...
      4.   │   └─base::eval(mf, parent.frame())
      5.   ├─stats::model.frame(...)
      6.   └─stats::model.frame.default(...)
      7.     └─base::eval(extras, data, env)
      8.       └─base::eval(extras, data, env)
      9.         ├─wvar
     10.         └─rlang:::`$.rlang_fake_data_pronoun`(.data, wvar)
     11.           └─rlang:::stop_fake_data_subset(call)
     12.             └─rlang::abort(...)
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# qacBase

<details>

* Version: 1.0.2
* GitHub: https://github.com/rkabacoff/qacBase
* Source code: https://github.com/cran/qacBase
* Date/Publication: 2022-01-10 18:52:49 UTC
* Number of recursive dependencies: 111

Run `cloud_details(, "qacBase")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘qacBase-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: qstats
    > ### Title: Summary statistics for a quantitative variable
    > ### Aliases: qstats
    > 
    > ### ** Examples
    > 
    > # If no keyword arguments are provided, default values are used
    > qstats(mtcars, mpg, am, gear)
    Error in get(x, envir = env, mode = "function") : 
      object 'n' of mode 'function' was not found
    Calls: qstats ... my_sums -> <Anonymous> -> map -> .f -> <Anonymous> -> get
    Execution halted
    ```

# RAQSAPI

<details>

* Version: 2.0.2
* GitHub: https://github.com/USEPA/RAQSAPI
* Source code: https://github.com/cran/RAQSAPI
* Date/Publication: 2021-11-29 18:40:06 UTC
* Number of recursive dependencies: 99

Run `cloud_details(, "RAQSAPI")` for more info

</details>

## Newly broken

*   checking whether package ‘RAQSAPI’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/RAQSAPI/new/RAQSAPI.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘RAQSAPI’ ...
** package ‘RAQSAPI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: object ‘call_frame’ is not exported by 'namespace:rlang'
Execution halted
ERROR: lazy loading failed for package ‘RAQSAPI’
* removing ‘/tmp/workdir/RAQSAPI/new/RAQSAPI.Rcheck/RAQSAPI’


```
### CRAN

```
* installing *source* package ‘RAQSAPI’ ...
** package ‘RAQSAPI’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (RAQSAPI)


```
# survivalAnalysis

<details>

* Version: 0.2.0
* GitHub: NA
* Source code: https://github.com/cran/survivalAnalysis
* Date/Publication: 2021-04-24 12:20:05 UTC
* Number of recursive dependencies: 175

Run `cloud_details(, "survivalAnalysis")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘survivalAnalysis-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: forest_plot
    > ### Title: Forest plots for survival analysis.
    > ### Aliases: forest_plot forest_plot.df
    > 
    > ### ** Examples
    > 
    > library(magrittr)
    ...
     15. │   └─mask$eval_all_mutate(quo)
     16. ├─survivalAnalysis remove_sequential_duplicates_unless_breakafter(...)
     17. │ ├─base::ifelse(...)
     18. │ └─tidytidbits::sequential_duplicates(x, ordering = rev(order(ordered_index)))
     19. │   └─rlang::lgl_along(strings)
     20. │     └─rlang:::stop_defunct("`lgl_along()` is deprecated as of rlang 0.2.0.")
     21. │       └─base::stop(err)
     22. └─dplyr `<fn>`(`<dfnctErr>`)
     23.   └─rlang::abort(...)
    Execution halted
    ```

# tidyMicro

<details>

* Version: 1.47
* GitHub: https://github.com/CharlieCarpenter/tidyMicro
* Source code: https://github.com/cran/tidyMicro
* Date/Publication: 2020-09-13 17:10:03 UTC
* Number of recursive dependencies: 200

Run `cloud_details(, "tidyMicro")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidyMicro-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: alpha_div
    > ### Title: Alpha Diversity Calculations for tidy_micro
    > ### Aliases: alpha_div
    > 
    > ### ** Examples
    > 
    > data(bpd_phy); data(bpd_cla); data(bpd_ord); data(bpd_fam); data(bpd_clin)
    ...
     26. │         └─tidyselect:::eval_c(expr, data_mask, context_mask)
     27. │           └─tidyselect:::reduce_sels(node, data_mask, context_mask, init = init)
     28. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
     29. │               └─tidyselect:::eval_context(expr, context_mask)
     30. │                 └─rlang::eval_tidy(expr, context_mask)
     31. ├─rlang::.data$Lib
     32. └─rlang:::`$.rlang_fake_data_pronoun`(rlang::.data, Lib)
     33.   └─rlang:::stop_fake_data_subset(call)
     34.     └─rlang::abort(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       22. │     └─tidyselect:::vars_select_eval(...)
       23. │       └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
       24. │         └─tidyselect:::eval_c(expr, data_mask, context_mask)
       25. │           └─tidyselect:::reduce_sels(node, data_mask, context_mask, init = init)
       26. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
       27. │               └─tidyselect:::eval_context(expr, context_mask)
       28. │                 └─rlang::eval_tidy(expr, context_mask)
       29. ├─rlang::.data$Lib
       30. └─rlang:::`$.rlang_fake_data_pronoun`(rlang::.data, Lib)
       31.   └─rlang:::stop_fake_data_subset(call)
       32.     └─rlang::abort(...)
      
      [ FAIL 18 | WARN 0 | SKIP 2 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking whether package ‘tidyMicro’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: package ‘tidyverse’ was built under R version 4.0.4
      Warning: package ‘ggplot2’ was built under R version 4.0.5
      Warning: package ‘tibble’ was built under R version 4.0.5
      Warning: package ‘tidyr’ was built under R version 4.0.5
      Warning: package ‘readr’ was built under R version 4.0.5
      Warning: package ‘purrr’ was built under R version 4.0.5
      Warning: package ‘dplyr’ was built under R version 4.0.5
    See ‘/tmp/workdir/tidyMicro/new/tidyMicro.Rcheck/00install.out’ for details.
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Evomorph’ ‘cowplot’ ‘factoextra’ ‘gridExtra’ ‘lme4’ ‘lsr’ ‘plotly’
      ‘png’ ‘shapes’
      All declared Imports should be used.
    ```

# tidyselect

<details>

* Version: 1.1.1
* GitHub: https://github.com/r-lib/tidyselect
* Source code: https://github.com/cran/tidyselect
* Date/Publication: 2021-04-30 06:40:02 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "tidyselect")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      
      > 
      > test_check("tidyselect")
      ══ Skipped tests ═══════════════════════════════════════════════════════════════
      • FIXME (1)
      • Non-deterministic failures (1)
      • On CRAN (16)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-lifecycle-deprecated.R:73:3): empty selection signals a condition ──
      catch_cnd(vars_select(letters, starts_with("1"))) inherits from `'lifecycle_soft_deprecated'/'condition'` not `'character'`.
      
      [ FAIL 1 | WARN 2 | SKIP 18 | PASS 452 ]
      Error: Test failures
      Execution halted
    ```

# tidytidbits

<details>

* Version: 0.3.0
* GitHub: NA
* Source code: https://github.com/cran/tidytidbits
* Date/Publication: 2022-01-12 16:30:02 UTC
* Number of recursive dependencies: 29

Run `cloud_details(, "tidytidbits")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tidytidbits-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: replace_sequential_duplicates
    > ### Title: Replace sequential duplicates
    > ### Aliases: replace_sequential_duplicates
    > 
    > ### ** Examples
    > 
    > # returns c("a", "", "b", "", "", "a")
    > replace_sequential_duplicates(c("a", "a", "b", "b", "b", "a"))
    Error: `lgl_along()` is deprecated as of rlang 0.2.0.
    Execution halted
    ```

# Tplyr

<details>

* Version: 0.4.3
* GitHub: https://github.com/atorus-research/Tplyr
* Source code: https://github.com/cran/Tplyr
* Date/Publication: 2021-12-06 17:50:02 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "Tplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_error(...) at test-utils.R:5:2
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─Tplyr:::modify_nested_call(mean(c(1, 2, 3)))
        7.   └─rlang::call_name(c)
        8.     └─rlang:::check_call(call)
        9.       └─rlang:::stop_input_type(x, what, ..., arg = arg, call = call)
       10.         └─rlang::abort(message, ..., call = call)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 686 ]
      Error: Test failures
      Execution halted
    ```

