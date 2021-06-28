# arrow

<details>

* Version: 4.0.1
* GitHub: https://github.com/apache/arrow
* Source code: https://github.com/cran/arrow
* Date/Publication: 2021-05-28 09:50:02 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─arrow:::expect_dplyr_equal(...) test-dplyr.R:96:2
        2. │ └─rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(input = record_batch(tbl)))) helper-expectation.R:79:4
        3. ├─input %>% group_by(chr) %>% select() %>% collect()
        4. ├─dplyr::collect(.)
        5. └─arrow:::collect.arrow_dplyr_query(.)
        6.   └─arrow:::ensure_group_vars(x)
        7.     ├─arrow:::make_field_refs(gv, dataset = query_on_dataset(.data))
        8.     └─arrow:::query_on_dataset(.data)
        9.       ├─x$.data
       10.       └─rlang:::`$.rlang_fake_data_pronoun`(x, ".data")
       11.         └─rlang:::stop_fake_data_subset()
      
      [ FAIL 2 | WARN 0 | SKIP 60 | PASS 3778 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 58.8Mb
      sub-directories of 1Mb or more:
        R      3.6Mb
        libs  54.5Mb
    ```

# catchr

<details>

* Version: 0.2.3
* GitHub: NA
* Source code: https://github.com/cran/catchr
* Date/Publication: 2020-08-31 04:40:03 UTC
* Number of recursive dependencies: 59

Run `cloud_details(, "catchr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘catchr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: beep_with
    > ### Title: Play short sounds
    > ### Aliases: beep_with
    > 
    > ### ** Examples
    > 
    > warning_in_middle <- function() {
    ...
     10. │ └─catchr:::.f(.x[[i]], ...)
     11. │   └─catchr:::classify_arg(., spec_names)
     12. │     └─catchr:::approx_arg_name(!!arg)
     13. │       └─get_expr(enquo(x)) %>% expr_deparse(999) %>% paste(collapse = "")
     14. ├─base::paste(., collapse = "")
     15. └─rlang::expr_deparse(., 999)
     16.   └─rlang::check_dots_empty0(...)
     17.     └─rlang::check_dots_empty()
     18.       └─rlang:::action_dots(...)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        7. │ └─purrr::map2(...)
        8. ├─purrr::map(., ~classify_arg(., spec_names))
        9. │ └─catchr:::.f(.x[[i]], ...)
       10. │   └─catchr:::classify_arg(., spec_names)
       11. │     └─catchr:::approx_arg_name(!!arg)
       12. │       └─get_expr(enquo(x)) %>% expr_deparse(999) %>% paste(collapse = "")
       13. ├─base::paste(., collapse = "")
       14. └─rlang::expr_deparse(., 999)
       15.   └─rlang::check_dots_empty0(...)
       16.     └─rlang::check_dots_empty()
       17.       └─rlang:::action_dots(...)
      
      [ FAIL 18 | WARN 0 | SKIP 0 | PASS 22 ]
      Error: Test failures
      Execution halted
    ```

# ecochange

<details>

* Version: 1.3
* GitHub: NA
* Source code: https://github.com/cran/ecochange
* Date/Publication: 2020-10-13 15:00:02 UTC
* Number of recursive dependencies: 88

Run `cloud_details(, "ecochange")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘ecochange-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: gaugeIndicator
    > ### Title: Gauge Indicator
    > ### Aliases: gaugeIndicator
    > 
    > ### ** Examples
    > 
    > ## Warnings from GDAL/PROJ are suppressed.
    ...
      2. ├─base::plot(defareas)
      3. └─ecochange::plot.Indicator(defareas)
      4.   ├─ggplot2::ylab(unique(.data$metric))
      5.   │ └─ggplot2::labs(y = label)
      6.   │   └─rlang::dots_list(...)
      7.   ├─base::unique(.data$metric)
      8.   ├─<unknown>
      9.   └─rlang:::`$.rlang_fake_data_pronoun`(.data, "metric")
     10.     └─rlang:::stop_fake_data_subset()
    Execution halted
    ```

# heemod

<details>

* Version: 0.14.2
* GitHub: https://github.com/pierucci/heemod
* Source code: https://github.com/cran/heemod
* Date/Publication: 2021-01-22 13:00:02 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "heemod")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘heemod-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: run_dsa
    > ### Title: Run Sensitivity Analysis
    > ### Aliases: run_dsa
    > 
    > ### ** Examples
    > 
    > param <- define_parameters(
    ...
      8. ├─dplyr::mutate(...)
      9. ├─dplyr::left_join(...)
     10. ├─dplyr:::left_join.data.frame(...)
     11. │ └─dplyr:::join_mutate(...)
     12. │   └─dplyr:::join_cols(...)
     13. │     └─dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
     14. ├─.strategy_names
     15. └─rlang:::`$.rlang_fake_data_pronoun`(.data, .strategy_names)
     16.   └─rlang:::stop_fake_data_subset()
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8. ├─dplyr::arrange(., .data$.par_names, !!sym(var_plot))
        9. ├─dplyr::filter(., .data$.strategy_names %in% strategy)
       10. ├─dplyr::mutate(...)
       11. ├─dplyr::left_join(...)
       12. ├─dplyr:::left_join.data.frame(...)
       13. │ └─dplyr:::join_mutate(...)
       14. │   └─dplyr:::join_cols(...)
       15. │     └─dplyr:::standardise_join_by(by, x_names = x_names, y_names = y_names)
       16. ├─.strategy_names
       17. └─rlang:::`$.rlang_fake_data_pronoun`(.data, .strategy_names)
       18.   └─rlang:::stop_fake_data_subset()
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 503 ]
      Error: Test failures
      Execution halted
    ```

# ipmr

<details>

* Version: 0.0.2
* GitHub: https://github.com/levisc8/ipmr
* Source code: https://github.com/cran/ipmr
* Date/Publication: 2021-05-22 15:40:02 UTC
* Number of recursive dependencies: 69

Run `cloud_details(, "ipmr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error: missing value where TRUE/FALSE needed
      Backtrace:
          ▆
       1. ├─`%>%`(...)
       2. ├─ipmr::make_ipm(...)
       3. └─ipmr:::make_ipm.simple_dd_stoch_param(...)
       4.   └─ipmr:::.make_sub_kernel_simple_lazy(...)
       5.     └─ipmr:::.make_sub_kernel_simple_lazy_di(proto, main_env, return_envs)
       6.       └─ipmr:::.make_sub_kernel_simple(proto, env_list, return_envs = return_envs)
       7.         └─ipmr:::.extract_kernel_from_eval_env(...)
       8.           └─ipmr:::.valid_it_mat(out, kernel_id)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 266 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  5.1Mb
      sub-directories of 1Mb or more:
        doc    1.5Mb
        libs   2.5Mb
    ```

# pammtools

<details>

* Version: 0.5.7
* GitHub: https://github.com/adibender/pammtools
* Source code: https://github.com/cran/pammtools
* Date/Publication: 2021-06-21 13:00:02 UTC
* Number of recursive dependencies: 112

Run `cloud_details(, "pammtools")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       18. │             │     └─rlang:::endots(...)
       19. │             │       └─rlang:::map(...)
       20. │             │         └─base::lapply(.x, .f, ...)
       21. │             │           └─rlang:::FUN(X[[i]], ...)
       22. │             │             └─rlang::splice(...)
       23. │             └─(function (arg) ...
       24. ├─tibble::as_tibble(.)
       25. └─purrr::reduce(., left_join)
       26.   └─purrr:::reduce_impl(.x, .f, ..., .init = .init, .dir = .dir)
       27.     └─purrr:::reduce_init(.x, .init, left = left)
       28.       └─rlang::is_empty(x)
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 344 ]
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
      3.   │ └─base::eval(mf, parent.frame())
      4.   │   └─base::eval(mf, parent.frame())
      5.   ├─stats::model.frame(...)
      6.   └─stats::model.frame.default(...)
      7.     └─base::eval(extras, data, env)
      8.       └─base::eval(extras, data, env)
      9.         ├─wvar
     10.         └─rlang:::`$.rlang_fake_data_pronoun`(.data, wvar)
     11.           └─rlang:::stop_fake_data_subset()
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘ggplot2’
      All declared Imports should be used.
    ```

# ricu

<details>

* Version: 0.4.0
* GitHub: https://github.com/eth-mds/ricu
* Source code: https://github.com/cran/ricu
* Date/Publication: 2021-05-20 00:30:15 UTC
* Number of recursive dependencies: 98

Run `cloud_details(, "ricu")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─testthat:::expect_condition_matching(...)
        3. │   └─testthat:::quasi_capture(...)
        4. │     ├─testthat:::.capture(...)
        5. │     │ └─base::withCallingHandlers(...)
        6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. ├─ricu::as_ts_tbl(dat, index_var = "d")
        8. └─ricu:::as_ts_tbl.default(dat, index_var = "d")
        9.   ├─ricu::as_ts_tbl(...)
       10.   └─ricu:::as_ts_tbl.data.table(...)
       11.     └─ricu:::new_ts_tbl(x, id_vars, index_var, interval, by_ref = by_ref)
       12.       └─ricu:::assert_that(is.string(index_var), has_time_cols(x, index_var))
      
      [ FAIL 2 | WARN 0 | SKIP 6 | PASS 558 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘glue’
      All declared Imports should be used.
    ```

# sketch

<details>

* Version: 1.0.3
* GitHub: https://github.com/kcf-jackson/sketch
* Source code: https://github.com/cran/sketch
* Date/Publication: 2020-10-08 07:40:03 UTC
* Number of recursive dependencies: 67

Run `cloud_details(, "sketch")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘sketch-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: print.rule
    > ### Title: Print function for 'rule' objects
    > ### Aliases: print.rule
    > 
    > ### ** Examples
    > 
    > rule_1 <- make_rule("+", "Math.add")
    > print(rule_1)
    Error in cat(x, ..., sep = sep) : 
      argument 1 (type 'closure') cannot be handled by 'cat'
    Calls: print -> print.rule -> cat
    Execution halted
    ```

# table.express

<details>

* Version: 0.3.1
* GitHub: https://github.com/asardaes/table.express
* Source code: https://github.com/cran/table.express
* Date/Publication: 2019-09-07 11:10:02 UTC
* Number of recursive dependencies: 50

Run `cloud_details(, "table.express")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─`%>%`(...)
        2. ├─table.express::end_expr(.)
        3. ├─table.express::transmute_sd(., mad(.COL, ...), low = TRUE, .SDcols = sd_cols)
        4. └─table.express:::transmute_sd.ExprBuilder(...)
        5.   └─table.express:::standardize_calls(...)
        6.     └─base::lapply(...)
        7.       └─table.express:::FUN(X[[i]], ...)
        8.         └─rlang::call_standardise(.expr, .env)
        9.           └─rlang::call_match(expr, fn, defaults = defaults, dots_env = dots_env)
       10.             └─base::match.call(fn, call, expand.dots = FALSE, envir = dots_env)
      
      [ FAIL 3 | WARN 0 | SKIP 1 | PASS 588 ]
      Error: Test failures
      Execution halted
    ```

# tidyMicro

<details>

* Version: 1.47
* GitHub: https://github.com/CharlieCarpenter/tidyMicro
* Source code: https://github.com/cran/tidyMicro
* Date/Publication: 2020-09-13 17:10:03 UTC
* Number of recursive dependencies: 187

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
     25. │       └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
     26. │         └─tidyselect:::eval_c(expr, data_mask, context_mask)
     27. │           └─tidyselect:::reduce_sels(node, data_mask, context_mask, init = init)
     28. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
     29. │               └─tidyselect:::eval_context(expr, context_mask)
     30. │                 └─rlang::eval_tidy(expr, context_mask)
     31. ├─rlang::.data$Lib
     32. └─rlang:::`$.rlang_fake_data_pronoun`(rlang::.data, Lib)
     33.   └─rlang:::stop_fake_data_subset()
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       21. │     │   └─base::withCallingHandlers(...)
       22. │     └─tidyselect:::vars_select_eval(...)
       23. │       └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
       24. │         └─tidyselect:::eval_c(expr, data_mask, context_mask)
       25. │           └─tidyselect:::reduce_sels(node, data_mask, context_mask, init = init)
       26. │             └─tidyselect:::walk_data_tree(new, data_mask, context_mask)
       27. │               └─tidyselect:::eval_context(expr, context_mask)
       28. │                 └─rlang::eval_tidy(expr, context_mask)
       29. ├─rlang::.data$Lib
       30. └─rlang:::`$.rlang_fake_data_pronoun`(rlang::.data, Lib)
       31.   └─rlang:::stop_fake_data_subset()
      
      [ FAIL 18 | WARN 0 | SKIP 2 | PASS 2 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘Evomorph’ ‘cowplot’ ‘factoextra’ ‘gridExtra’ ‘lme4’ ‘lsr’ ‘plotly’
      ‘png’ ‘shapes’
      All declared Imports should be used.
    ```

# unpivotr

<details>

* Version: 0.6.1
* GitHub: https://github.com/nacnudus/unpivotr
* Source code: https://github.com/cran/unpivotr
* Date/Publication: 2020-08-03 22:50:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "unpivotr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘unpivotr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: behead
    > ### Title: Strip a level of headers from a pivot table
    > ### Aliases: behead behead_if
    > 
    > ### ** Examples
    > 
    > # A simple table with a row of headers
    ...
    3     3     1 int       <NA>      2
    4     1     2 chr       b        NA
    5     2     2 int       <NA>      3
    6     3     2 int       <NA>      4
    > 
    > # Strip the cells in row 1 (the original headers) and use them as data
    > behead(cells, "N", foo)
    Error in (function (arg)  : use of NULL environment is defunct
    Calls: behead ... <Anonymous> -> enexpr -> <Anonymous> -> <Anonymous>
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_true(...) test-tibble.R:24:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─tibble::is_tibble(enhead(data_cells, col_headers$`1`, "ABOVE"))
        5. ├─unpivotr::enhead(data_cells, col_headers$`1`, "ABOVE")
        6. └─unpivotr:::enhead.data.frame(data_cells, col_headers$`1`, "ABOVE")
        7.   ├─base::do.call(direction, list(data_cells, header_cells))
        8.   └─unpivotr:::`up-ish`(...)
        9.     └─unpivotr:::side_join(data_cells, header_cells, "up-left", drop)
       10.       └─rlang::as_function(corner)
       11.         └─base::get(x, envir = env, mode = "function")
      
      [ FAIL 23 | WARN 0 | SKIP 0 | PASS 135 ]
      Error: Test failures
      Execution halted
    ```

