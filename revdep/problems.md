# arrow

<details>

* Version: 5.0.0.2
* GitHub: https://github.com/apache/arrow
* Source code: https://github.com/cran/arrow
* Date/Publication: 2021-09-05 04:30:22 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Failure (test-dplyr-filter.R:325:3): Filtering with unsupported functions ───
      `via_table <- rlang::eval_tidy(expr, rlang::new_data_mask(rlang::env(input = Table$create(tbl))))` produced unexpected warnings.
      Expected match: \\* In nchar\\(chr, type = "bytes", allowNA = TRUE\\) == 1, allowNA = TRUE not supported by Arrow\n\\* Expression pnorm\\(dbl\\) > 0.99 not supported in Arrow\npulling data into R
      Actual values:
      * * In ... == 1, allowNA = TRUE not supported by Arrow
      * Expression pnorm(dbl) > 0.99 not supported in Arrow
      pulling data into R
      Backtrace:
          ▆
       1. └─arrow:::expect_dplyr_equal(...) at test-dplyr-filter.R:325:2
       2.   └─testthat::expect_warning(...) at tests/testthat/helper-expectation.R:114:4
      
      [ FAIL 2 | WARN 0 | SKIP 53 | PASS 4807 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 66.3Mb
      sub-directories of 1Mb or more:
        R      3.6Mb
        libs  61.8Mb
    ```

# assemblerr

<details>

* Version: 0.1.0
* GitHub: https://github.com/UUPharmacometrics/assemblerr
* Source code: https://github.com/cran/assemblerr
* Date/Publication: 2021-07-28 11:40:05 UTC
* Number of recursive dependencies: 55

Run `cloud_details(, "assemblerr")` for more info

</details>

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Missing or unexported object: ‘rlang::as_pairlist’
    ```

# bignum

<details>

* Version: 0.2.2
* GitHub: https://github.com/davidchall/bignum
* Source code: https://github.com/cran/bignum
* Date/Publication: 2021-09-21 16:00:02 UTC
* Number of recursive dependencies: 45

Run `cloud_details(, "bignum")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘bignum-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: biginteger
    > ### Title: Arbitrary-Precision Integer Vectors
    > ### Aliases: biginteger as_biginteger is_biginteger
    > 
    > ### ** Examples
    > 
    > # default options limit displayed precision
    ...
    > # display full precision
    > format(biginteger(2)^50L, notation = "dec")
    [1] "1125899906842624"
    > 
    > # lossy casts raise a warning
    > biginteger(c(2, 2.5, 3))
    Error in stop_vctrs(message, x = x, y = to, to = to, result = result,  : 
      could not find function "stop_vctrs"
    Calls: biginteger ... <Anonymous> -> warning_cnd -> cnd_fields -> dots_list
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       18. │           └─vctrs:::stop_lossy_cast(...)
       19. │             └─vctrs:::stop_vctrs(...)
       20. │               └─rlang::abort(message, class = c(class, "vctrs_error"), ...)
       21. │                 └─rlang:::signal_abort(cnd, .file)
       22. │                   └─base::signalCondition(cnd)
       23. └─bignum `<fn>`(`<vctrs___>`)
       24.   ├─base::do.call(warn, condition_data)
       25.   └─rlang `<fn>`(...)
       26.     └─rlang::warning_cnd(...)
       27.       └─rlang:::cnd_fields(...)
       28.         └─rlang::dots_list(...)
      
      [ FAIL 6 | WARN 0 | SKIP 22 | PASS 344 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 14.1Mb
      sub-directories of 1Mb or more:
        libs  13.8Mb
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

# cmstatr

<details>

* Version: 0.9.0
* GitHub: https://github.com/cmstatr/cmstatr
* Source code: https://github.com/cran/cmstatr
* Date/Publication: 2021-07-01 05:10:02 UTC
* Number of recursive dependencies: 85

Run `cloud_details(, "cmstatr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Snapshot of code has changed:
      old[1:6] vs new[1:6]
        "Code"
        "  res <- basis_normal(x = x, batch = batch)"
      - "Warning <warning>"
      + "Warning <rlang_warning>"
        "  `outliers_within_batch` failed: Maximum normed residual test detected outliers within one or more batch"
        "  `between_batch_variability` failed: Anderson-Darling k-Sample test indicates that batches are drawn from different distributions"
        "  `outliers` failed: Maximum normed residual test detected outliers within data"
      
      Run `snapshot_accept('basis')` if this is a deliberate change
      
      [ FAIL 1 | WARN 1 | SKIP 30 | PASS 1116 ]
      Error: Test failures
      Execution halted
    ```

# dtplyr

<details>

* Version: 1.1.0
* GitHub: https://github.com/tidyverse/dtplyr
* Source code: https://github.com/cran/dtplyr
* Date/Publication: 2021-02-20 01:50:05 UTC
* Number of recursive dependencies: 61

Run `cloud_details(, "dtplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘dtplyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: filter.dtplyr_step
    > ### Title: Subset rows using column values
    > ### Aliases: filter.dtplyr_step
    > 
    > ### ** Examples
    > 
    > library(dplyr, warn.conflicts = FALSE)
    ...
    
    # Use as.data.table()/as.data.frame()/as_tibble() to access results
    > 
    > dt %>%
    +   group_by(cyl) %>%
    +   filter(mpg > mean(mpg))
    Error in step_subset(parent, i = i) : 
      is.null(i) || is_expression(i) || is_step(i) is not TRUE
    Calls: %>% ... filter.dtplyr_step -> step_subset_i -> step_subset -> stopifnot
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        1. ├─testthat::expect_equal(...) at test-tidyeval.R:202:2
        2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
        3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
        4. ├─... %>% pull()
        5. ├─dplyr::pull(.)
        6. └─dplyr::summarise_at(., vars(x), add)
        7.   ├─dplyr::summarise(.tbl, !!!funs)
        8.   └─dtplyr:::summarise.dtplyr_step(.tbl, !!!funs)
        9.     └─dtplyr:::step_subset_j(...)
       10.       └─dtplyr:::step_subset(...)
       11.         └─base::stopifnot(is.null(j) || is_expression(j))
      
      [ FAIL 6 | WARN 0 | SKIP 12 | PASS 335 ]
      Error: Test failures
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
      Error: values must be length 1,
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
       6.     └─is_expression(body) %because% "Body must be an expression or closure"
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 1040 ]
      Error: Test failures
      Execution halted
    ```

# ggh4x

<details>

* Version: 0.2.0
* GitHub: https://github.com/teunbrand/ggh4x
* Source code: https://github.com/cran/ggh4x
* Date/Publication: 2021-08-21 09:50:02 UTC
* Number of recursive dependencies: 70

Run `cloud_details(, "ggh4x")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected match: "The `nonsense` package is required"
      Actual message: "The package `nonsense` is required for `test`."
      Backtrace:
          ▆
       1. ├─testthat::expect_error(try_require("nonsense", "test"), text) at test-utils.R:7:2
       2. │ └─testthat:::quasi_capture(...)
       3. │   ├─testthat .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. └─ggh4x:::try_require("nonsense", "test")
       7.   └─rlang::check_installed(package, paste0("for `", fun, "`.\n"))
      
      [ FAIL 1 | WARN 1 | SKIP 0 | PASS 743 ]
      Error: Test failures
      Execution halted
    ```

# heemod

<details>

* Version: 0.14.2
* GitHub: https://github.com/pierucci/heemod
* Source code: https://github.com/cran/heemod
* Date/Publication: 2021-01-22 13:00:02 UTC
* Number of recursive dependencies: 117

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
     16.   └─rlang:::stop_fake_data_subset(call)
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
       18.   └─rlang:::stop_fake_data_subset(call)
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 503 ]
      Error: Test failures
      Execution halted
    ```

# icecream

<details>

* Version: 0.1.1
* GitHub: https://github.com/lewinfox/icecream
* Source code: https://github.com/cran/icecream
* Date/Publication: 2021-05-31 07:40:02 UTC
* Number of recursive dependencies: 31

Run `cloud_details(, "icecream")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > library(icecream)
      > 
      > test_check("icecream")
      i ic| <env: global>
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-icecream.R:76:3): source file is correctly identified ─────────
      `f()` did not throw the expected message.
      Backtrace:
          ▆
       1. └─testthat::expect_message(f(), regexp = "my_name_is_inigo_montoya") at test-icecream.R:76:2
       2.   └─testthat:::expect_condition_matching(...)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 19 ]
      Error: Test failures
      Execution halted
    ```

# lares

<details>

* Version: 5.0.2
* GitHub: https://github.com/laresbernardo/lares
* Source code: https://github.com/cran/lares
* Date/Publication: 2021-09-10 13:40:02 UTC
* Number of recursive dependencies: 142

Run `cloud_details(, "lares")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lares-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: distr
    > ### Title: Compare Variables with their Distributions
    > ### Aliases: distr
    > 
    > ### ** Examples
    > 
    > Sys.unsetenv("LARES_FONT") # Temporal
    ...
    > dft %>% distr(Survived, Sex)
    Error in `distr()`: Can't subset `.data` outside of a data mask context.
    Backtrace:
        ▆
     1. ├─dft %>% distr(Survived, Sex)
     2. └─lares::distr(., Survived, Sex)
     3.   ├─<unknown>
     4.   └─rlang:::`$.rlang_fake_data_pronoun`(.data, "value")
     5.     └─rlang:::stop_fake_data_subset(call)
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
    Backtrace:
        ▆
     1. ├─global call_fn(.(. ~ (!!f)(.)^2), 1)
     2. │ └─nofrills::as_fn(.f)
     3. │   └─nofrills:::interpret_fn(x, match.fun(.f), parent.frame(2))
     4. │     └─base::eval(x, env)
     5. │       └─base::eval(x, env)
     6. └─nofrills `<fn>`(. ~ (!!f)(.)^2)
     7.   └─nofrills function_(d$args, d$body, ..env)
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      > test_check("nofrills")
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-functions.R:48:3): body can be a closure ────────────────────────
      Error: Body must be an expression or closure.
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(fn(x ~ function(y) x + y), foo) at test-functions.R:48:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─nofrills::fn(x ~ function(y) x + y)
       5.   └─nofrills function_(d$args, d$body, ..env)
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 107 ]
      Error: Test failures
      Execution halted
    ```

# OncoBayes2

<details>

* Version: 0.8-2
* GitHub: NA
* Source code: https://github.com/cran/OncoBayes2
* Date/Publication: 2021-09-14 10:10:02 UTC
* Number of recursive dependencies: 91

Run `cloud_details(, "OncoBayes2")` for more info

</details>

## Newly broken

*   checking whether package ‘OncoBayes2’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/tmp/workdir/OncoBayes2/new/OncoBayes2.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking installed package size ... NOTE
    ```
      installed size is 60.7Mb
      sub-directories of 1Mb or more:
        libs  59.5Mb
    ```

*   checking for GNU extensions in Makefiles ... NOTE
    ```
    GNU make is a SystemRequirements.
    ```

## Installation

### Devel

```
* installing *source* package ‘OncoBayes2’ ...
** package ‘OncoBayes2’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: current / base
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
Info: integer division implicitly rounds to integer. Found int division: left_ind + right_ind / 2
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

** libs
...
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/ProductEvaluators.h:35:90:   required from ‘Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::evaluator(const XprType&) [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Options = 0; Eigen::internal::evaluator<Eigen::Product<Lhs, Rhs, Option> >::XprType = Eigen::Product<Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>, Eigen::Matrix<double, -1, 1>, 0>]’
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/Product.h:132:22:   required from ‘Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::operator const Scalar() const [with Lhs = Eigen::Product<Eigen::CwiseBinaryOp<Eigen::internal::scalar_product_op<double, double>, const Eigen::CwiseNullaryOp<Eigen::internal::scalar_constant_op<double>, const Eigen::Matrix<double, 1, -1> >, const Eigen::Transpose<Eigen::Matrix<double, -1, 1> > >, Eigen::Matrix<double, -1, -1>, 0>; Rhs = Eigen::Matrix<double, -1, 1>; int Option = 0; Eigen::internal::dense_product_base<Lhs, Rhs, Option, 6>::Scalar = double]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:23:56:   required from ‘double stan::mcmc::dense_e_metric<Model, BaseRNG>::T(stan::mcmc::dense_e_point&) [with Model = model_blrm_exnex_namespace::model_blrm_exnex; BaseRNG = boost::random::additive_combine_engine<boost::random::linear_congruential_engine<unsigned int, 40014, 0, 2147483563>, boost::random::linear_congruential_engine<unsigned int, 40692, 0, 2147483399> >]’
/opt/R/4.0.3/lib/R/site-library/StanHeaders/include/src/stan/mcmc/hmc/hamiltonians/dense_e_metric.hpp:22:10:   required from here
/opt/R/4.0.3/lib/R/site-library/RcppEigen/include/Eigen/src/Core/DenseCoeffsBase.h:55:30: warning: ignoring attributes on template argument ‘Eigen::internal::packet_traits<double>::type’ {aka ‘__vector(2) double’} [-Wignored-attributes]
g++: fatal error: Killed signal terminated program cc1plus
compilation terminated.
make: *** [/opt/R/4.0.3/lib/R/etc/Makeconf:179: stanExports_blrm_exnex.o] Error 1
ERROR: compilation failed for package ‘OncoBayes2’
* removing ‘/tmp/workdir/OncoBayes2/new/OncoBayes2.Rcheck/OncoBayes2’


```
### CRAN

```
* installing *source* package ‘OncoBayes2’ ...
** package ‘OncoBayes2’ successfully unpacked and MD5 sums checked
** using staged installation
DIAGNOSTIC(S) FROM PARSER:
Info: integer division implicitly rounds to integer. Found int division: current / base
 Positive values rounded down, negative values rounded up or down in platform-dependent way.
Info: integer division implicitly rounds to integer. Found int division: left_ind + right_ind / 2
 Positive values rounded down, negative values rounded up or down in platform-dependent way.

** libs
...

** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (OncoBayes2)


```
# partition

<details>

* Version: 0.1.3
* GitHub: https://github.com/USCbiostats/partition
* Source code: https://github.com/cran/partition
* Date/Publication: 2021-01-07 22:30:03 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "partition")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      `attr(attr(expected$direct, 'body'), '.Environment')` is <env:0x55c3b883a568>
      
      `attr(attr(actual$measure, 'body'), '.Environment')` is <env:0x55c3b5389c00>
      `attr(attr(expected$measure, 'body'), '.Environment')` is <env:0x55c3b883a568>
      
      `attr(attr(actual$reduce, 'body'), '.Environment')` is <env:0x55c3b5389c00>
      `attr(attr(expected$reduce, 'body'), '.Environment')` is <env:0x55c3b883a568>
      Backtrace:
          ▆
       1. └─partition expect_identical_partition(search_k, above_k) at test-misc-partitioner-arguments.R:43:2
       2.   └─testthat::expect_equal(.x$partitioner, .y$partitioner, ignore_function_env = TRUE) at test-misc-partitioner-arguments.R:8:4
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 162 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 11.0Mb
      sub-directories of 1Mb or more:
        doc    1.2Mb
        libs   8.6Mb
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
     11.           └─rlang:::stop_fake_data_subset(call)
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

* Version: 0.5.0
* GitHub: https://github.com/eth-mds/ricu
* Source code: https://github.com/cran/ricu
* Date/Publication: 2021-08-18 10:00:02 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "ricu")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       14.                     └─ricu `<fn>`(...)
       15.                       ├─ricu::init_itm(res, ...)
       16.                       └─ricu:::init_itm.hrd_itm(res, ...)
       17.                         ├─ricu::load_id(...)
       18.                         │ └─base::lapply(as.list(mcal)[-1L], eval, parent.frame())
       19.                         │   └─base FUN(X[[i]], ...)
       20.                         │     └─base FUN(X[[i]], ...)
       21.                         ├─.data$id %in% .env$ids
       22.                         ├─id
       23.                         └─rlang:::`$.rlang_fake_data_pronoun`(.data, id)
       24.                           └─rlang:::stop_fake_data_subset(call)
      
      [ FAIL 1 | WARN 0 | SKIP 8 | PASS 591 ]
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

# tidyMicro

<details>

* Version: 1.47
* GitHub: https://github.com/CharlieCarpenter/tidyMicro
* Source code: https://github.com/cran/tidyMicro
* Date/Publication: 2020-09-13 17:10:03 UTC
* Number of recursive dependencies: 178

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
     33.   └─rlang:::stop_fake_data_subset(call)
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
       31.   └─rlang:::stop_fake_data_subset(call)
      
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

# tidyquery

<details>

* Version: 0.2.2
* GitHub: https://github.com/ianmcook/tidyquery
* Source code: https://github.com/cran/tidyquery
* Date/Publication: 2021-02-06 07:30:04 UTC
* Number of recursive dependencies: 64

Run `cloud_details(, "tidyquery")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       11. ├─dplyr fun(., ...)
       12. └─dtplyr:::filter.dtplyr_step(., ...)
       13.   └─dtplyr:::step_subset_i(.data, i)
       14.     └─dtplyr:::step_subset(parent, i = i)
       15.       └─base::stopifnot(is.null(i) || is_expression(i) || is_step(i))
      ── Failure (test-errors.R:96:3): query() fails on two very long expressions with no aliases ──
      `query("SELECT 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1, 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+2 FROM games")` did not throw the expected error.
      Backtrace:
          ▆
       1. └─testthat::expect_error(...) at test-errors.R:96:2
       2.   └─testthat:::expect_condition_matching(...)
      
      [ FAIL 2 | WARN 0 | SKIP 1 | PASS 216 ]
      Error: Test failures
      Execution halted
    ```

# tidytext

<details>

* Version: 0.3.1
* GitHub: https://github.com/juliasilge/tidytext
* Source code: https://github.com/cran/tidytext
* Date/Publication: 2021-04-10 18:10:02 UTC
* Number of recursive dependencies: 123

Run `cloud_details(, "tidytext")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Actual message: "`class` must be a character vector."
      Backtrace:
          ▆
       1. ├─testthat::expect_error(...) at test-unnest-tokens.R:206:2
       2. │ └─testthat:::quasi_capture(...)
       3. │   ├─testthat .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. └─tidytext::unnest_tokens(...)
       7.   └─rlang::abort(...)
       8.     └─rlang:::validate_signal_args(message, class, call, .subclass)
      
      [ FAIL 2 | WARN 0 | SKIP 2 | PASS 311 ]
      Error: Test failures
      Execution halted
    ```

# Tplyr

<details>

* Version: 0.4.1
* GitHub: https://github.com/atorus-research/Tplyr
* Source code: https://github.com/cran/Tplyr
* Date/Publication: 2021-07-13 16:20:02 UTC
* Number of recursive dependencies: 116

Run `cloud_details(, "Tplyr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Expected match: "Argument `group_name` in function `add_total_group` must be character."
      Actual message: "Argument `group_name` must be character. Instead a class of \"numeric\" was passed."
      Backtrace:
          ▆
       1. ├─testthat::expect_error(add_total_group(t, 1), "Argument `group_name` in function `add_total_group` must be character.") at test-pop_data.R:17:2
       2. │ └─testthat:::quasi_capture(...)
       3. │   ├─testthat .capture(...)
       4. │   │ └─base::withCallingHandlers(...)
       5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
       6. └─Tplyr::add_total_group(t, 1)
       7.   └─Tplyr:::assert_has_class(group_name, "character")
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 685 ]
      Error: Test failures
      Execution halted
    ```

# winch

<details>

* Version: 0.0.6
* GitHub: https://github.com/r-prof/winch
* Source code: https://github.com/cran/winch
* Date/Publication: 2020-11-16 07:30:03 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "winch")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘example0.R’
      Running ‘example1.R’
      Running ‘example2.R’
      Running ‘example3.R’
      Running ‘example4.R’
      Running ‘example5.R’
      Running ‘example6.R’
      Running ‘example7.R’
      Running ‘example8.R’
      Running ‘testthat.R’
    ...
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Failure (test-add_trace_back.R:27:3): traceback changed if native code ──────
      any(grepl("/winch", foo_baz$calls)) is not TRUE
      
      `actual`:   FALSE
      `expected`: TRUE 
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

