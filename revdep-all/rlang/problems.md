# exampletestr

<details>

* Version: 1.6.1
* Source code: https://github.com/cran/exampletestr
* URL: https://rorynolan.github.io/exampletestr, https://github.com/rorynolan/exampletestr#readme
* BugReports: https://www.github.com/rorynolan/exampletestr/issues
* Date/Publication: 2020-04-15 18:10:02 UTC
* Number of recursive dependencies: 82

Run `revdep_details(,"exampletestr")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      11: lapply(teardown_env$queue, function(f) try(f()))
      12: teardown_run(dirname(path))
      13: FUN(X[[i]], ...)
      14: lapply(paths, test_file, env = env, reporter = current_reporter,     start_end_reporter = FALSE, load_helpers = FALSE, wrap = wrap)
      15: force(code)
      16: doWithOneRestart(return(expr), restart)
      17: withOneRestart(expr, restarts[[1L]])
      18: withRestarts(testthat_abort_reporter = function() NULL, force(code))
      19: with_reporter(reporter = current_reporter, results <- lapply(paths,     test_file, env = env, reporter = current_reporter, start_end_reporter = FALSE,     load_helpers = FALSE, wrap = wrap))
      20: test_files(paths, reporter = reporter, env = env, stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      21: test_dir(path = test_path, reporter = reporter, env = env, filter = filter,     ..., stop_on_failure = stop_on_failure, stop_on_warning = stop_on_warning,     wrap = wrap)
      22: test_package_dir(package = package, test_path = test_path, filter = filter,     reporter = reporter, ..., stop_on_failure = stop_on_failure,     stop_on_warning = stop_on_warning, wrap = wrap)
      23: test_check("exampletestr")
      An irrecoverable exception occurred. R is aborting now ...
      Segmentation fault
    ```

