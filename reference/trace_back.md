# Capture a backtrace

A backtrace captures the sequence of calls that lead to the current
function (sometimes called the call stack). Because of lazy evaluation,
the call stack in R is actually a tree, which the
[`print()`](https://rdrr.io/r/base/print.html) method for this object
will reveal.

Users rarely need to call `trace_back()` manually. Instead, signalling
an error with [`abort()`](https://rlang.r-lib.org/reference/abort.md) or
setting up
[`global_entrace()`](https://rlang.r-lib.org/reference/global_entrace.md)
is the most common way to create backtraces when an error is thrown.
Inspect the backtrace created for the most recent error with
[`last_error()`](https://rlang.r-lib.org/reference/last_error.md).

`trace_length()` returns the number of frames in a backtrace.

## Usage

``` r
trace_back(top = NULL, bottom = NULL)

trace_length(trace)
```

## Arguments

- top:

  The first frame environment to be included in the backtrace. This
  becomes the top of the backtrace tree and represents the oldest call
  in the backtrace.

  This is needed in particular when you call `trace_back()` indirectly
  or from a larger context, for example in tests or inside an RMarkdown
  document where you don't want all of the knitr evaluation mechanisms
  to appear in the backtrace.

  If not supplied, the `rlang_trace_top_env` global option is consulted.
  This makes it possible to trim the embedding context for all
  backtraces created while the option is set. If knitr is in progress,
  the default value for this option is
  [`knitr::knit_global()`](https://rdrr.io/pkg/knitr/man/knit_global.html)
  so that the knitr context is trimmed out of backtraces.

- bottom:

  The last frame environment to be included in the backtrace. This
  becomes the rightmost leaf of the backtrace tree and represents the
  youngest call in the backtrace.

  Set this when you would like to capture a backtrace without the
  capture context.

  Can also be an integer that will be passed to
  [`caller_env()`](https://rlang.r-lib.org/reference/stack.md).

- trace:

  A backtrace created by `trace_back()`.

## Examples

``` r
# Trim backtraces automatically (this improves the generated
# documentation for the rlang website and the same trick can be
# useful within knitr documents):
options(rlang_trace_top_env = current_env())

f <- function() g()
g <- function() h()
h <- function() trace_back()

# When no lazy evaluation is involved the backtrace is linear
# (i.e. every call has only one child)
f()
#>     ▆
#>  1. └─f()
#>  2.   └─g()
#>  3.     └─h()

# Lazy evaluation introduces a tree like structure
identity(identity(f()))
#>     ▆
#>  1. ├─base::identity(identity(f()))
#>  2. ├─base::identity(f())
#>  3. └─f()
#>  4.   └─g()
#>  5.     └─h()
identity(try(f()))
#>     ▆
#>  1. ├─base::identity(try(f()))
#>  2. ├─base::try(f())
#>  3. │ └─base::tryCatch(...)
#>  4. │   └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>  5. │     └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>  6. │       └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>  7. └─f()
#>  8.   └─g()
#>  9.     └─h()
try(identity(f()))
#>     ▆
#>  1. ├─base::try(identity(f()))
#>  2. │ └─base::tryCatch(...)
#>  3. │   └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>  4. │     └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>  5. │       └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>  6. ├─base::identity(f())
#>  7. └─f()
#>  8.   └─g()
#>  9.     └─h()

# When printing, you can request to simplify this tree to only show
# the direct sequence of calls that lead to `trace_back()`
x <- try(identity(f()))
x
#>     ▆
#>  1. ├─base::try(identity(f()))
#>  2. │ └─base::tryCatch(...)
#>  3. │   └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>  4. │     └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>  5. │       └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>  6. ├─base::identity(f())
#>  7. └─f()
#>  8.   └─g()
#>  9.     └─h()
print(x, simplify = "branch")
#>  1. base::try(identity(f()))
#>  7. f()
#>  8. g()
#>  9. h()

# With a little cunning you can also use it to capture the
# tree from within a base NSE function
x <- NULL
with(mtcars, {x <<- f(); 10})
#> [1] 10
x
#>     ▆
#>  1. ├─base::with(...)
#>  2. └─base::with.default(...)
#>  3.   └─base::eval(substitute(expr), data, enclos = parent.frame())
#>  4.     └─base::eval(substitute(expr), data, enclos = parent.frame())
#>  5.       └─f()
#>  6.         └─g()
#>  7.           └─h()


# Restore default top env for next example
options(rlang_trace_top_env = NULL)

# When code is executed indirectly, i.e. via source or within an
# RMarkdown document, you'll tend to get a lot of guff at the beginning
# related to the execution environment:
conn <- textConnection("summary(f())")
source(conn, echo = TRUE, local = TRUE)
#> 
#> > summary(f())
#>      ▆
#>   1. └─pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
#>   2.   └─pkgdown::build_site(...)
#>   3.     └─pkgdown:::build_site_local(...)
#>   4.       └─pkgdown::build_reference(...)
#>   5.         ├─pkgdown:::unwrap_purrr_error(...)
#>   6.         │ └─base::withCallingHandlers(...)
#>   7.         └─purrr::map(...)
#>   8.           └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
#>   9.             ├─purrr:::with_indexed_errors(...)
#>  10.             │ └─base::withCallingHandlers(...)
#>  11.             ├─purrr:::call_with_cleanup(...)
#>  12.             └─pkgdown (local) .f(.x[[i]], ...)
#>  13.               ├─base::withCallingHandlers(...)
#>  14.               └─pkgdown:::data_reference_topic(...)
#>  15.                 └─pkgdown:::run_examples(...)
#>  16.                   └─pkgdown:::highlight_examples(code, topic, env = env)
#>  17.                     └─downlit::evaluate_and_highlight(...)
#>  18.                       └─evaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler)
#>  19.                         ├─base::withRestarts(...)
#>  20.                         │ └─base (local) withRestartList(expr, restarts)
#>  21.                         │   ├─base (local) withOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]])
#>  22.                         │   │ └─base (local) doWithOneRestart(return(expr), restart)
#>  23.                         │   └─base (local) withRestartList(expr, restarts[-nr])
#>  24.                         │     └─base (local) withOneRestart(expr, restarts[[1L]])
#>  25.                         │       └─base (local) doWithOneRestart(return(expr), restart)
#>  26.                         ├─evaluate:::with_handlers(...)
#>  27.                         │ ├─base::eval(call)
#>  28.                         │ │ └─base::eval(call)
#>  29.                         │ └─base::withCallingHandlers(...)
#>  30.                         ├─base::withVisible(eval(expr, envir))
#>  31.                         └─base::eval(expr, envir)
#>  32.                           └─base::eval(expr, envir)
#>  33.                             ├─base::source(conn, echo = TRUE, local = TRUE)
#>  34.                             │ ├─base::withVisible(eval(ei, envir))
#>  35.                             │ └─base::eval(ei, envir)
#>  36.                             │   └─base::eval(ei, envir)
#>  37.                             ├─base::summary(f())
#>  38.                             └─f()
#>  39.                               └─g()
#>  40.                                 └─h()
close(conn)

# To automatically strip this off, specify which frame should be
# the top of the backtrace. This will automatically trim off calls
# prior to that frame:
top <- current_env()
h <- function() trace_back(top)

conn <- textConnection("summary(f())")
source(conn, echo = TRUE, local = TRUE)
#> 
#> > summary(f())
#>     ▆
#>  1. ├─base::summary(f())
#>  2. └─f()
#>  3.   └─g()
#>  4.     └─h()
close(conn)
```
