#' Evaluate a formula
#'
#' \code{tidy_eval_rhs} evaluates the RHS of a formula and \code{tidy_eval_lhs}
#' evaluates the LHS. \code{tidy_eval} is a shortcut for \code{tidy_eval_rhs} since
#' that is what you most commonly need.
#'
#' If \code{data} is specified, variables will be looked for first in this
#' object, and if not found in the environment of the formula.
#'
#' @section Pronouns:
#' When used with \code{data}, \code{tidy_eval} provides two pronouns to make it
#' possible to be explicit about where you want values to come from:
#' \code{.env} and \code{.data}. These are thin wrappers around \code{.data}
#' and \code{.env} that throw errors if you try to access non-existent values.
#'
#' @param f A formula. Any expressions wrapped in \code{ UQ() } will
#'   will be "unquoted", i.e. they will be evaluated, and the results
#'   inserted back into the formula. See \code{\link{tidy_quote}()}
#'   for more details. If a list of formulas, \code{tidy_eval()} is
#'   applied to each of them in turn and the list of results is
#'   returned.
#' @param data A list (or data frame). \code{data_source} is a generic
#'   used to find the data associated with a given object. If you want
#'   to make \code{tidy_eval} work for your own objects, you can
#'   define a method for this generic.
#' @export
#' @examples
#' tidy_eval(~ 1 + 2 + 3)
#'
#' # formulas automatically capture their enclosing environment
#' foo <- function(x) {
#'   y <- 10
#'   ~ x + y
#' }
#' f <- foo(1)
#' f
#' tidy_eval(f)
#'
#' # If you supply data, tidy_eval will look their first:
#' tidy_eval(~ cyl, mtcars)
#'
#' # To avoid ambiguity, you can use .env and .data pronouns to be
#' # explicit:
#' cyl <- 10
#' tidy_eval(~ .data$cyl, mtcars)
#' tidy_eval(~ .env$cyl, mtcars)
#'
#' # Imagine you are computing the mean of a variable:
#' tidy_eval(~ mean(cyl), mtcars)
#' # How can you change the variable that's being computed?
#' # The easiest way is "unquote" with !!
#' # See ?tidy_quote for more details
#' var <- ~ cyl
#' tidy_eval(tidy_quote(mean( !!var )), mtcars)
#' @name tidy_eval
tidy_eval_rhs <- function(f, data = NULL) {
  rhs <- new_tidy_quote(f_rhs(f), f_env(f))
  rhs <- tidy_eval(rhs, data)
  f_rhs(f) <- rhs
  f
}
#' @rdname tidy_eval
#' @export
tidy_eval_lhs <- function(f, data = NULL) {
  lhs <- new_tidy_quote(f_lhs(f), f_env(f))
  lhs <- tidy_eval(lhs, data)
  f_lhs(f) <- lhs
  f
}
#' @rdname tidy_eval
#' @export
tidy_eval <- function(f, data = NULL) {
  if (is_list(f)) {
    return(map(f, tidy_eval, data = data))
  }

  if (is_formula(f)) {
    expr <- f_rhs(f)
    env <- f_env(f)
  } else {
    expr <- f
    env <- caller_env()
  }

  env <- tidy_eval_env(env, data)
  on.exit(tidy_eval_env_cleanup(env))

  .Call(rlang_eval, expr, env)
}

#' Create a tidy evaluation environment.
#'
#' This is useful when want to use the tidy evaluation framework in
#' your own evaluating functions. The returned environment has
#' elements of \code{data} in scope, the \code{.env} and \code{.data}
#' pronouns, and treats formulas as self-evaluating promises. See
#' \code{\link{tidy_eval}()} and \code{\link{tidy_quote}()} for more
#' information.
#'
#' Once an expression has been evaluated in the tidy environment, it's
#' a good idea to clean up the definitions that make self-evaluation
#' of formulas possible \code{tidy_eval_env_cleanup()}. Otherwise your
#' users may face unexpected results in specific corner cases (see
#' examples).
#'
#' @param env The original scope.
#' @param data Additional data to put in scope.
#' @export
#' @examples
#' # Evaluating in a tidy evaluation environment enables all tidy
#' # features:
#' env <- tidy_eval_env(data = mtcars)
#' eval(quote(list(.data$cyl, ~letters)), env)
#'
#' # However you need to cleanup the environment after
#' # evaluation. Otherwise the leftover definitions for self-evaluation
#' # of formulas might cause unexpected results:
#' fn <- eval(quote(function() ~letters), env)
#' fn()
#'
#' tidy_eval_env_cleanup(env)
#' fn()
tidy_eval_env <- function(env = base_env(), data = NULL) {
  data_src <- data_source(data)

  if (!length(data)) {
    # Derive a child because we're going to add bindings
    eval_env <- child_env(env)
  } else {
    # Emulate dynamic scope for established data
    eval_env <- env_bury(env, discard_unnamed(data))
  }

  # Install pronouns
  eval_env$.data <- data_src
  eval_env$.env <- data_source(env)

  # Install fpromises and make sure to propagate `data`.
  eval_env$`~` <- f_self_eval(data, env, eval_env)

  # Guarded formulas are wrapped in another call to make sure they
  # don't self-evaluate.
  eval_env$`_F` <- unguard_formula

  eval_env
}
#' @rdname tidy_eval_env
#' @param eval_env A tidy evaluation env to clean up.
#' @export
tidy_eval_env_cleanup <- function(eval_env) {
  env_unbind(eval_env, c("~", "_F"))
  eval_env
}

f_self_eval <- function(`_data`, `_orig_env`, `_orig_eval_env`) {
  function(...) {
    f <- sys.call()

    # Two-sided formulas are not fpromises
    if (length(f) > 2) {
      # Make sure to propagate scope info when formula is quoted:
      f <- expr_eval(f, `_orig_env`)
      return(f)
    }

    # Take care of degenerate formulas (e.g. created with ~~letters)
    if (is_null(f_env(f))) {
      f_env(f) <- `_orig_eval_env`
    }

    eval_env <- tidy_eval_env(f_env(f), `_data`)
    .Call(rlang_eval, f_rhs(f), eval_env)
  }
}
unguard_formula <- function(...) {
  tilde <- sys.call()
  tilde[[1]] <- quote(`~`)
  tilde
}


#' Invoke a function with a list of arguments.
#'
#' Normally, you invoke a R function by typing arguments manually. A
#' powerful alternative is to call a function with a list of arguments
#' assembled programmatically. This is the purpose of \code{invoke()}.
#'
#' Technically, \code{invoke()} is basically a version of
#' \code{\link[base]{do.call}()} that creates cleaner call traces
#' because it does not inline the function and the arguments in the
#' call (see examples). To achieve this, \code{invoke()} creates a
#' child environment of \code{.env} with \code{.fn} and all arguments
#' bound to new symbols (see \code{\link{env_bury}()}). It then uses
#' the same strategy as \code{\link{expr_eval}()} to evaluate with
#' minimal noise.
#'
#' @param .fn A function to invoke. Can be a function object or the
#'   name of a function in scope of \code{.env}.
#' @param .args,... List of arguments (possibly named) to be passed to
#'   \code{.fn}.
#' @param .env The environment in which to call \code{.fn}.
#' @param .bury A character vector of length 2. The first string
#'   specifies which name should the function have in the call
#'   recorded in the evaluation stack. The second string specifies a
#'   prefix for the argument names. Set \code{.bury} to FALSE if you
#'   prefer to inline the function and its arguments in the call.
#' @export
#' @examples
#' # invoke() has the same purpose as do.call():
#' invoke(paste, letters)
#'
#' # But it creates much cleaner calls:
#' invoke(call_inspect, mtcars)
#'
#' # and stacktraces:
#' fn <- function(...) sys.calls()
#' invoke(fn, list(mtcars))
#'
#' # Compare to do.call():
#' do.call(call_inspect, mtcars)
#' do.call(fn, list(mtcars))
#'
#'
#' # Specify the function name either by supplying a string
#' # identifying the function (it should be visible in .env):
#' invoke("call_inspect", letters)
#'
#' # Or by changing the .bury argument, with which you can also change
#' # the argument prefix:
#' invoke(call_inspect, mtcars, .bury = c("inspect!", "col"))
invoke <- function(.fn, .args = list(), ...,
                   .env = caller_env(), .bury = c(".fn", "")) {
  args <- c(.args, list(...))

  if (is_false(.bury) || !length(args)) {
    if (is_scalar_character(.fn)) {
      .fn <- env_get(.env, .fn, inherit = TRUE)
    }
    call <- as.call(c(.fn, args))
    return(.Call(rlang_eval, call, .env))
  }


  if (!is_character(.bury, 2)) {
    stop(".bury must be a character vector of length 2", call. = FALSE)
  }
  arg_prefix <- .bury[[2]]
  fn_nm <- .bury[[1]]

  buried_nms <- paste0(arg_prefix, seq_along(.args))
  buried_args <- set_names(.args, buried_nms)
  .env <- env_bury(.env, buried_args)
  .args <- set_names(buried_nms, names(.args))
  .args <- map(.args, as.name)

  if (is_function(.fn)) {
    env_assign(.env, fn_nm, .fn)
    .fn <- fn_nm
  }

  call <- as.call(c(as_symbol(.fn), .args))
  .Call(rlang_eval, call, .env)
}
