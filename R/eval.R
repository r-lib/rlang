#' Evaluate a formula
#'
#' \code{f_eval_rhs} evaluates the RHS of a formula and \code{f_eval_lhs}
#' evaluates the LHS. \code{f_eval} is a shortcut for \code{f_eval_rhs} since
#' that is what you most commonly need.
#'
#' If \code{data} is specified, variables will be looked for first in this
#' object, and if not found in the environment of the formula.
#'
#' @section Pronouns:
#' When used with \code{data}, \code{f_eval} provides two pronouns to make it
#' possible to be explicit about where you want values to come from:
#' \code{.env} and \code{.data}. These are thin wrappers around \code{.data}
#' and \code{.env} that throw errors if you try to access non-existent values.
#'
#' @param f A formula. Any expressions wrapped in \code{ UQ() } will
#'   will be "unquoted", i.e. they will be evaluated, and the results inserted
#'   back into the formula. See \code{\link{f_quote}()} for more details.
#' @param data A list (or data frame). \code{data_source} is a generic used to
#'   find the data associated with a given object. If you want to make
#'   \code{f_eval} work for your own objects, you can define a method for this
#'   generic.
#' @export
#' @examples
#' f_eval(~ 1 + 2 + 3)
#'
#' # formulas automatically capture their enclosing environment
#' foo <- function(x) {
#'   y <- 10
#'   ~ x + y
#' }
#' f <- foo(1)
#' f
#' f_eval(f)
#'
#' # If you supply data, f_eval will look their first:
#' f_eval(~ cyl, mtcars)
#'
#' # To avoid ambiguity, you can use .env and .data pronouns to be
#' # explicit:
#' cyl <- 10
#' f_eval(~ .data$cyl, mtcars)
#' f_eval(~ .env$cyl, mtcars)
#'
#' # Imagine you are computing the mean of a variable:
#' f_eval(~ mean(cyl), mtcars)
#' # How can you change the variable that's being computed?
#' # The easiest way is "unquote" with !!
#' # See ?f_quote for more details
#' var <- ~ cyl
#' f_eval(f_quote(mean( !!var )), mtcars)
#' @name f_eval
f_eval_rhs <- function(f, data = NULL) {
  f_eval(f, data)
}
#' @rdname f_eval
#' @export
f_eval_lhs <- function(f, data = NULL) {
  f <- f_new(f_lhs(f), env = f_env(f))
  f_eval(f, data)
}
#' @rdname f_eval
#' @export
f_eval <- function(f, data = NULL) {
  if (is_formula(f)) {
    expr <- f_rhs(f)
    env <- f_env(f)
  } else {
    expr <- f
    env <- parent.frame()
  }

  env <- eval_env(env, data)
  eval(expr, envir = env)
}

eval_env <- function(env, data) {
  data_src <- data_source(data)

  if (!length(data)) {
    # Derive a child because we're going to add bindings
    eval_env <- env_new(env)
  } else {
    # Emulate dynamic scope for established data
    eval_env <- env_bury(env, data)
  }

  # Install pronouns
  eval_env$.data <- data_src
  eval_env$.env <- data_source(env)

  # Install fpromises and make sure to propagate `data`.
  eval_env$`~` <- f_self_eval(data, env, eval_env)

  # Guarded formulas are wrapped in another call to make sure they
  # don't self-evaluate.
  eval_env$`_F` <- unguard_formula

  # Make unquoting and guarding operators available even when not imported
  eval_env$UQ <- rlang::UQ
  eval_env$UQS <- rlang::UQS
  eval_env$UQE <- rlang::UQE
  eval_env$UQF <- rlang::UQF

  eval_env
}

f_self_eval <- function(`_data`, `_orig_env`, `_orig_eval_env`) {
  function(...) {
    f <- sys.call()

    # Two-sided formulas are not fpromises
    if (length(f) > 2) {
      # Make sure to propagate scope info when formula is quoted:
      f <- eval(f, `_orig_env`)
      return(f)
    }

    # Take care of degenerate formulas (e.g. created with ~~letters)
    if (is_null(f_env(f))) {
      f_env(f) <- `_orig_eval_env`
    }

    eval_env <- eval_env(f_env(f), `_data`)
    eval(f_rhs(f), eval_env)
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
#' \code{invoke()} is basically a version of
#' \code{\link[base]{do.call}()} that creates cleaner call traces
#' because it does not inline the function and the arguments in the
#' call (see examples). To achieve this, \code{invoke()} creates a
#' child environment of \code{.env} with \code{.fn} and all arguments
#' bound to new symbols (see \code{\link{env_bury}()}). It then builds
#' a call with those symbols and evaluates it in a forged promise (see
#' \code{\link{with_env}()}).
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
                   .env = env_caller(), .bury = c(".fn", "")) {
  args <- c(.args, list(...))

  if (is_false(.bury) || !length(args)) {
    # Evaluate with a promise rather than do.call() to keep eval stack clean
    if (is_scalar_character(.fn)) {
      .fn <- env_get(.env, .fn, inherit = TRUE)
    }
    call <- as.call(c(.fn, args))
    env_assign_lazily_(env(), "promise", call, .env)
    return(promise)
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
  .args <- lapply(.args, as.name)

  if (is_function(.fn)) {
    env_assign(.env, fn_nm, .fn)
    .fn <- fn_nm
  }

  call <- as.call(c(as_name(.fn), .args))
  env_assign_lazily_(env(), "promise", call, .env)
  promise
}
