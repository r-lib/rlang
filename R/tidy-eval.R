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

  expr <- get_expr(f)
  lexical_env <- if (is_formula(f)) f_env(f) else caller_env()

  eval_env <- overscope_env(lexical_env, data)
  on.exit(overscope_clean(eval_env))

  .Call(rlang_eval, expr, eval_env)
}

#' Tidy evaluation in a custom environment.
#'
#' We recommend using [tidy_eval()] in your DSLs as much as possible
#' to ensure some consistency across packages (`.data` and `.env`
#' pronouns, etc). However, some DSLs might need a different
#' evaluation environment. In this case, you can call
#' `overscope_eval()` with the bottom and the top of your custom
#' dynamic scope (see [overscope_env()] for more information).
#'
#' Note that `overscope_eval()` always installs a `.env` pronoun in the
#' bottom environment of your dynamic scope. This pronoun provides a
#' shortcut to the original lexical enclosure (typically, the dynamic
#' environment of a captured argument, see [tidy_capture()]).
#'
#' @inheritParams tidy_eval
#' @inheritParams overscope_env
#' @export overscope_eval
overscope_eval <- function(f, bottom_env, top_env = NULL) {
  if (is_list(f)) {
    return(map(f, overscope_eval, bottom_env, top_env))
  }

  expr <- get_expr(f)
  lexical_env <- if (is_formula(f)) f_env(f) else caller_env()
  top_env <- top_env %||% bottom_env

  bottom_env <- overscope_install(bottom_env, top_env, lexical_env)
  on.exit(overscope_clean(bottom_env))

  .Call(rlang_eval, expr, bottom_env)
}

#' Create a dynamic scope for tidy evaluation.
#'
#' Tidy evaluation works by rescoping a set of symbols (column names
#' of a data frame for example) to custom bindings. While doing this,
#' it is important to keep the original environment of captured
#' expressions in scope. The gist of tidy evaluation is to create a
#' dynamic scope containing custom bindings that should have
#' precedence when expressions are evaluated, and chain this scope
#' (set of linked environments) to the lexical enclosure of formulas
#' under evaluation. During tidy evaluation, formulas are transformed
#' into formula-promises and will self-evaluate their RHS as soon as
#' they are called. The main trick of tidyeval is to consistently
#' rechain the dynamic scope to the lexical enclosure of each tidy
#' quote under evaluation.
#'
#' These functions are useful for embedding the tidy evaluation
#' framework in your own DSLs with your own evaluating function. They
#' let you create a custom dynamic scope. That is, a set of chained
#' environments whose bottom serves as evaluation environment and
#' whose top is rechained to the current lexical enclosure. But most
#' of the time, you can just use [overscope_eval()] as it will take
#' care of installing the tidyeval components in your custom dynamic
#' scope.
#'
#' * `overscope_env()` is the function that powers [tidy_eval()]. It
#'   could be useful if you cannot use `tidy_eval()` for some reason,
#'   but serves mostly as an example of how to build a dynamic scope
#'   for tidy evaluation. In this case, it creates pronouns `.data`
#'   and `.env` and buries all dynamic bindings from the supplied
#'   `data` in new environments.
#'
#' * `overscope_install()` is called by `overscope_env()` and
#'   [overscope_eval()]. It installs the definitions for making
#'   formulas self-evaluate and for formula-guards. It also installs
#'   the pronoun `.top_env` that helps keeping track of the boundary
#'   of the dynamic scope. If you evaluate a tidy quote with
#'   [overscope_eval()], you don't need to use this.
#'
#' * `overscope_next()` is useful when you have several quosures to
#'   evaluate in a same dynamic scope. That's a simple wrapper around
#'   [expr_eval()] that updates the `.env` pronoun and rechains the
#'   dynamic scope to the new formula enclosure to evaluate.
#'
#' * Once an expression has been evaluated in the tidy environment,
#'   it's a good idea to clean up the definitions that make
#'   self-evaluation of formulas possible `overscope_clean()`.
#'   Otherwise your users may face unexpected results in specific
#'   corner cases (e.g. when the evaluation environment is leaked, see
#'   examples). Note that this function is automatically called by
#'   [overscope_eval()].
#'
#' @param lexical_env The original lexical scope. Usually the
#'   environment bundled with the outermost tidy quote.
#' @param data Additional data to put in scope.
#' @export
#' @examples
#' # Evaluating in a tidy evaluation environment enables all tidy
#' # features:
#' env <- overscope_env(data = mtcars)
#' eval(quote(list(.data$cyl, ~letters)), env)
#'
#' # However you need to cleanup the environment after evaluation.
#' # Otherwise the leftover definitions for self-evaluation of
#' # formulas might cause unexpected results:
#' fn <- eval(quote(function() ~letters), env)
#' fn()
#'
#' overscope_clean(env)
#' fn()
#' @md
overscope_env <- function(lexical_env = base_env(), data = NULL) {
  data_src <- data_source(data)

  # Create top and bottom environments, pre-chained to the lexical scope.
  top_env <- bottom_env <- child_env(lexical_env)

  # Emulate dynamic scope for established data
  if (length(data)) {
    bottom_env <- env_bury(bottom_env, discard_unnamed(data))
  }

  # Install data pronoun
  bottom_env$.data <- data_src

  overscope_install(bottom_env, top_env, lexical_env)
}

#' @rdname overscope_env
#' @param bottom_env This is the environment in which formula-promises
#'   are evaluated. This environment typically contains pronouns and
#'   its direct parents contain the rescoping bindings. The last one
#'   of these parents is the `top_env`.
#' @param top_env The top environment of the dynamic scope is chained
#'   and rechained to lexical enclosures of self-evaluating formulas
#'   (or formula-promises). This ensures hygienic scoping: the
#'   bindings in the dynamic scope have precedence, but the bindings
#'   in the dynamic environment where the tidy quotes were created are
#'   in scope as well.
#' @export
overscope_install <- function(bottom_env, top_env = NULL,
                              lexical_env = base_env()) {
  top_env <- top_env %||% bottom_env
  env_parent(top_env) <- lexical_env

  # Create a child because we don't know what might be in bottom_env.
  # This way we can just remove all bindings between the parent of
  # `bottom_env` and `top_env`. We don't want to clean everything in
  # `bottom_env` in case the environment is leaked, e.g. through a
  # closure that might rely on some local bindings installed by the
  # user.
  bottom_env <- child_env(bottom_env)

  bottom_env$`~` <- f_self_eval(bottom_env, top_env)
  bottom_env$`_F` <- f_unguard
  bottom_env$.top_env <- top_env
  bottom_env$.env <- lexical_env

  bottom_env
}
#' @rdname overscope_env
#' @inheritParams tidy_eval
#' @export
overscope_next <- function(f, bottom_env) {
  lexical_env <- f_env(f)

  bottom_env$.env <- lexical_env
  env_set_parent(bottom_env$.top_env, lexical_env)

  .Call(rlang_eval, f_rhs(f), bottom_env)
}
#' @rdname overscope_env
#' @export
overscope_clean <- function(bottom_env) {
  cur_env <- env_parent(bottom_env)
  top_env <- bottom_env$.top_env %||% cur_env

  # At this level we only want to remove what we have installed
  env_unbind(bottom_env, c("~", "_F", ".top_env", ".env"))

  while(!identical(cur_env, env_parent(top_env))) {
    env_unbind(cur_env, names(cur_env))
    cur_env <- env_parent(cur_env)
  }

  bottom_env
}

f_self_eval <- function(bottom_env, top_env) {
  function(...) {
    f <- sys.call()

    if (is_null(f_env(f))) {
      # Take care of degenerate formulas (e.g. created with ~~letters).
      # We assign them in `bottom_env` rather than `lexical_env` so that
      # things like case_when() within mutate() work out.
      f <- as_tidy_quote(f, bottom_env)
      fixup <- TRUE
    } else {
      fixup <- FALSE
    }

    # Two-sided formulas are not fpromises
    if (length(f) > 2) {
      return(f)
    }

    if (!fixup) {
      # Swap enclosures temporarily by rechaining the top of the dynamic
      # scope to the enclosure of the new formula, if it has one.
      env_parent(top_env) <- f_env(f) %||% bottom_env$.env
      on.exit(env_parent(top_env) <- bottom_env$.env)
    }

    .Call(rlang_eval, f_rhs(f), bottom_env)
  }
}
f_unguard <- function(...) {
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


  if (!is_character(.bury, 2L)) {
    abort("`.bury` must be a character vector of length 2")
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
