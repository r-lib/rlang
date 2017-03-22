#' Evaluate a quosure.
#'
#' `eval_tidy_rhs` evaluates the RHS of a formula and
#' `eval_tidy_lhs()` evaluates the LHS. `eval_tidy()` is a shortcut
#' for `eval_tidy_rhs()` since that is what you most commonly need.
#'
#' If `data` is specified, variables will be looked for first in this
#' object, and if not found in the environment of the formula.
#'
#' @section Pronouns:
#'
#'   When used with `data`, `eval_tidy()` provides two pronouns to
#'   make it possible to be explicit about where you want values to
#'   come from: `.env` and `.data`. These are thin wrappers around
#'   `.data` and `.env` that throw errors if you try to access
#'   non-existent values.
#'
#' @param f A formula. Any expressions wrapped in `UQ()` will will be
#'   "unquoted", i.e. they will be evaluated, and the results inserted
#'   back into the formula. See [quosure()] for more details. If a
#'   list of formulas, `eval_tidy()` is applied to each of them in
#'   turn and the list of results is returned.
#' @param data A list (or data frame). `data_source` is a generic used
#'   to find the data associated with a given object. If you want to
#'   make `eval_tidy` work for your own objects, you can define a
#'   method for this generic.
#' @export
#' @examples
#' eval_tidy(~ 1 + 2 + 3)
#'
#' # formulas automatically capture their enclosing environment
#' foo <- function(x) {
#'   y <- 10
#'   ~ x + y
#' }
#' f <- foo(1)
#' f
#' eval_tidy(f)
#'
#' # If you supply data, eval_tidy() will look there first:
#' eval_tidy(~ cyl, mtcars)
#'
#' # To avoid ambiguity, you can use .env and .data pronouns to be
#' # explicit:
#' cyl <- 10
#' eval_tidy(~ .data$cyl, mtcars)
#' eval_tidy(~ .env$cyl, mtcars)
#'
#' # Imagine you are computing the mean of a variable:
#' eval_tidy(~ mean(cyl), mtcars)
#' # How can you change the variable that's being computed?
#' # The easiest way is "unquote" with !!
#' # See ?quosure for more details
#' var <- ~ cyl
#' eval_tidy(quosure(mean( !!var )), mtcars)
#' @name eval_tidy
eval_tidy_rhs <- function(f, data = NULL) {
  rhs <- new_quosure(f_rhs(f), f_env(f))
  rhs <- eval_tidy(rhs, data)
  f_rhs(f) <- rhs
  f
}
#' @rdname eval_tidy
#' @export
eval_tidy_lhs <- function(f, data = NULL) {
  lhs <- new_quosure(f_lhs(f), f_env(f))
  lhs <- eval_tidy(lhs, data)
  f_lhs(f) <- lhs
  f
}
#' @rdname eval_tidy
#' @export
eval_tidy <- function(f, data = NULL) {
  if (is_list(f)) {
    return(map(f, eval_tidy, data = data))
  }

  f <- as_quosure(f, caller_env())
  overscope <- as_overscope(f, data)
  on.exit(overscope_clean(overscope))

  overscope_eval_next(overscope, f)
}

#' Tidy evaluation in a custom environment.
#'
#' We recommend using [eval_tidy()] in your DSLs as much as possible
#' to ensure some consistency across packages (`.data` and `.env`
#' pronouns, etc). However, some DSLs might need a different
#' evaluation environment. In this case, you can call `eval_tidy_()`
#' with the bottom and the top of your custom overscope (see
#' [as_overscope()] for more information).
#'
#' Note that `eval_tidy_()` always installs a `.env` pronoun in the
#' bottom environment of your dynamic scope. This pronoun provides a
#' shortcut to the original lexical enclosure (typically, the dynamic
#' environment of a captured argument, see [catch_quosure()]). It also
#' cleans up the overscope after evaluation. See [overscope_eval_next()]
#' for evaluating several quosures in the same overscope.
#'
#' @inheritParams eval_tidy
#' @inheritParams as_overscope
#' @export
eval_tidy_ <- function(f, bottom, top = NULL) {
  if (is_list(f)) {
    return(map(f, eval_tidy_, bottom, top))
  }

  top <- top %||% bottom
  overscope <- new_overscope(bottom, top)
  on.exit(overscope_clean(overscope))

  f <- as_quosure(f, caller_env())
  overscope_eval_next(overscope, f)
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
#' of the time, you can just use [eval_tidy_()] as it will take
#' care of installing the tidyeval components in your custom dynamic
#' scope.
#'
#' * `as_overscope()` is the function that powers [eval_tidy()]. It
#'   could be useful if you cannot use `eval_tidy()` for some reason,
#'   but serves mostly as an example of how to build a dynamic scope
#'   for tidy evaluation. In this case, it creates pronouns `.data`
#'   and `.env` and buries all dynamic bindings from the supplied
#'   `data` in new environments.
#'
#' * `new_overscope()` is called by `as_overscope()` and
#'   [eval_tidy_()]. It installs the definitions for making
#'   formulas self-evaluate and for formula-guards. It also installs
#'   the pronoun `.top_env` that helps keeping track of the boundary
#'   of the dynamic scope. If you evaluate a tidy quote with
#'   [eval_tidy_()], you don't need to use this.
#'
#' * `eval_tidy_()` is useful when you have several quosures to
#'   evaluate in a same dynamic scope. That's a simple wrapper around
#'   [eval_bare()] that updates the `.env` pronoun and rechains the
#'   dynamic scope to the new formula enclosure to evaluate.
#'
#' * Once an expression has been evaluated in the tidy environment,
#'   it's a good idea to clean up the definitions that make
#'   self-evaluation of formulas possible `overscope_clean()`.
#'   Otherwise your users may face unexpected results in specific
#'   corner cases (e.g. when the evaluation environment is leaked, see
#'   examples). Note that this function is automatically called by
#'   [eval_tidy_()].
#'
#' @param quo A [quosure].
#' @param data Additional data to put in scope.
#' @return An overscope environment.
#' @export
#' @examples
#' # Evaluating in a tidy evaluation environment enables all tidy
#' # features:
#' expr <- quote(list(.data$cyl, ~letters))
#' f <- as_quosure(expr)
#' overscope <- as_overscope(f, data = mtcars)
#' overscope_eval_next(overscope, f)
#'
#' # However you need to cleanup the environment after evaluation.
#' # Otherwise the leftover definitions for self-evaluation of
#' # formulas might cause unexpected results:
#' fn <- overscope_eval_next(overscope, ~function() ~letters)
#' fn()
#'
#' overscope_clean(overscope)
#' fn()
as_overscope <- function(quo, data = NULL) {
  data_src <- data_source(data)
  enclosure <- f_env(quo) %||% base_env()

  # Create bottom environment pre-chained to the lexical scope
  bottom <- child_env(enclosure)

  # Emulate dynamic scope for established data
  if (length(data)) {
    bottom <- env_bury(bottom, discard_unnamed(data))
  }

  # Install data pronoun
  bottom$.data <- data_src

  new_overscope(bottom, enclosure = enclosure)
}

#' @rdname as_overscope
#' @param bottom This is the environment (or the bottom of a set of
#'   environments) containing definitions for overscoped symbols. The
#'   bottom environment typically contains pronouns (like `.data`)
#'   while its direct parents contain the overscoping bindings. The
#'   last one of these parents is the `top`.
#' @param top The top environment of the overscope. During tidy
#'   evaluation, this environment is chained and rechained to lexical
#'   enclosures of self-evaluating formulas (or quosures). This is the
#'   mechanism that ensures hygienic scoping: the bindings in the
#'   overscope have precedence, but the bindings in the dynamic
#'   environment where the tidy quotes were created in the first place
#'   are in scope as well.
#' @param enclosure The default enclosure. After a quosure is done
#'   self-evaluating, the overscope is rechained to the default
#'   enclosure.
#' @return A valid overscope: a child environment of `bottom`
#'   containing the definitions enabling tidy evaluation
#'   (self-evaluating quosures, formula-unguarding, ...).
#' @export
new_overscope <- function(bottom, top = NULL, enclosure = base_env()) {
  top <- top %||% bottom

  # Create a child because we don't know what might be in bottom_env.
  # This way we can just remove all bindings between the parent of
  # `overscope` and `overscope_top`. We don't want to clean everything in
  # `overscope` in case the environment is leaked, e.g. through a
  # closure that might rely on some local bindings installed by the
  # user.
  overscope <- child_env(bottom)

  overscope$`~` <- f_self_eval(overscope, top)
  overscope$`_F` <- f_unguard
  overscope$.top_env <- top
  overscope$.env <- enclosure

  overscope
}
#' @rdname as_overscope
#' @param overscope A valid overscope containing bindings for `~`,
#'   `.top_env` and `_F` and whose parents contain overscoped bindings
#'   for tidy evaluation.
#' @param env The lexical enclosure in case `quo` is not a validly
#'   scoped quosure. This is the [base environment][base_env] by
#'   default.
#' @export
overscope_eval_next <- function(overscope, quo, env = base_env()) {
  quo <- as_quosureish(quo, env)
  lexical_env <- f_env(quo)

  overscope$.env <- lexical_env
  env_set_parent(overscope$.top_env, lexical_env)

  .Call(rlang_eval, f_rhs(quo), overscope)
}
#' @rdname as_overscope
#' @export
overscope_clean <- function(overscope) {
  cur_env <- env_parent(overscope)
  top_env <- overscope$.top_env %||% cur_env

  # At this level we only want to remove what we have installed
  env_unbind(overscope, c("~", "_F", ".top_env", ".env"))

  while(!identical(cur_env, env_parent(top_env))) {
    env_unbind(cur_env, names(cur_env))
    cur_env <- env_parent(cur_env)
  }

  overscope
}

#' @useDynLib rlang rlang_set_parent
f_self_eval <- function(overscope, overscope_top) {
  function(...) {
    f <- sys.call()

    if (is_null(f_env(f))) {
      # Take care of degenerate formulas (e.g. created with ~~letters).
      # We assign them in `overscope` rather than `lexical_env` so that
      # things like case_when() within mutate() work out.
      f <- as_quosureish(f, overscope)
      fixup <- TRUE
    } else {
      fixup <- FALSE
    }

    # Two-sided formulas are not fpromises
    if (length(f) > 2) {
      return(f)
    }

    if (!fixup) {
      # Swap enclosures temporarily by rechaining the top of the
      # dynamic scope to the enclosure of the new formula, if it has
      # one. We do it at C level to avoid GC adjustments when changing
      # the parent. This should be safe since we reset everything
      # afterwards.
      .Call(rlang_set_parent, overscope_top, f_env(f) %||% overscope$.env)
      on.exit(.Call(rlang_set_parent, overscope_top, overscope$.env))
    }

    .Call(rlang_eval, f_rhs(f), overscope)
  }
}
f_unguard <- function(...) {
  tilde <- sys.call()
  tilde[[1]] <- sym_tilde

  # Formulas explicitly supplied by the user are guarded but not
  # unquoted. We evaluate them now to get the right environment.
  if (is_null(attr(tilde, ".Environment"))) {
    tilde <- structure(tilde,
      class = "formula",
      .Environment = caller_env()
    )
  }

  tilde
}


#' Invoke a function with a list of arguments.
#'
#' Normally, you invoke a R function by typing arguments manually. A
#' powerful alternative is to call a function with a list of arguments
#' assembled programmatically. This is the purpose of `invoke()`.
#'
#' Technically, `invoke()` is basically a version of [base::do.call()]
#' that creates cleaner call traces because it does not inline the
#' function and the arguments in the call (see examples). To achieve
#' this, `invoke()` creates a child environment of `.env` with `.fn`
#' and all arguments bound to new symbols (see [env_bury()]). It then
#' uses the same strategy as [eval_bare()] to evaluate with minimal
#' noise.
#'
#' @param .fn A function to invoke. Can be a function object or the
#'   name of a function in scope of `.env`.
#' @param .args,... List of arguments (possibly named) to be passed to
#'   `.fn`.
#' @param .env The environment in which to call `.fn`.
#' @param .bury A character vector of length 2. The first string
#'   specifies which name should the function have in the call
#'   recorded in the evaluation stack. The second string specifies a
#'   prefix for the argument names. Set `.bury` to `FALSE` if you
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
