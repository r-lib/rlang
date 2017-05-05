#' Evaluate a quosure.
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
#'   back into the formula. See [quo()] for more details. If a
#'   list of formulas, `eval_tidy()` is applied to each of them in
#'   turn and the list of results is returned.
#' @param data A list (or data frame). This is passed to the
#'   [as_dictionary()] coercer, a generic used to transform an object
#'   to a proper data source. If you want to make `eval_tidy()` work
#'   for your own objects, you can define a method for this generic.
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
#' eval_tidy(quo(mean( !!var )), mtcars)
#' @name eval_tidy
eval_tidy <- function(f, data = NULL) {
  if (is_list(f)) {
    return(map(f, eval_tidy, data = data))
  }

  if (!inherits(f, "quosure")) {
    f <- new_quosure(f, caller_env())
  }
  overscope <- as_overscope(f, data)
  on.exit(overscope_clean(overscope))

  overscope_eval_next(overscope, f)
}

#' Data pronoun for tidy evaluation
#'
#' This pronoun is installed by functions performing [tidy
#' evaluation][eval_tidy]. It allows you to refer to overscoped data
#' explicitly.
#'
#' You can import this object in your package namespace to avoid `R
#' CMD check` errors when referring to overscoped objects.
#'
#' @name tidyeval-data
#' @export
#' @examples
#' quo <- quo(.data$foo)
#' eval_tidy(quo, list(foo = "bar"))
.data <- NULL
delayedAssign(".data", as_dictionary(list(), read_only = TRUE))

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
#' environment of a captured argument, see [enquo()]). It also
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

  if (!inherits(f, "quosure")) {
    f <- new_quosure(f, caller_env())
  }
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
  data_src <- as_dictionary(data, read_only = TRUE)
  enclosure <- f_env(quo) %||% base_env()

  # Create bottom environment pre-chained to the lexical scope
  bottom <- child_env(enclosure)

  # Emulate dynamic scope for established data
  if (is_vector(data)) {
    bottom <- env_bury(bottom, !!! discard_unnamed(data))
  } else if (is_env(data)) {
    bottom <- env_clone(data, parent = bottom)
  } else if (!is_null(data)) {
    abort("`data` must be a list or an environment")
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
  mut_parent_env(overscope$.top_env, lexical_env)

  .Call(rlang_eval, f_rhs(quo), overscope)
}
#' @rdname as_overscope
#' @export
overscope_clean <- function(overscope) {
  cur_env <- env_parent(overscope)
  top_env <- overscope$.top_env %||% cur_env

  # At this level we only want to remove what we have installed
  env_unbind(overscope, c("~", ".top_env", ".env"))

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

    if (!inherits(f, "quosure")) {
      # We want formulas to be evaluated in the overscope so that
      # functions like case_when() can pick up overscoped data. Using
      # the parent because the bottom level has definitions for
      # quosure self-evaluation etc.
      return(eval_bare(f, env_parent(overscope)))
    }
    if (quo_is_missing(f)) {
      return(missing_arg())
    }

    # Swap enclosures temporarily by rechaining the top of the
    # dynamic scope to the enclosure of the new formula, if it has
    # one. We do it at C level to avoid GC adjustments when changing
    # the parent. This should be safe since we reset everything
    # afterwards.
    .Call(rlang_set_parent, overscope_top, f_env(f) %||% overscope$.env)
    on.exit(.Call(rlang_set_parent, overscope_top, overscope$.env))

    .Call(rlang_eval, f_rhs(f), overscope)
  }
}
