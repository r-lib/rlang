#' Evaluate an expression tidily
#'
#' @description
#'
#' `eval_tidy()` is a variant of [base::eval()] and [eval_bare()] that
#' powers the [tidy evaluation
#' framework](http://rlang.tidyverse.org/articles/tidy-evaluation.html).
#' It evaluates `expr` in an [overscope][as_overscope] where the
#' special definitions enabling tidy evaluation are installed. This
#' enables the following features:
#'
#' - Overscoped data. You can supply a data frame or list of named
#'   vectors to the `data` argument. The data contained in this list
#'   has precedence over the objects in the contextual environment.
#'   This is similar to how [base::eval()] accepts a list instead of
#'   an environment.
#'
#' - Self-evaluation of quosures. Within the overscope, quosures act
#'   like promises. When a quosure within an expression is evaluated,
#'   it automatically invokes the quoted expression in the captured
#'   environment (chained to the overscope). Note that quosures do not
#'   always get evaluated because of lazy semantics, e.g. `TRUE ||
#'   ~never_called`.
#'
#' - Pronouns. `eval_tidy()` installs the `.env` and `.data`
#'   pronouns. `.env` contains a reference to the calling environment,
#'   while `.data` refers to the `data` argument. These pronouns lets
#'   you be explicit about where to find values and throw errors if
#'   you try to access non-existent values.
#'
#' @param expr An expression.
#' @param data A list (or data frame). This is passed to the
#'   [as_dictionary()] coercer, a generic used to transform an object
#'   to a proper data source. If you want to make `eval_tidy()` work
#'   for your own objects, you can define a method for this generic.
#' @param env The lexical environment in which to evaluate `expr`.
#' @seealso [quo()], [quasiquotation]
#' @export
#' @examples
#' # Like base::eval() and eval_bare(), eval_tidy() evaluates quoted
#' # expressions:
#' expr <- expr(1 + 2 + 3)
#' eval_tidy(expr)
#'
#' # Like base::eval(), it lets you supply overscoping data:
#' foo <- 1
#' bar <- 2
#' expr <- quote(list(foo, bar))
#' eval_tidy(expr, list(foo = 100))
#'
#' # The main difference is that quosures self-evaluate within
#' # eval_tidy():
#' quo <- quo(1 + 2 + 3)
#' eval(quo)
#' eval_tidy(quo)
#'
#' # Quosures also self-evaluate deep in an expression not just when
#' # directly supplied to eval_tidy():
#' expr <- expr(list(list(list(!! quo))))
#' eval(expr)
#' eval_tidy(expr)
#'
#' # Self-evaluation of quosures is powerful because they
#' # automatically capture their enclosing environment:
#' foo <- function(x) {
#'   y <- 10
#'   quo(x + y)
#' }
#' f <- foo(1)
#'
#' # This quosure refers to `x` and `y` from `foo()`'s evaluation
#' # frame. That's evaluated consistently by eval_tidy():
#' f
#' eval_tidy(f)
#'
#'
#' # Finally, eval_tidy() installs handy pronouns that allows users to
#' # be explicit about where to find symbols. If you supply data,
#' # eval_tidy() will look there first:
#' cyl <- 10
#' eval_tidy(quo(cyl), mtcars)
#'
#' # To avoid ambiguity and be explicit, you can use the `.env` and
#' # `.data` pronouns:
#' eval_tidy(quo(.data$cyl), mtcars)
#' eval_tidy(quo(.env$cyl), mtcars)
#'
#' # Note that instead of using `.env` it is often equivalent to
#' # unquote a value. The only difference is the timing of evaluation
#' # since unquoting happens earlier (when the quosure is created):
#' eval_tidy(quo(!! cyl), mtcars)
#' @name eval_tidy
eval_tidy <- function(expr, data = NULL, env = caller_env()) {
  if (is_list(expr)) {
    return(map(expr, eval_tidy, data = data))
  }

  if (!inherits(expr, "quosure")) {
    expr <- new_quosure(expr, env)
  }
  overscope <- as_overscope(expr, data)
  on.exit(overscope_clean(overscope))

  overscope_eval_next(overscope, expr)
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

#' Tidy evaluation in a custom environment
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
eval_tidy_ <- function(expr, bottom, top = NULL, env = caller_env()) {
  top <- top %||% bottom
  overscope <- new_overscope(bottom, top)
  on.exit(overscope_clean(overscope))

  if (!inherits(expr, "quosure")) {
    expr <- new_quosure(expr, env)
  }
  overscope_eval_next(overscope, expr)
}


#' Create a dynamic scope for tidy evaluation
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
  mut_env_parent(overscope$.top_env, lexical_env)

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

f_self_eval <- function(overscope, overscope_top) {
  function(...) {
    f <- sys.call()

    # Evaluate formula in the overscope with base::`~`()
    if (!inherits(f, "quosure")) {
      return(tilde_eval(f, overscope))
    }
    if (quo_is_missing(f)) {
      return(missing_arg())
    }

    # Swap enclosures temporarily by rechaining the top of the dynamic
    # scope to the enclosure of the new formula, if it has one
    mut_env_parent(overscope_top, f_env(f) %||% overscope$.env)
    on.exit(mut_env_parent(overscope_top, overscope$.env))

    .Call(rlang_eval, f_rhs(f), overscope)
  }
}
tilde_eval <- function(f, env) {
  if (is_env(attr(f, ".Environment"))) {
    return(f)
  }

  # Inline the base primitive because overscopes override `~` to make
  # quosures self-evaluate
  f <- new_language(base::`~`, node_cdr(f))

  # Change it back because the result still has the primitive inlined
  mut_node_car(eval_bare(f, env), sym_tilde)
}
