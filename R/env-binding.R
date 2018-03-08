#' Bind symbols to objects in an environment
#'
#' @description
#'
#' These functions create bindings in an environment. The bindings are
#' supplied through `...` as pairs of names and values or expressions.
#' `env_bind()` is equivalent to evaluating a `<-` expression within
#' the given environment. This function should take care of the
#' majority of use cases but the other variants can be useful for
#' specific problems.
#'
#' - `env_bind()` takes named _values_ which are bound in `.env`.
#'   `env_bind()` is equivalent to [base::assign()].
#'
#' - `env_bind_fns()` takes named _functions_ and creates active
#'   bindings in `.env`. This is equivalent to
#'   [base::makeActiveBinding()]. An active binding executes a
#'   function each time it is evaluated. The arguments are passed to
#'   [as_function()] so you can supply formulas instead of functions.
#'
#'   Remember that functions are scoped in their own environment.
#'   These functions can thus refer to symbols from this enclosure
#'   that are not actually in scope in the dynamic environment where
#'   the active bindings are invoked. This allows creative solutions
#'   to difficult problems (see the implementations of `dplyr::do()`
#'   methods for an example).
#'
#' - `env_bind_exprs()` takes named _expressions_. This is equivalent
#'   to [base::delayedAssign()]. The arguments are captured with
#'   [exprs()] (and thus support call-splicing and unquoting) and
#'   assigned to symbols in `.env`. These expressions are not
#'   evaluated immediately but lazily. Once a symbol is evaluated, the
#'   corresponding expression is evaluated in turn and its value is
#'   bound to the symbol (the expressions are thus evaluated only
#'   once, if at all).
#'
#' @section Side effects:
#'
#' Since environments have reference semantics (see relevant section
#' in [env()] documentation), modifying the bindings of an environment
#' produces effects in all other references to that environment. In
#' other words, `env_bind()` and its variants have side effects.
#'
#' As they are called primarily for their side effects, these
#' functions follow the convention of returning their input invisibly.
#'
#' @param .env An environment or an object bundling an environment,
#'   e.g. a formula, [quosure][quotation] or [closure][is_closure].
#'   This argument is passed to [get_env()].
#' @param ... Pairs of names and expressions, values or functions.
#'   These dots support [tidy dots][tidy-dots] features.
#' @return The input object `.env`, with its associated environment
#'   modified in place, invisibly.
#' @export
#' @examples
#' # env_bind() is a programmatic way of assigning values to symbols
#' # with `<-`. We can add bindings in the current environment:
#' env_bind(current_env(), foo = "bar")
#' foo
#'
#' # Or modify those bindings:
#' bar <- "bar"
#' env_bind(current_env(), bar = "BAR")
#' bar
#'
#' # It is most useful to change other environments:
#' my_env <- env()
#' env_bind(my_env, foo = "foo")
#' my_env$foo
#'
#' # A useful feature is to splice lists of named values:
#' vals <- list(a = 10, b = 20)
#' env_bind(my_env, !!! vals, c = 30)
#' my_env$b
#' my_env$c
#'
#' # You can also unquote a variable referring to a symbol or a string
#' # as binding name:
#' var <- "baz"
#' env_bind(my_env, !!var := "BAZ")
#' my_env$baz
#'
#'
#' # env_bind() and its variants are generic over formulas, quosures
#' # and closures. To illustrate this, let's create a closure function
#' # referring to undefined bindings:
#' fn <- function() list(a, b)
#' fn <- set_env(fn, child_env("base"))
#'
#' # This would fail if run since `a` etc are not defined in the
#' # enclosure of fn() (a child of the base environment):
#' # fn()
#'
#' # Let's define those symbols:
#' env_bind(fn, a = "a", b = "b")
#'
#' # fn() now sees the objects:
#' fn()
env_bind <- function(.env, ...) {
  invisible(env_bind_impl(.env, dots_list(...)))
}
env_bind_impl <- function(env, data) {
  if (!is_vector(data)) {
    type <- friendly_type_of(type_of(data))
    abort(sprintf("`data` must be a vector not a %s", type))
  }
  if (length(data)) {
    nms <- names(data)
    if (is_null(nms) || any(nms_are_invalid(nms))) {
      abort("Can't bind data because all arguments must be named")
    }
    if (any(duplicated(nms))) {
      abort("Can't bind data because some arguments have the same name")
    }
  }

  nms <- names(data)
  env_ <- get_env(env)

  for (i in seq_along(data)) {
    nm <- nms[[i]]
    base::assign(nm, data[[nm]], envir = env_)
  }

  env
}

# FIXME: Should these be env_bind_promises() and env_bind_actives()?

#' Bind lazy or active bindings
#'
#' @keywords internal
#' @section Life cycle:
#'
#' These functions are experimental. Expect API changes.
#'
#' @inheritParams env_bind
#' @param .eval_env The environment where the expressions will be
#'   evaluated when the symbols are forced.
#' @export
#' @examples
#'
#' # env_bind_exprs() assigns expressions lazily:
#' env <- env()
#' env_bind_exprs(env, name = cat("forced!\n"))
#' env$name
#' env$name
#'
#' # You can unquote expressions. Note that quosures are not
#' # supported, only raw expressions:
#' expr <- quote(message("forced!"))
#' env_bind_exprs(env, name = !! expr)
#' env$name
env_bind_exprs <- function(.env, ..., .eval_env = caller_env()) {
  exprs <- exprs(...)
  stopifnot(is_named(exprs))

  nms <- names(exprs)
  env_ <- get_env(.env)

  for (i in seq_along(exprs)) {
    do.call("delayedAssign", list(
      x = nms[[i]],
      value = exprs[[i]],
      eval.env = .eval_env,
      assign.env = env_
    ))
  }

  invisible(.env)
}
#' @rdname env_bind_exprs
#' @export
#' @examples
#'
#' # You can create active bindings with env_bind_fns()
#' # Let's create some bindings in the lexical enclosure of `fn`:
#' counter <- 0
#'
#' # And now a function that increments the counter and returns a
#' # string with the count:
#' fn <- function() {
#'   counter <<- counter + 1
#'   paste("my counter:", counter)
#' }
#'
#' # Now we create an active binding in a child of the current
#' # environment:
#' env <- env()
#' env_bind_fns(env, symbol = fn)
#'
#' # `fn` is executed each time `symbol` is evaluated or retrieved:
#' env$symbol
#' env$symbol
#' eval_bare(quote(symbol), env)
#' eval_bare(quote(symbol), env)
#'
#' # All arguments are passed to as_function() so you can use the
#' # formula shortcut:
#' env_bind_fns(env, foo = ~runif(1))
#' env$foo
#' env$foo
env_bind_fns <- function(.env, ...) {
  fns <- dots_splice(...)
  stopifnot(is_named(fns))
  fns <- map(fns, as_function)

  nms <- names(fns)
  env_ <- get_env(.env)

  exist <- env_has(env_, nms)
  env_unbind(env_, nms[exist])

  for (i in seq_along(fns)) {
    makeActiveBinding(nms[[i]], fns[[i]], env_)
  }

  invisible(.env)
}

#' Temporarily change bindings of an environment
#'
#' @description
#'
#' * `scoped_bindings()` temporarily changes bindings in `.env` (which
#'   is by default the caller environment). The bindings are reset to
#'   their original values when the current frame (or an arbitrary one
#'   if you specify `.frame`) goes out of scope.
#'
#' * `with_bindings()` evaluates `expr` with temporary bindings. When
#'   `with_bindings()` returns, bindings are reset to their original
#'   values. It is a simple wrapper around `scoped_bindings()`.
#'
#' @inheritParams env_bind
#' @param ... Pairs of names and values. These dots support splicing
#'   (with value semantics) and name unquoting.
#' @param .frame The frame environment that determines the scope of
#'   the temporary bindings. When that frame is popped from the call
#'   stack, bindings are switched back to their original values.
#' @return `scoped_bindings()` returns the values of old bindings
#'   invisibly; `with_bindings()` returns the value of `expr`.
#' @export
#' @examples
#' foo <- "foo"
#' bar <- "bar"
#'
#' # `foo` will be temporarily rebinded while executing `expr`
#' with_bindings(paste(foo, bar), foo = "rebinded")
#' paste(foo, bar)
scoped_bindings <- function(..., .env = .frame, .frame = caller_env()) {
  env <- get_env(.env)
  bindings <- dots_list(...)
  stopifnot(is_named(bindings))

  nms <- names(bindings)
  is_old <- env_has(env, nms)
  old <- env_get_list(env, nms[is_old])

  unbind_lang <- call2(env_unbind, env, nms[!is_old])
  rebind_lang <- call2(env_bind_impl, env, old)
  scoped_exit(frame = .frame, {
    !! unbind_lang
    !! rebind_lang
  })

  env_bind_impl(env, bindings)
  invisible(old)
}
#' @rdname scoped_bindings
#' @param .expr An expression to evaluate with temporary bindings.
#' @export
with_bindings <- function(.expr, ..., .env = caller_env()) {
  scoped_bindings(..., .env = .env)
  .expr
}

#' Mask bindings by defining symbols deeper in a scope
#'
#' `env_bury()` is like [env_bind()] but it creates the bindings in a
#' new child environment. This makes sure the new bindings have
#' precedence over old ones, without altering existing environments.
#' Unlike `env_bind()`, this function does not have side effects and
#' returns a new environment (or object wrapping that environment).
#'
#' @inheritParams env_bind
#' @return A copy of `.env` enclosing the new environment containing
#'   bindings to `...` arguments.
#' @seealso [env_bind()], [env_unbind()]
#' @export
#' @examples
#' orig_env <- env(a = 10)
#' fn <- set_env(function() a, orig_env)
#'
#' # fn() currently sees `a` as the value `10`:
#' fn()
#'
#' # env_bury() will bury the current scope of fn() behind a new
#' # environment:
#' fn <- env_bury(fn, a = 1000)
#' fn()
#'
#' # Even though the symbol `a` is still defined deeper in the scope:
#' orig_env$a
env_bury <- function(.env, ...) {
  env_ <- get_env(.env)
  env_ <- child_env(env_, ...)
  set_env(.env, env_)
}

#' Remove bindings from an environment
#'
#' `env_unbind()` is the complement of [env_bind()]. Like `env_has()`,
#' it ignores the parent environments of `env` by default. Set
#' `inherit` to `TRUE` to track down bindings in parent environments.
#'
#' @inheritParams get_env
#' @param nms A character vector containing the names of the bindings
#'   to remove.
#' @param inherit Whether to look for bindings in the parent
#'   environments.
#' @return The input object `env` with its associated environment
#'   modified in place, invisibly.
#' @export
#' @examples
#' data <- set_names(as_list(letters), letters)
#' env_bind(environment(), !!! data)
#' env_has(environment(), letters)
#'
#' # env_unbind() removes bindings:
#' env_unbind(environment(), letters)
#' env_has(environment(), letters)
#'
#' # With inherit = TRUE, it removes bindings in parent environments
#' # as well:
#' parent <- child_env(NULL, foo = "a")
#' env <- child_env(parent, foo = "b")
#' env_unbind(env, "foo", inherit = TRUE)
#' env_has(env, "foo", inherit = TRUE)
env_unbind <- function(env = caller_env(), nms, inherit = FALSE) {
  env_ <- get_env(env)

  if (inherit) {
    while (any(exist <- env_has(env_, nms, inherit = TRUE))) {
      rm(list = nms[exist], envir = env, inherits = TRUE)
    }
  } else {
    exist <- env_has(env_, nms, inherit = FALSE)
    rm(list = nms[exist], envir = env)
  }

  invisible(env)
}

#' Does an environment have or see bindings?
#'
#' `env_has()` is a vectorised predicate that queries whether an
#' environment owns bindings personally (with `inherit` set to
#' `FALSE`, the default), or sees them in its own environment or in
#' any of its parents (with `inherit = TRUE`).
#'
#' @inheritParams env_unbind
#' @return A named logical vector as long as `nms`.
#' @export
#' @examples
#' parent <- child_env(NULL, foo = "foo")
#' env <- child_env(parent, bar = "bar")
#'
#' # env does not own `foo` but sees it in its parent environment:
#' env_has(env, "foo")
#' env_has(env, "foo", inherit = TRUE)
env_has <- function(env = caller_env(), nms, inherit = FALSE) {
  nms <- set_names(nms)
  map_lgl(nms, exists, envir = get_env(env), inherits = inherit)
}

#' Get an object in an environment
#'
#' `env_get()` extracts an object from an enviroment `env`. By
#' default, it does not look in the parent environments.
#' `env_get_list()` extracts multiple objects from an environment into
#' a named list.
#'
#' @inheritParams get_env
#' @inheritParams env_has
#' @param nm,nms Names of bindings. `nm` must be a single string.
#' @param default A default value in case there is no binding for `nm`
#'   in `env`.
#' @return An object if it exists. Otherwise, throws an error.
#' @export
#' @examples
#' parent <- child_env(NULL, foo = "foo")
#' env <- child_env(parent, bar = "bar")
#'
#' # This throws an error because `foo` is not directly defined in env:
#' # env_get(env, "foo")
#'
#' # However `foo` can be fetched in the parent environment:
#' env_get(env, "foo", inherit = TRUE)
#'
#' # You can also avoid an error by supplying a default value:
#' env_get(env, "foo", default = "FOO")
env_get <- function(env = caller_env(), nm, inherit = FALSE, default) {
  env <- get_env(env)
  if (!missing(default)) {
    exists <- env_has(env, nm, inherit = inherit)
    if (!exists) {
      return(default)
    }
  }
  get(nm, envir = env, inherits = inherit)
}
#' @rdname env_get
#' @export
env_get_list <- function(env = caller_env(), nms, inherit = FALSE, default) {
  nms <- set_names(nms)

  # FIXME R 3.1: missingness of `default` is not passed through dots on 3.1
  if (missing(default)) {
    map(nms, env_get, env = env, inherit = inherit)
  } else {
    map(nms, env_get, env = env, inherit = inherit, default = default)
  }
}

#' Poke an object in an environment
#'
#' `env_poke()` will assign or reassign a binding in `env` if `create`
#' is `TRUE`. If `create` is `FALSE` and a binding does not already
#' exists, an error is issued.
#'
#' If `inherit` is `TRUE`, the parents environments are checked for
#' an existing binding to reassign. If not found and `create` is
#' `TRUE`, a new binding is created in `env`. The default value for
#' `create` is a function of `inherit`: `FALSE` when inheriting,
#' `TRUE` otherwise.
#'
#' This default makes sense because the inheriting case is mostly
#' for overriding an existing binding. If not found, something
#' probably went wrong and it is safer to issue an error. Note that
#' this is different to the base R operator `<<-` which will create
#' a binding in the global environment instead of the current
#' environment when no existing binding is found in the parents.
#'
#'
#' @section Life cycle:
#'
#' `env_poke()` is experimental. We are still experimenting with
#' reducing the number of redundant functions by using quasiquotation.
#' It is possible `env_poke()` will be deprecated in favour of
#' `env_bind()` and name-unquoting with `:=`.
#'
#' @inheritParams env_get
#' @param value The value for a new binding.
#' @param create Whether to create a binding if it does not already
#'   exist in the environment.
#' @return The old value of `nm` or the [missing
#'   argument][missing_arg()] if it did not exist yet.
#'
#' @keywords internal
#' @export
env_poke <- function(env = caller_env(), nm, value,
                     inherit = FALSE, create = NULL) {
  stopifnot(is_string(nm))
  env_ <- get_env(env)
  old <- env_get(env_, nm, inherit = inherit, default = missing_arg())

  # It is safer not to create a new binding when inherit is TRUE,
  # since the main purpose is to override an existing binding
  if (is_null(create)) {
    create <- if (inherit) FALSE else TRUE
  }

  if (inherit) {
    scope_poke(env, nm, value, create)
  } else if (create || env_has(env_, nm)) {
    assign(nm, value, envir = env_)
  } else {
    abort(paste0("Can't find existing binding in `env` for \"", nm, "\""))
  }

  invisible(maybe_missing(old))
}
scope_poke <- function(env, nm, value, create) {
  env_ <- get_env(env)
  cur <- env_

  while (!env_has(cur, nm) && !is_empty_env(cur)) {
    cur <- env_parent(cur)
  }

  if (is_empty_env(cur)) {
    if (!create) {
      abort(paste0("Can't find existing binding in `env` for \"", nm, "\""))
    }
    cur <- env_
  }

  assign(nm, value, envir = cur)
  env
}

#' Names of symbols bound in an environment
#'
#' `env_names()` returns object names from an enviroment `env` as a
#' character vector. All names are returned, even those starting with
#' a dot.
#'
#' @section Names of symbols and objects:
#'
#' Technically, objects are bound to symbols rather than strings,
#' since the R interpreter evaluates symbols (see [is_expression()] for a
#' discussion of symbolic objects versus literal objects). However it
#' is often more convenient to work with strings. In rlang
#' terminology, the string corresponding to a symbol is called the
#' _name_ of the symbol (or by extension the name of an object bound
#' to a symbol).
#'
#' @section Encoding:
#'
#' There are deep encoding issues when you convert a string to symbol
#' and vice versa. Symbols are _always_ in the native encoding (see
#' [set_chr_encoding()]). If that encoding (let's say latin1) cannot
#' support some characters, these characters are serialised to
#' ASCII. That's why you sometimes see strings looking like
#' `<U+1234>`, especially if you're running Windows (as R doesn't
#' support UTF-8 as native encoding on that platform).
#'
#' To alleviate some of the encoding pain, `env_names()` always
#' returns a UTF-8 character vector (which is fine even on Windows)
#' with unicode points unserialised.
#'
#' @inheritParams get_env
#' @return A character vector of object names.
#' @export
#' @examples
#' env <- env(a = 1, b = 2)
#' env_names(env)
env_names <- function(env) {
  nms <- names(get_env(env))
  .Call(rlang_unescape_character, nms)
}


#' What kind of environment binding?
#'
#' @param env An environment.
#' @param nms Names of bindings. Defaults to all bindings in `env`.
#'
#' @return A logical vector as long as `nms` and named after it.
#' @export
env_binding_are_active <- function(env, nms = NULL) {
  nms <- env_binding_validate_names(env, nms)
  set_names(map_lgl(nms, bindingIsActive, env = env), nms)
}
#' @rdname env_binding_are_active
#' @export
env_binding_are_promise <- function(env, nms = NULL) {
  nms <- env_binding_validate_names(env, nms)
  set_names(.Call(rlang_env_binding_are_promise, env, syms(nms)), nms)
}

env_binding_validate_names <- function(env, nms) {
  if (is_null(nms)) {
    nms <- env_names(env)
  } else {
    if (!is_character(nms)) {
      abort("`nms` must be a character vector of names")
    }
  }
  nms
}
