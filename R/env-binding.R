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
#' - `env_bind_active()` takes named _functions_ and creates active
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
#' - `env_bind_lazy()` takes named _expressions_. This is equivalent
#'   to [base::delayedAssign()]. The arguments are captured with
#'   [exprs()] (and thus support call-splicing and unquoting) and
#'   assigned to symbols in `.env`. These expressions are not
#'   evaluated immediately but lazily. Once a symbol is evaluated, the
#'   corresponding expression is evaluated in turn and its value is
#'   bound to the symbol (the expressions are thus evaluated only
#'   once, if at all).
#'
#'
#' @section Side effects:
#'
#' Since environments have reference semantics (see relevant section
#' in [env()] documentation), modifying the bindings of an environment
#' produces effects in all other references to that environment. In
#' other words, `env_bind()` and its variants have side effects.
#'
#' Like other side-effecty functions like `par()` and `options()`,
#' `env_bind()` and variants return the old values invisibly.
#'
#'
#' @section Life cycle:
#'
#' Passing an environment wrapper like a formula or a function instead
#' of an environment is soft-deprecated as of rlang 0.3.0. This
#' internal genericity was causing confusion (see issue #427). You
#' should now extract the environment separately before calling these
#' functions.
#'
#' @param .env An environment.
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
#' # You can remove bindings by supplying zap sentinels:
#' env_bind(current_env(), foo = zap())
#' try(foo)
#'
#' # Unquote-splice a named list of zaps
#' zaps <- rep_named(c("foo", "bar"), list(zap()))
#' env_bind(current_env(), !!!zaps)
#' try(bar)
#'
#' # It is most useful to change other environments:
#' my_env <- env()
#' env_bind(my_env, foo = "foo")
#' my_env$foo
#'
#' # A useful feature is to splice lists of named values:
#' vals <- list(a = 10, b = 20)
#' env_bind(my_env, !!!vals, c = 30)
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
#' # The old values of the bindings are returned invisibly:
#' old <- env_bind(my_env, a = 1, b = 2, baz = "baz")
#' old
#'
#' # You can restore the original environment state by supplying the
#' # old values back:
#' env_bind(my_env, !!!old)
env_bind <- function(.env, ...) {
  env_bind_impl(.env, list3(...), "env_bind()", bind = TRUE)
}
env_bind_impl <- function(env, data, fn, bind = FALSE, binder = NULL) {
  if (!is_vector(data)) {
    type <- as_friendly_type(type_of(data))
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

  env_ <- get_env_retired(env, fn, caller_env(2))
  nms <- names2(data)

  if (bind) {
    old <- new_list(length(nms), nms)
    overwritten <- env_has(env_, nms)
    old[overwritten] <- env_get_list(env_, nms[overwritten])
    old[!overwritten] <- list(zap())
  }

  if (is_null(binder)) {
    binder <- function(env, nm, value) {
      base::assign(nm, data[[nm]], envir = env)
    }
  }

  for (i in seq_along(data)) {
    if (bind && overwritten[[i]] && is_zap(maybe_missing(data[[i]]))) {
      base::rm(list = nms[[i]], envir = env)
    } else {
      binder(env_, nms[[i]], data[[i]])
    }
  }

  if (bind) {
    invisible(old)
  } else {
    invisible(env_)
  }
}

#' @rdname env_bind
#' @param .eval_env The environment where the expressions will be
#'   evaluated when the symbols are forced.
#' @export
#' @examples
#'
#' # env_bind_lazy() assigns expressions lazily:
#' env <- env()
#' env_bind_lazy(env, name = { cat("forced!\n"); "value" })
#'
#' # Referring to the binding will cause evaluation:
#' env$name
#'
#' # But only once, subsequent references yield the final value:
#' env$name
#'
#' # You can unquote expressions:
#' expr <- quote(message("forced!"))
#' env_bind_lazy(env, name = !!expr)
#' env$name
#'
#'
#' # By default the expressions are evaluated in the current
#' # environment. For instance we can create a local binding and refer
#' # to it, even though the variable is bound in a different
#' # environment:
#' who <- "mickey"
#' env_bind_lazy(env, name = paste(who, "mouse"))
#' env$name
#'
#' # You can specify another evaluation environment with `.eval_env`:
#' eval_env <- env(who = "minnie")
#' env_bind_lazy(env, name = paste(who, "mouse"), .eval_env = eval_env)
#' env$name
#'
#' # Or by unquoting a quosure:
#' quo <- local({
#'   who <- "fievel"
#'   quo(paste(who, "mouse"))
#' })
#' env_bind_lazy(env, name = !!quo)
#' env$name
env_bind_lazy <- function(.env, ..., .eval_env = caller_env()) {
  exprs <- exprs(...)
  exprs <- map_if(exprs, is_quosure, function(quo) call2(as_function(quo)))

  binder <- function(env, nm, value) {
    do.call("delayedAssign", list(
      x = nm,
      value = value,
      eval.env = .eval_env,
      assign.env = env
    ))
  }

  env_bind_impl(.env, exprs, "env_bind_lazy()", TRUE, binder)
}
#' @rdname env_bind
#' @export
#' @examples
#'
#' # You can create active bindings with env_bind_active(). Active
#' # bindings execute a function each time they are evaluated:
#' fn <- function() {
#'   cat("I have been called\n")
#'   rnorm(1)
#' }
#'
#' env <- env()
#' env_bind_active(env, symbol = fn)
#'
#' # `fn` is executed each time `symbol` is evaluated or retrieved:
#' env$symbol
#' env$symbol
#' eval_bare(quote(symbol), env)
#' eval_bare(quote(symbol), env)
#'
#' # All arguments are passed to as_function() so you can use the
#' # formula shortcut:
#' env_bind_active(env, foo = ~ runif(1))
#' env$foo
#' env$foo
env_bind_active <- function(.env, ...) {
  fns <- map_if(list3(...), negate(is_zap), as_function)

  existing <- env_names(.env)
  binder <- function(env, nm, value) {
    # makeActiveBinding() fails if there is already a regular binding
    if (nm %in% existing) {
      env_unbind(env, nm)
    }
    makeActiveBinding(nm, value, env)
  }

  env_bind_impl(.env, fns, "env_bind_active()", TRUE, binder)
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
  env <- get_env_retired(.env, "scoped_bindings()")

  bindings <- list2(...)
  if (!length(bindings)) {
    return(list())
  }

  old <- env_bind_impl(env, bindings, "scoped_bindings()", bind = TRUE)
  scoped_exit(frame = .frame, !!call2(env_bind_impl, env, old, bind = TRUE))

  invisible(old)
}
#' @rdname scoped_bindings
#' @param .expr An expression to evaluate with temporary bindings.
#' @export
with_bindings <- function(.expr, ..., .env = caller_env()) {
  env <- get_env_retired(.env, "with_bindings()")
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
  if (inherit) {
    while (any(exist <- env_has(env, nms, inherit = TRUE))) {
      rm(list = nms[exist], envir = env, inherits = TRUE)
    }
  } else {
    exist <- env_has(env, nms, inherit = FALSE)
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
  env <- get_env_retired(env, "env_has()")
  nms <- set_names(nms)
  map_lgl(nms, exists, envir = env, inherits = inherit)
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
env_get <- function(env = caller_env(), nm, default, inherit = FALSE) {
  env <- get_env_retired(env, "env_get()")
  if (!missing(default)) {
    exists <- env_has(env, nm, inherit = inherit)
    if (!exists) {
      return(default)
    }
  }

  # FIXME: The `inherit` case fails with missing arguments
  if (inherit) {
    return(get(nm, envir = env, inherits = TRUE))
  }

  .Call(rlang_env_get, env, nm)
}
#' @rdname env_get
#' @export
env_get_list <- function(env = caller_env(), nms, default, inherit = FALSE) {
  env <- get_env_retired(env, "env_get_list()")

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
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' `env_poke()` will assign or reassign a binding in `env` if `create`
#' is `TRUE`. If `create` is `FALSE` and a binding does not already
#' exists, an error is issued.
#'
#'
#' @details
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
#' @return The old value of `nm` or a [zap sentinel][zap] if the
#'   binding did not exist yet.
#'
#' @keywords internal
#' @export
env_poke <- function(env = caller_env(), nm, value,
                     inherit = FALSE, create = !inherit) {
  stopifnot(is_string(nm))
  env_ <- get_env_retired(env, "env_poke()")
  old <- env_get(env_, nm, inherit = inherit, default = zap())

  if (inherit) {
    scope_poke(env_, nm, value, create)
  } else if (create || env_has(env_, nm)) {
    assign(nm, value, envir = env_)
  } else {
    abort(paste0("Can't find existing binding in `env` for \"", nm, "\""))
  }

  invisible(maybe_missing(old))
}
scope_poke <- function(env, nm, value, create) {
  cur <- env

  while (!env_has(cur, nm) && !is_empty_env(cur)) {
    cur <- env_parent(cur)
  }

  if (is_empty_env(cur)) {
    if (!create) {
      abort(paste0("Can't find existing binding in `env` for \"", nm, "\""))
    }
    cur <- env
  }

  assign(nm, value, envir = cur)
  env
}

#' Names and numbers of symbols bound in an environment
#'
#' `env_names()` returns object names from an enviroment `env` as a
#' character vector. All names are returned, even those starting with
#' a dot. `env_length()` returns the number of bindings.
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
  env <- get_env_retired(env, "env_names()")
  nms <- names(env)
  .Call(rlang_unescape_character, nms)
}

#' @rdname env_names
#' @export
env_length <- function(env) {
  if (!is_environment(env)) {
    abort("`env` must be an environment")
  }
  length(env)
}

#' Lock or unlock environment bindings
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' Locked environment bindings trigger an error when an attempt is
#' made to redefine the binding.
#'
#' @param env An environment.
#' @param nms Names of bindings. Defaults to all bindings in `env`.
#'
#' @return `env_binding_are_unlocked()` returns a logical vector as
#'   long as `nms` and named after it. `env_binding_lock()` and
#'   `env_binding_unlock()` return the old value of
#'   `env_binding_are_unlocked()` invisibly.
#'
#' @seealso [env_lock()] for locking an environment.
#'
#' @keywords internal
#' @export
#' @examples
#' # Bindings are unlocked by default:
#' env <- env(a = "A", b = "B")
#' env_binding_are_locked(env)
#'
#' # But can optionally be locked:
#' env_binding_lock(env, "a")
#' env_binding_are_locked(env)
#'
#' # If run, the following would now return an error because `a` is locked:
#' # env_bind(env, a = "foo")
#' # with_env(env, a <- "bar")
#'
#' # Let's unlock it. Note that the return value indicate which
#' # bindings were locked:
#' were_locked <- env_binding_unlock(env)
#' were_locked
#'
#' # Now that it is unlocked we can modify it again:
#' env_bind(env, a = "foo")
#' with_env(env, a <- "bar")
#' env$a
env_binding_lock <- function(env, nms = NULL) {
  nms <- env_binding_validate_names(env, nms)
  old <- env_binding_are_locked(env, nms)
  map(nms, lockBinding, env = env)
  invisible(old)
}
#' @rdname env_binding_lock
#' @export
env_binding_unlock <- function(env, nms = NULL) {
  nms <- env_binding_validate_names(env, nms)
  old <- env_binding_are_locked(env, nms)
  map(nms, unlockBinding, env = env)
  invisible(old)
}
#' @rdname env_binding_lock
#' @export
env_binding_are_locked <- function(env, nms = NULL) {
  nms <- env_binding_validate_names(env, nms)
  set_names(map_lgl(nms, bindingIsLocked, env = env), nms)
}

#' What kind of environment binding?
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
#'
#' @inheritParams env_binding_lock
#'
#' @keywords internal
#' @return A logical vector as long as `nms` and named after it.
#' @export
env_binding_are_active <- function(env, nms = NULL) {
  env_binding_are_type(env, nms, 2L)
}
#' @rdname env_binding_are_active
#' @export
env_binding_are_lazy <- function(env, nms = NULL) {
  env_binding_are_type(env, nms, 1L)
}
env_binding_are_type <- function(env, nms, type) {
  nms <- env_binding_validate_names(env, nms)
  promise <- env_binding_types(env, nms)

  if (is_null(promise)) {
    promise <- rep(FALSE, length(nms))
  } else {
    promise <- promise == type
  }
  set_names(promise, nms)
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
env_binding_types <- function(env, nms = env_names(env)) {
  .Call(rlang_env_binding_types, env, nms)
}

env_binding_type_sum <- function(env, nms = NULL) {
  nms <- env_binding_validate_names(env, nms)

  active <- env_binding_are_active(env, nms)
  promise <- env_binding_are_lazy(env, nms)
  other <- !active & !promise

  types <- new_character(length(nms), nms)
  types[active] <- "active"
  types[promise] <- "lazy"
  types[other] <- map_chr(env_get_list(env, nms[other]), rlang_type_sum)

  types
}
