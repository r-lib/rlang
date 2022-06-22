#' Create a new environment
#'
#' @description
#'
#' These functions create new environments.
#'
#' * `env()` creates a child of the current environment by default
#'   and takes a variable number of named objects to populate it.
#'
#' * `new_environment()` creates a child of the empty environment by
#'   default and takes a named list of objects to populate it.
#'
#'
#' @section Environments as objects:
#'
#' Environments are containers of uniquely named objects. Their most
#' common use is to provide a scope for the evaluation of R
#' expressions. Not all languages have first class environments,
#' i.e. can manipulate scope as regular objects. Reification of scope
#' is one of the most powerful features of R as it allows you to change
#' what objects a function or expression sees when it is evaluated.
#'
#' Environments also constitute a data structure in their own
#' right. They are a collection of uniquely named objects, subsettable
#' by name and modifiable by reference. This latter property (see
#' section on reference semantics) is especially useful for creating
#' mutable OO systems (cf the [R6 package](https://github.com/r-lib/R6)
#' and the [ggproto
#' system](https://ggplot2.tidyverse.org/articles/extending-ggplot2.html)
#' for extending ggplot2).
#'
#'
#' @section Inheritance:
#'
#' All R environments (except the [empty environment][empty_env]) are
#' defined with a parent environment. An environment and its
#' grandparents thus form a linear hierarchy that is the basis for
#' [lexical
#' scoping](https://en.wikipedia.org/wiki/Scope_(computer_science)) in
#' R. When R evaluates an expression, it looks up symbols in a given
#' environment. If it cannot find these symbols there, it keeps
#' looking them up in parent environments. This way, objects defined
#' in child environments have precedence over objects defined in
#' parent environments.
#'
#' The ability of overriding specific definitions is used in the
#' tidyeval framework to create powerful domain-specific grammars. A
#' common use of masking is to put data frame columns in scope. See
#' for example [as_data_mask()].
#'
#'
#' @section Reference semantics:
#'
#' Unlike regular objects such as vectors, environments are an
#' [uncopyable][is_copyable()] object type. This means that if you
#' have multiple references to a given environment (by assigning the
#' environment to another symbol with `<-` or passing the environment
#' as argument to a function), modifying the bindings of one of those
#' references changes all other references as well.
#'
#'
#' @section Life cycle:
#'
#' - `child_env()` is in the questioning stage. It is redundant now
#'   that `env()` accepts parent environments.
#'
#' @param ...,data <[dynamic][dyn-dots]> Named values. You can
#'   supply one unnamed to specify a custom parent, otherwise it
#'   defaults to the current environment.
#' @param .parent,parent A parent environment.
#' @seealso [env_has()], [env_bind()].
#' @export
#' @examples
#' # env() creates a new environment which has the current environment
#' # as parent
#' env <- env(a = 1, b = "foo")
#' env$b
#' identical(env_parent(env), current_env())
#'
#' # Supply one unnamed argument to override the default:
#' env <- env(base_env(), a = 1, b = "foo")
#' identical(env_parent(env), base_env())
#'
#'
#' # child_env() lets you specify a parent:
#' child <- child_env(env, c = "bar")
#' identical(env_parent(child), env)
#'
#' # This child environment owns `c` but inherits `a` and `b` from `env`:
#' env_has(child, c("a", "b", "c", "d"))
#' env_has(child, c("a", "b", "c", "d"), inherit = TRUE)
#'
#' # `parent` is passed to as_environment() to provide handy
#' # shortcuts. Pass a string to create a child of a package
#' # environment:
#' child_env("rlang")
#' env_parent(child_env("rlang"))
#'
#' # Or `NULL` to create a child of the empty environment:
#' child_env(NULL)
#' env_parent(child_env(NULL))
#'
#' # The base package environment is often a good default choice for a
#' # parent environment because it contains all standard base
#' # functions. Also note that it will never inherit from other loaded
#' # package environments since R keeps the base package at the tail
#' # of the search path:
#' base_child <- child_env("base")
#' env_has(base_child, c("lapply", "("), inherit = TRUE)
#'
#' # On the other hand, a child of the empty environment doesn't even
#' # see a definition for `(`
#' empty_child <- child_env(NULL)
#' env_has(empty_child, c("lapply", "("), inherit = TRUE)
#'
#' # Note that all other package environments inherit from base_env()
#' # as well:
#' rlang_child <- child_env("rlang")
#' env_has(rlang_child, "env", inherit = TRUE)     # rlang function
#' env_has(rlang_child, "lapply", inherit = TRUE)  # base function
#'
#'
#' # Both env() and child_env() support tidy dots features:
#' objs <- list(b = "foo", c = "bar")
#' env <- env(a = 1, !!! objs)
#' env$c
#'
#' # You can also unquote names with the definition operator `:=`
#' var <- "a"
#' env <- env(!!var := "A")
#' env$a
#'
#'
#' # Use new_environment() to create containers with the empty
#' # environment as parent:
#' env <- new_environment()
#' env_parent(env)
#'
#' # Like other new_ constructors, it takes an object rather than dots:
#' new_environment(list(a = "foo", b = "bar"))
env <- function(...) {
  dots <- dots_split(..., .n_unnamed = 0:1)

  if (length(dots$unnamed)) {
    parent <- dots$unnamed[[1]]
  } else {
    parent = caller_env()
  }

  env <- new.env(parent = parent)
  env_bind0(env, dots$named)
  env
}
#' @rdname env
#' @export
child_env <- function(.parent, ...) {
  env <- new.env(parent = as_environment(.parent))
  env_bind0(env, list2(...))
  env
}
#' @rdname env
#' @export
new_environment <- function(data = list(), parent = empty_env()) {
  env <- new.env(parent = parent)

  if (!is_list(data)) {
    data <- rlang_as_list(data)
  }

  env_bind0(env, data)
  env
}

#' Coerce to an environment
#'
#' `as_environment()` coerces named vectors (including lists) to an
#' environment. The names must be unique. If supplied an unnamed
#' string, it returns the corresponding package environment (see
#' [pkg_env()]).
#'
#' If `x` is an environment and `parent` is not `NULL`, the
#' environment is duplicated before being set a new parent. The return
#' value is therefore a different environment than `x`.
#'
#' @param x An object to coerce.
#' @param parent A parent environment, [empty_env()] by default. This
#'   argument is only used when `x` is data actually coerced to an
#'   environment (as opposed to data representing an environment, like
#'   `NULL` representing the empty environment).
#' @export
#' @examples
#' # Coerce a named vector to an environment:
#' env <- as_environment(mtcars)
#'
#' # By default it gets the empty environment as parent:
#' identical(env_parent(env), empty_env())
#'
#'
#' # With strings it is a handy shortcut for pkg_env():
#' as_environment("base")
#' as_environment("rlang")
#'
#' # With NULL it returns the empty environment:
#' as_environment(NULL)
as_environment <- function(x, parent = NULL) {
  if (is_string(x) && !is_named(x)) {
    return(pkg_env(x))
  }

  switch(typeof(x),
    NULL = empty_env(),
    environment = x,
    logical = ,
    integer = ,
    double = ,
    character = ,
    complex = ,
    raw = ,
    list = vec_as_environment(x, parent),
    abort_coercion(x, "an environment")
  )
}
vec_as_environment <- function(x, parent = NULL) {
  stopifnot(is_dictionaryish(x))
  if (is_atomic(x)) {
    x <- vec_coerce(x, "list")
  }
  list2env(x, parent = parent %||% empty_env())
}

#' Get parent environments
#'
#' @description
#'
#' - `env_parent()` returns the parent environment of `env` if called
#'   with `n = 1`, the grandparent with `n = 2`, etc.
#'
#' - `env_tail()` searches through the parents and returns the one
#'   which has [empty_env()] as parent.
#'
#' - `env_parents()` returns the list of all parents, including the
#'   empty environment. This list is named using [env_name()].
#'
#' See the section on _inheritance_ in [env()]'s documentation.
#'
#' @inheritParams get_env
#' @param n The number of generations to go up.
#' @param last The environment at which to stop. Defaults to the
#'   global environment. The empty environment is always a stopping
#'   condition so it is safe to leave the default even when taking the
#'   tail or the parents of an environment on the search path.
#'
#'   `env_tail()` returns the environment which has `last` as parent
#'   and `env_parents()` returns the list of environments up to `last`.
#' @return An environment for `env_parent()` and `env_tail()`, a list
#'   of environments for `env_parents()`.
#' @export
#' @examples
#' # Get the parent environment with env_parent():
#' env_parent(global_env())
#'
#' # Or the tail environment with env_tail():
#' env_tail(global_env())
#'
#' # By default, env_parent() returns the parent environment of the
#' # current evaluation frame. If called at top-level (the global
#' # frame), the following two expressions are equivalent:
#' env_parent()
#' env_parent(base_env())
#'
#' # This default is more handy when called within a function. In this
#' # case, the enclosure environment of the function is returned
#' # (since it is the parent of the evaluation frame):
#' enclos_env <- env()
#' fn <- set_env(function() env_parent(), enclos_env)
#' identical(enclos_env, fn())
env_parent <- function(env = caller_env(), n = 1) {
  check_environment(env)

  while (n > 0) {
    if (is_empty_env(env)) {
      abort("The empty environment has no parent.")
    }
    n <- n - 1
    env <- parent.env(env)
  }

  env
}
#' @rdname env_parent
#' @export
env_tail <- function(env = caller_env(), last = global_env()) {
  check_environment(env)
  parent <- env_parent(env)

  while (!identical(parent, last) && !is_empty_env(parent)) {
    env <- parent
    parent <- env_parent(parent)
  }

  env
}
#' @rdname env_parent
#' @export
env_parents <- function(env = caller_env(), last = global_env()) {
  if (is_empty_env(env)) {
    return(new_environments(list()))
  }

  n <- env_depth(env)
  out <- new_list(n)

  if (!is_null(last)) {
    check_environment(last, what = "`NULL` or an environment")
  }

  i <- 1L
  parent <- env_parent(env)

  while (TRUE) {
    out[[i]] <- parent

    if (is_reference(parent, last) || is_empty_env(parent)) {
      break
    }

    i <- i + 1L
    env <- parent
    parent <- env_parent(env)
  }

  if (i < n) {
    out <- out[seq_len(i)]
  }

  new_environments(out)
}

#' Depth of an environment chain
#'
#' This function returns the number of environments between `env` and
#' the [empty environment][empty_env()], including `env`. The depth of
#' `env` is also the number of parents of `env` (since the empty
#' environment counts as a parent).
#'
#' @inheritParams get_env
#' @return An integer.
#' @seealso The section on inheritance in [env()] documentation.
#' @export
#' @examples
#' env_depth(empty_env())
#' env_depth(pkg_env("rlang"))
env_depth <- function(env) {
  check_environment(env)

  n <- 0L
  while (!is_empty_env(env)) {
    env <- env_parent(env)
    n <- n + 1L
  }

  n
}
`_empty_env` <- emptyenv()
is_empty_env <- function(env) {
  is_reference(env, `_empty_env`)
}

#' Get or set the environment of an object
#'
#' These functions dispatch internally with methods for functions,
#' formulas and frames. If called with a missing argument, the
#' environment of the current evaluation frame is returned. If you
#' call `get_env()` with an environment, it acts as the identity
#' function and the environment is simply returned (this helps
#' simplifying code when writing generic functions for environments).
#'
#' While `set_env()` returns a modified copy and does not have side
#' effects, `env_poke_parent()` operates changes the environment by
#' side effect. This is because environments are
#' [uncopyable][is_copyable]. Be careful not to change environments
#' that you don't own, e.g. a parent environment of a function from a
#' package.
#'
#' @param env An environment.
#' @param default The default environment in case `env` does not wrap
#'   an environment. If `NULL` and no environment could be extracted,
#'   an error is issued.
#'
#' @seealso [quo_get_env()] and [quo_set_env()] for versions of
#'   [get_env()] and [set_env()] that only work on quosures.
#' @export
#' @examples
#' # Environment of closure functions:
#' fn <- function() "foo"
#' get_env(fn)
#'
#' # Or of quosures or formulas:
#' get_env(~foo)
#' get_env(quo(foo))
#'
#'
#' # Provide a default in case the object doesn't bundle an environment.
#' # Let's create an unevaluated formula:
#' f <- quote(~foo)
#'
#' # The following line would fail if run because unevaluated formulas
#' # don't bundle an environment (they didn't have the chance to
#' # record one yet):
#' # get_env(f)
#'
#' # It is often useful to provide a default when you're writing
#' # functions accepting formulas as input:
#' default <- env()
#' identical(get_env(f, default), default)
get_env <- function(env, default = NULL) {
  out <- switch(typeof(env),
    environment = env,
    definition = ,
    language = if (is_formula(env)) attr(env, ".Environment"),
    builtin = ,
    special = ,
    primitive = ns_env("base"),
    closure = environment(env)
  )

  out <- out %||% default

  if (is_null(out)) {
    type <- obj_type_friendly(env)
    abort(paste0("Can't extract an environment from ", type, "."))
  } else {
    out
  }
}

#' @rdname get_env
#' @param new_env An environment to replace `env` with.
#' @export
#' @examples
#'
#' # set_env() can be used to set the enclosure of functions and
#' # formulas. Let's create a function with a particular environment:
#' env <- child_env("base")
#' fn <- set_env(function() NULL, env)
#'
#' # That function now has `env` as enclosure:
#' identical(get_env(fn), env)
#' identical(get_env(fn), current_env())
#'
#' # set_env() does not work by side effect. Setting a new environment
#' # for fn has no effect on the original function:
#' other_env <- child_env(NULL)
#' set_env(fn, other_env)
#' identical(get_env(fn), other_env)
#'
#' # Since set_env() returns a new function with a different
#' # environment, you'll need to reassign the result:
#' fn <- set_env(fn, other_env)
#' identical(get_env(fn), other_env)
set_env <- function(env, new_env = caller_env()) {
  if (is_formula(env) || is_closure(env)) {
    environment(env) <- new_env
    return(env)
  }

  check_environment(env)
  new_env
}
#' @rdname get_env
#' @export
env_poke_parent <- function(env, new_env) {
  check_environment(env)
  check_environment(new_env)
  .Call(ffi_env_poke_parent, env, new_env)
}
`env_parent<-` <- function(x, value) {
  check_environment(env)
  check_environment(value)
  .Call(ffi_env_poke_parent, env, value)
}


#' Clone or coalesce an environment
#'
#' @description
#' - `env_clone()` creates a new environment containing exactly the
#'   same bindings as the input, optionally with a new parent.
#'
#' - `env_coalesce()` copies binding from the RHS environment into the
#'   LHS. If the RHS already contains bindings with the same name as
#'   in the LHS, those are kept as is.
#'
#' Both these functions preserve active bindings and promises (the
#' latter are only preserved on R >= 4.0.0).
#'
#' @inheritParams get_env
#' @param parent The parent of the cloned environment.
#' @export
#' @examples
#' # A clone initially contains the same bindings as the original
#' # environment
#' env <- env(a = 1, b = 2)
#' clone <- env_clone(env)
#'
#' env_print(clone)
#' env_print(env)
#'
#' # But it can acquire new bindings or change existing ones without
#' # impacting the original environment
#' env_bind(clone, a = "foo", c = 3)
#'
#' env_print(clone)
#' env_print(env)
#'
#'
#' # `env_coalesce()` copies bindings from one environment to another
#' lhs <- env(a = 1)
#' rhs <- env(a = "a", b = "b", c = "c")
#' env_coalesce(lhs, rhs)
#' env_print(lhs)
#'
#' # To copy all the bindings from `rhs` into `lhs`, first delete the
#' # conflicting bindings from `rhs`
#' env_unbind(lhs, env_names(rhs))
#' env_coalesce(lhs, rhs)
#' env_print(lhs)
env_clone <- function(env, parent = env_parent(env)) {
  check_environment(env)
  check_environment(parent)
  .Call(ffi_env_clone, env, parent)
}
#' @rdname env_clone
#' @param from Environment to copy bindings from.
#' @export
env_coalesce <- function(env, from) {
  check_environment(env)
  check_environment(from)
  invisible(.Call(ffi_env_coalesce, env, from))
}

#' Does environment inherit from another environment?
#'
#' This returns `TRUE` if `x` has `ancestor` among its parents.
#'
#' @inheritParams get_env
#' @param ancestor Another environment from which `x` might inherit.
#' @export
env_inherits <- function(env, ancestor) {
  check_environment(env)
  check_environment(ancestor)
  .Call(ffi_env_inherits, env, ancestor)
}

#' Lock an environment
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Locked environments cannot be modified. An important example is
#' namespace environments which are locked by R when loaded in a
#' session. Once an environment is locked it normally cannot be
#' unlocked.
#'
#' Note that only the environment as a container is locked, not the
#' individual bindings. You can't remove or add a binding but you can
#' still modify the values of existing bindings. See
#' [env_binding_lock()] for locking individual bindings.
#'
#' @param env An environment.
#' @return The old value of `env_is_locked()` invisibly.
#'
#' @seealso [env_binding_lock()]
#' @keywords internal
#' @export
#' @examples
#' # New environments are unlocked by default:
#' env <- env(a = 1)
#' env_is_locked(env)
#'
#' # Use env_lock() to lock them:
#' env_lock(env)
#' env_is_locked(env)
#'
#' # Now that `env` is locked, it is no longer possible to remove or
#' # add bindings. If run, the following would fail:
#' # env_unbind(env, "a")
#' # env_bind(env, b = 2)
#'
#' # Note that even though the environment as a container is locked,
#' # the individual bindings are still unlocked and can be modified:
#' env$a <- 10
env_lock <- function(env) {
  old <- env_is_locked(env)
  lockEnvironment(env)
  invisible(old)
}
#' @rdname env_lock
#' @export
env_is_locked <- function(env) {
  environmentIsLocked(env)
}

#' Unlock an environment
#'
#' This function should only be used in development tools or
#' interactively.
#'
#' @inheritParams env_lock
#' @return Whether the environment has been unlocked.
#'
#' @keywords internal
#' @export
env_unlock <- function(env) {
  invisible(.Call(ffi_env_unlock, env))
}


#' Pretty-print an environment
#'
#' @description
#'
#' This prints:
#'
#' * The [label][env_label] and the parent label.
#'
#' * Whether the environment is [locked][env_lock].
#'
#' * The bindings in the environment (up to 20 bindings). They are
#'   printed succintly using `pillar::type_sum()` (if available,
#'   otherwise uses an internal version of that generic). In addition
#'   [fancy bindings][env_bind_lazy] (actives and promises) are
#'   indicated as such.
#'
#' * Locked bindings get a `[L]` tag
#'
#' Note that printing a package namespace (see [ns_env()]) with
#' `env_print()` will typically tag function bindings as `<lazy>`
#' until they are evaluated the first time. This is because package
#' functions are lazily-loaded from disk to improve performance when
#' loading a package.
#'
#' @param env An environment, or object that can be converted to an
#'   environment by [get_env()].
#'
#' @export
env_print <- function(env = caller_env()) {
  env <- get_env(env)

  if (is_empty_env(env)) {
    parent <- "NULL"
  } else {
    parent <- sprintf("<environment: %s>", env_label(env_parent(env)))
  }

  if (env_is_locked(env)) {
    locked <- " [L]"
  } else {
    locked <- ""
  }

  header <- format_cls(sprintf("environment: %s", env_label(env)))
  cat_line(
    bold(paste0(header, locked)),
    sprintf("Parent: %s", parent)
  )

  class <- attr(env, "class")
  if (is_character(class)) {
    class <- paste(class, collapse = ", ")
    cat_line(sprintf("Class: %s", class))
  }

  nms <- env_names(env)
  n <- length(nms)

  if (n) {
    cat_line("Bindings:")

    if (n > 20) {
      other <- nms[seq(21L, n)]
      nms <- nms[1:20]
    } else {
      other <- chr()
    }

    escaped_nms <- map_chr(syms(nms), deparse, backtick = TRUE)

    types <- env_binding_type_sum(env, nms)
    types <- paste0(escaped_nms, ": <", types, ">")

    locked <- env_binding_are_locked(env, nms)
    locked <- ifelse(locked, " [L]", "")
    types <- paste0(types, locked)

    types <- set_names(types, "*")

    n_other <- length(other)
    if (n_other) {
      types <- c(types, sprintf("... with %s more bindings", n_other))
    }

    writeLines(format_error_bullets(types))
  }

  invisible(env)
}

new_environments <- function(envs, names) {
  stopifnot(is_list(envs))
  structure(
    envs,
    names = map_chr(unname(envs), env_name),
    class = "rlang_envs"
  )
}

#' @export
print.rlang_envs <- function(x, ...) {
  n <- length(x)
  if (!n) {
    print(list())
    return(invisible(x))
  }
  if (n > 20L) {
    footer <- sprintf("... and %s more environments", n - 20L)
    x <- x[seq_len(20L)]
  } else {
    footer <- chr()
  }

  digits <- n_digits(seq_along(x))
  pads <- digits[[length(x)]] - digits
  pads <- map_chr(pads, spaces)

  labels <- map_chr(x, env_label)
  nms_tags <- names_tags(names(x))

  cat_line(
    paste0(pads, "[[", seq_along(x), "]]", nms_tags, " <env: ", labels, ">"),
    footer
  )

  invisible(x)
}
n_digits <- function(x) {
  floor(log10(x) + 1)
}
names_tags <- function(nms) {
  if (is_null(nms)) {
    return("")
  }

  invalid <- detect_void_name(nms)
  if (all(invalid)) {
    return("")
  }

  ifelse(invalid, "  ", " $")
}

#' @export
c.rlang_envs <- function(...) {
  new_environments(NextMethod())
}
#' @export
`[.rlang_envs` <- function(x, i) {
  new_environments(NextMethod())
}

#' @export
str.rlang_envs <- function(object, ...) {
  i <- 0
  for (env in object) {
    i <- inc(i)
    cat(sprintf("[[%s]]\n", i))
    env_print(env)
    cat("\n")
  }
  invisible(object)
}

#' Browse environments
#'
#' @description
#'
#' * `env_browse(env)` is equivalent to evaluating `browser()` in
#'   `env`. It persistently sets the environment for step-debugging.
#'   Supply `value = FALSE` to disable browsing.
#'
#' * `env_is_browsed()` is a predicate that inspects whether an
#'   environment is being browsed.
#'
#' @param env An environment.
#' @param value Whether to browse `env`.
#' @return `env_browse()` returns the previous value of
#'   `env_is_browsed()` (a logical), invisibly.
#' @export
env_browse <- function(env, value = TRUE) {
  invisible(.Call(ffi_env_browse, env, value))
}
#' @rdname env_browse
#' @export
env_is_browsed <- function(env) {
  .Call(ffi_env_is_browsed, env)
}
