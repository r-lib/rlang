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
#' mutable OO systems (cf the [R6 package](https://github.com/wch/R6)
#' and the [ggproto
#' system](http://ggplot2.tidyverse.org/articles/extending-ggplot2.html)
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
#' @param ...,data Named values. You can supply one unnamed to specify
#'   a custom parent, otherwise it defaults to the current
#'   environment. These dots support [tidy dots][tidy-dots]
#'   features.
#' @param .parent,parent A parent environment. Can be an object
#'   supported by [as_environment()].
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
  env_bind_impl(env, dots$named)
  env
}
#' @rdname env
#' @export
child_env <- function(.parent, ...) {
  env <- new.env(parent = as_environment(.parent))
  env_bind_impl(env, list2(...))
  env
}
#' @rdname env
#' @export
new_environment <- function(data = list(), parent = empty_env()) {
  env <- new.env(parent = parent)
  env_bind_impl(env, data)
  env
}

#' Coerce to an environment
#'
#' `as_environment()` coerces named vectors (including lists) to an
#' environment. It first checks that `x` is a dictionary (see
#' [is_dictionaryish()]). If supplied an unnamed string, it returns the
#' corresponding package environment (see [pkg_env()]).
#'
#' If `x` is an environment and `parent` is not `NULL`, the
#' environment is duplicated before being set a new parent. The return
#' value is therefore a different environment than `x`.
#'
#'
#' @section Life cycle:
#'
#' `as_env()` was soft-deprecated and renamed to `as_environment()` in
#' rlang 0.2.0. This is for consistency as type predicates should not
#' be abbreviated.
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
  coerce_type(x, "an environment",
    NULL = {
      empty_env()
    },
    environment = {
      x
    },
    string = {
      if (length(x) > 1 || is_named(x)) {
        return(as_env_(x, parent))
      }
      pkg_env(x)
    },
    logical = ,
    integer = ,
    double = ,
    complex = ,
    character = ,
    raw = ,
    list = {
      as_env_(x, parent)
    }
  )
}
as_env_ <- function(x, parent = NULL) {
  stopifnot(is_dictionaryish(x))
  if (is_atomic(x)) {
    x <- as_list(x)
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
#'
#' @section Life cycle:
#'
#' The `sentinel` argument of `env_tail()` has been deprecated in
#' rlang 0.2.0 and renamed to `last`.
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
#' @param sentinel This argument is soft-deprecated, please use `last`
#'   instead.
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
  env_ <- get_env_retired(env, "env_parent()")

  while (n > 0) {
    if (is_empty_env(env_)) {
      abort("The empty environment has no parent")
    }
    n <- n - 1
    env_ <- parent.env(env_)
  }

  env_
}
#' @rdname env_parent
#' @export
env_tail <- function(env = caller_env(), last = global_env(),
                     sentinel = NULL) {
  if (!is_null(sentinel)) {
    warn_deprecated(paste_line(
      "`sentinel` is deprecated as of version 0.3.0.",
      "Please use `last` instead."
    ))
    last <- sentinel
  }

  env_ <- get_env_retired(env, "env_tail()")
  parent <- env_parent(env_)

  while (!is_reference(parent, last) && !is_empty_env(parent)) {
    env_ <- parent
    parent <- env_parent(parent)
  }

  env_
}
#' @rdname env_parent
#' @export
env_parents <- function(env = caller_env(), last = global_env()) {
  if (is_empty_env(env)) {
    return(new_environments(list()))
  }

  n <- env_depth(env)
  out <- new_list(n)

  if (!typeof(last) %in% c("environment", "NULL")) {
    abort("`last` must be `NULL` or an environment")
  }

  i <- 1L
  parent <- env_parent(env)

  while (TRUE) {
    out[[i]] <- parent
    i <- i + 1L

    if (is_reference(parent, last) || is_empty_env(parent)) {
      break
    }

    env <- parent
    parent <- env_parent(env)
  }

  if (i < n) {
    out <- out[seq_len(i - 1L)]
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
  env_ <- get_env_retired(env, "env_depth()")

  n <- 0L
  while (!is_empty_env(env_)) {
    env_ <- env_parent(env_)
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
#' environment of the current evaluation frame (see [ctxt_stack()]) is
#' returned. If you call `get_env()` with an environment, it acts as
#' the identity function and the environment is simply returned (this
#' helps simplifying code when writing generic functions for
#' environments).
#'
#' While `set_env()` returns a modified copy and does not have side
#' effects, `env_poke_parent()` operates changes the environment by
#' side effect. This is because environments are
#' [uncopyable][is_copyable]. Be careful not to change environments
#' that you don't own, e.g. a parent environment of a function from a
#' package.
#'
#'
#' @section Life cycle:
#'
#' - Using `get_env()` without supplying `env` is soft-deprecated as
#'   of rlang 0.3.0. Please use [current_env()] to retrieve the
#'   current environment.
#'
#' - Passing environment wrappers like formulas or functions instead
#'   of bare environments is soft-deprecated as of rlang 0.3.0. This
#'   internal genericity was causing confusion (see issue #427). You
#'   should now extract the environment separately before calling
#'   these functions.
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
  if (missing(env)) {
    signal_soft_deprecated("The `env` argument of `get_env()` must now be supplied")
    env <- caller_env()
  }

  out <- switch_type(env,
    environment = env,
    definition = ,
    formula = attr(env, ".Environment"),
    primitive = base_env(),
    closure = environment(env),
    list = switch_class(env, frame = env$env)
  )

  out <- out %||% default

  if (is_null(out)) {
    type <- friendly_type(type_of(env))
    abort(paste0("Can't extract an environment from ", type))
  } else {
    out
  }
}
get_env_retired <- function(x, fn, env = caller_env(2)) {
  if (is_environment(x)) {
    return(x)
  }

  type <- friendly_type_of(x)
  signal_soft_deprecated(env = env, paste_line(
    sprintf("Passing an environment wrapper like %s is deprecated.", type),
    sprintf("Please retrieve the environment before calling `%s`", fn)
  ))

  get_env(x)
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
  new_env <- get_env_retired(new_env, "set_env()")

  switch_type(env,
    definition = ,
    formula = ,
    closure = {
      environment(env) <- new_env
      env
    },
    environment = new_env,
    abort(paste0(
      "Can't set environment for ", friendly_type(type_of(env)), ""
    ))
  )
}
#' @rdname get_env
#' @export
env_poke_parent <- function(env, new_env) {
  env <- get_env_retired(env, "env_poke_parent()")
  new_env <- get_env_retired(new_env, "env_poke_parent()")
  .Call(rlang_env_poke_parent, env, new_env)
}
`env_parent<-` <- function(x, value) {
  env <- get_env_retired(x, "env_poke_parent<-")
  .Call(rlang_env_poke_parent, env, value)
}


#' Clone an environment
#'
#' This creates a new environment containing exactly the same objects,
#' optionally with a new parent.
#'
#' @inheritParams get_env
#' @param parent The parent of the cloned environment.
#' @export
#' @examples
#' env <- env(!!! mtcars)
#' clone <- env_clone(env)
#' identical(env, clone)
#' identical(env$cyl, clone$cyl)
env_clone <- function(env, parent = env_parent(env)) {
  env <- get_env_retired(env, "env_clone()")
  .Call(rlang_env_clone, env, parent)
}

#' Does environment inherit from another environment?
#'
#' This returns `TRUE` if `x` has `ancestor` among its parents.
#'
#' @inheritParams get_env
#' @param ancestor Another environment from which `x` might inherit.
#' @export
env_inherits <- function(env, ancestor) {
  env <- get_env_retired(env, "env_inherits()")
  stopifnot(is_environment(ancestor) && is_environment(env))

  while (!is_empty_env(env_parent(env))) {
    env <- env_parent(env)
    if (is_reference(env, ancestor)) {
      return(TRUE)
    }
  }

  is_empty_env(env)
}

#' Lock an environment
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("experimental")}
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
  invisible(.Call(rlang_env_unlock, env))
}


#' Scoped environments
#'
#' @description
#'
#' Scoped environments are named environments which form a
#' parent-child hierarchy called the search path. They define what
#' objects you can see (are in scope) from your workspace. They
#' typically are package environments, i.e. special environments
#' containing all exported functions from a package (and whose parent
#' environment is the package namespace, which also contains
#' unexported functions). Package environments are attached to the
#' search path with [base::library()]. Note however that any
#' environment can be attached to the search path, for example with
#' the unrecommended [base::attach()] base function which transforms
#' vectors to scoped environments.
#'
#' - You can list all scoped environments with `scoped_names()`. Unlike
#'   [base::search()], it also mentions the empty environment that
#'   terminates the search path (it is given the name `"NULL"`).
#'
#' - `scoped_envs()` returns all environments on the search path,
#'   including the empty environment.
#'
#' - `pkg_env()` takes a package name and returns the scoped
#'   environment of packages if they are attached to the search path,
#'   and throws an error otherwise.
#'
#' - `is_scoped()` allows you to check whether a named environment is
#'   on the search path.
#'
#'
#' @section Search path:
#'
#' The search path is a chain of scoped environments where newly
#' attached environments are the childs of earlier ones. However, the
#' global environment, where everything you define at top-level ends
#' up, is pinned as the head of that linked chain. Likewise, the base
#' package environment is pinned as the tail of the chain. You can
#' retrieve those environments with `global_env()` and `base_env()`
#' respectively. The global environment is also the environment of the
#' very first evaluation frame on the stack, see [global_frame()] and
#' [ctxt_stack()].
#'
#'
#' @section Life cycle:
#'
#' These functions are experimental and may not belong to the rlang
#' package. Expect API changes.
#'
#' @param nm The name of an environment attached to the search
#'   path. Call [base::search()] to see what is currently on the path.
#'
#' @keywords internal
#' @export
#' @examples
#' # List the names of scoped environments:
#' nms <- scoped_names()
#' nms
#'
#' # The global environment is always the first in the chain:
#' scoped_env(nms[[1]])
#'
#' # And the scoped environment of the base package is always the last:
#' scoped_env(nms[[length(nms)]])
#'
#' # These two environments have their own shortcuts:
#' global_env()
#' base_env()
#'
#' # Packages appear in the search path with a special name. Use
#' # pkg_env_name() to create that name:
#' pkg_env_name("rlang")
#' scoped_env(pkg_env_name("rlang"))
#'
#' # Alternatively, get the scoped environment of a package with
#' # pkg_env():
#' pkg_env("utils")
scoped_env <- function(nm) {
  if (identical(nm, "NULL")) {
    return(empty_env())
  }
  if (!is_scoped(nm)) {
    stop(paste0(nm, " is not in scope"), call. = FALSE)
  }
  as.environment(nm)
}
#' @rdname scoped_env
#' @param pkg The name of a package.
#' @export
pkg_env <- function(pkg) {
  pkg_name <- pkg_env_name(pkg)
  scoped_env(pkg_name)
}
#' @rdname scoped_env
#' @export
pkg_env_name <- function(pkg) {
  paste0("package:", pkg)
}

#' @rdname scoped_env
#' @export
scoped_names <- function() {
  c(search(), "NULL")
}
#' @rdname scoped_env
#' @export
scoped_envs <- function() {
  envs <- c(.GlobalEnv, env_parents(.GlobalEnv))
  set_names(envs, scoped_names())
}
#' @rdname scoped_env
#' @export
is_scoped <- function(nm) {
  if (!is_scalar_character(nm)) {
    stop("`nm` must be a string", call. = FALSE)
  }
  nm %in% scoped_names()
}

#' @rdname scoped_env
#' @export
base_env <- baseenv
#' @rdname scoped_env
#' @export
global_env <- globalenv

#' Get the empty environment
#'
#' The empty environment is the only one that does not have a parent.
#' It is always used as the tail of a scope chain such as the search
#' path (see [scoped_names()]).
#'
#' @export
#' @examples
#' # Create environments with nothing in scope:
#' child_env(empty_env())
empty_env <- emptyenv

#' Get the current or caller environment
#'
#' @description
#'
#' * The current environment is the execution environment of the
#'   current function (the one currently being evaluated).
#'
#' * The caller environment is the execution environment of the
#'   function that called the current function.
#'
#' @inheritParams caller_frame
#'
#' @seealso [caller_frame()] and [current_frame()]
#' @export
#' @examples
#' # Let's create a function that returns its current environment and
#' # its caller environment:
#' fn <- function() list(current = current_env(), caller = caller_env())
#'
#' # The current environment is an unique execution environment
#' # created when `fn()` was called. The caller environment is the
#' # global env because that's where we called `fn()`.
#' fn()
#'
#' # Let's call `fn()` again but this time within a function:
#' g <- function() fn()
#'
#' # Now the caller environment is also an unique execution environment.
#' # This is the exec env created by R for our call to g():
#' g()
caller_env <- function(n = 1) {
  parent.frame(n + 1)
}
#' @rdname caller_env
#' @export
current_env <- function() {
  parent.frame()
}


#' Get the namespace of a package
#'
#' Namespaces are the environment where all the functions of a package
#' live. The parent environments of namespaces are the `imports`
#' environments, which contain all the functions imported from other
#' packages.
#'
#'
#' @section Life cycle:
#'
#' These functions are experimental and may not belong to the rlang
#' package. Expect API changes.
#'
#' @param pkg The name of a package. If `NULL`, the surrounding
#'   namespace is returned, or an error is issued if not called within
#'   a namespace. If a function, the enclosure of that function is
#'   checked.
#'
#' @seealso [pkg_env()]
#' @keywords internal
#' @export
ns_env <- function(pkg = NULL) {
  if (is_null(pkg)) {
    bottom <- topenv(caller_env())
    if (!isNamespace(bottom)) abort("not in a namespace")
    bottom
  } else if (is_function(pkg)) {
    env <- env_parent(pkg)
    if (isNamespace(env)) {
      env
    } else {
      NULL
    }
  } else {
    asNamespace(pkg)
  }
}
#' @rdname ns_env
#' @export
ns_imports_env <- function(pkg = NULL) {
  env_parent(ns_env(pkg))
}
#' @rdname ns_env
#' @export
ns_env_name <- function(pkg = NULL) {
  if (is_null(pkg)) {
    pkg <- with_env(caller_env(), ns_env())
  } else if (is_function(pkg)) {
    pkg <- get_env(pkg)
  }
  unname(getNamespaceName(pkg))
}

#' Is an object a namespace environment?
#'
#' @param x An object to test.
#' @export
is_namespace <- function(x) {
  isNamespace(x)
}

#' Is a package installed in the library?
#'
#' This checks that a package is installed with minimal side effects.
#' If installed, the package will be loaded but not attached.
#'
#' @param pkg The name of a package.
#' @return `TRUE` if the package is installed, `FALSE` otherwise.
#' @export
#' @examples
#' is_installed("utils")
#' is_installed("ggplot5")
is_installed <- function(pkg) {
  is_true(requireNamespace(pkg, quietly = TRUE))
}


env_type <- function(env) {
  if (is_reference(env, global_env())) {
    "global"
  } else if (is_reference(env, empty_env())) {
    "empty"
  } else if (is_reference(env, base_env())) {
    "base"
  } else if (is_frame_env(env)) {
    "frame"
  } else {
    "local"
  }
}
friendly_env_type <- function(type) {
  switch(type,
    global = "the global environment",
    empty = "the empty environment",
    base = "the base environment",
    frame = "a frame environment",
    local = "a local environment",
    abort("Internal error: unknown environment type")
  )
}

env_format <- function(env) {
  type <- env_type(env)

  if (type %in% c("frame", "local")) {
    addr <- sexp_address(get_env(env))
    type <- paste(type, addr)
  }

  type
}

#' Label of an environment
#'
#' @description
#'
#' Special environments like the global environment have their own
#' names. `env_name()` returns:
#'
#' * "global" for the global environment.
#'
#' * "empty" for the empty environment.
#'
#' * "base" for the base package environment (the last environment on
#'   the search path).
#'
#' * "namespace:pkg" if `env` is the namespace of the package "pkg".
#'
#' * The `name` attribute of `env` if it exists. This is how the
#'   [package environments][scoped_env] and the [imports
#'   environments][ns_imports_env] store their names. The name of package
#'   environments is typically "package:pkg".
#'
#' * The empty string `""` otherwise.
#'
#' `env_label()` is exactly like `env_name()` but returns the memory
#' address of anonymous environments as fallback.
#'
#' @param env An environment.
#'
#' @export
#' @examples
#' # Some environments have specific names:
#' env_name(global_env())
#' env_name(ns_env("rlang"))
#'
#' # Anonymous environments don't have names but are labelled by their
#' # address in memory:
#' env_name(env())
#' env_label(env())
env_name <- function(env) {
  if (is_reference(env, global_env())) {
    return("global")
  }

  if (is_reference(env, empty_env())) {
    return("empty")
  }

  nm <- environmentName(env)

  if (is_namespace(env)) {
    return(paste0("namespace:", nm))
  }

  nm
}
#' @rdname env_name
#' @export
env_label <- function(env) {
  nm <- env_name(env)

  if (nzchar(nm)) {
    nm
  } else {
    sexp_address(env)
  }
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
#'   [fancy bindings][env_bind_promise] (actives and promises) are
#'   indicated as such.
#'
#' * Locked bindings get a `[L]` tag
#'
#' @param env An environment, or object that can be converted to an
#'   environment by [get_env()].
#'
#' @export
env_print <- function(env) {
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

  cat_line(
    sprintf("<environment: %s>%s", env_label(env), locked),
    sprintf("  parent: %s", parent)
  )

  class <- attr(env, "class")
  if (is_character(class)) {
    class <- paste(class, collapse = ", ")
    cat_line(sprintf("  class: %s", class))
  }

  nms <- env_names(env)
  n <- length(nms)

  if (n) {
    cat_line("  bindings:")

    if (n > 20) {
      other <- nms[seq(21L, n)]
      nms <- nms[1:20]
    } else {
      other <- chr()
    }

    escaped_nms <- map_chr(syms(nms), deparse, backtick = TRUE)

    types <- env_binding_type_sum(env, nms)
    types <- paste0("   * ", escaped_nms, ": <", types, ">")

    locked <- env_binding_are_locked(env, nms)
    locked <- ifelse(locked, " [L]", "")
    types <- paste0(types, locked)

    cat_line(types)

    n_other <- length(other)
    if (n_other) {
      cat_line(sprintf("   * ... with %s more bindings", n_other))
    }
  }

  invisible(env)
}

new_environments <- function(envs, names) {
  stopifnot(is_list(envs))
  structure(envs,
    names = map_chr(envs, env_name),
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

  invalid <- nms_are_invalid(nms)
  if (all(invalid)) {
    return("")
  }

  ifelse(invalid, "  ", " $")
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
