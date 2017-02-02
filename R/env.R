#' Get an environment.
#'
#' Environments are objects that create a scope for evaluation of R
#' code. Reification of scope is one of the most powerful feature of
#' the R language: it allows you to change what objects a function or
#' expression sees when it is evaluated. In R, scope is hierarchical:
#' each environment is defined with a parent environment. An
#' environment and its grandparents form together a linear
#' hierarchy. All objects within the grandparents are in scope unless
#' they are eclipsed by synonyms (other bindings with the same names)
#' in child environments.
#'
#' \code{env()} is a S3 generic. Methods are provided for functions,
#' formulas and frames. If called with a missing argument, the
#' environment of the current evaluation frame (see
#' \code{\link{eval_stack}()}) is returned. If you call \code{env()}
#' with an environment, it acts as the identity function and the
#' environment is simply returned (this helps simplifying code when
#' writing generic functions).
#'
#' \code{env_new()} creates a new environment. \code{env_parent()}
#' returns the parent environment of \code{env} if called with \code{n
#' = 1}, the grandparent with \code{n = 2}, etc. \code{env_tail()}
#' searches through the parents and returns the one which has
#' \code{\link{empty_env}()} as parent.
#'
#' @param env An environment or an object with a S3 method for
#'   \code{env()}. If missing, the environment of the current
#'   evaluation frame is returned.
#' @param parent A parent environment. Can be an object with a S3
#'   method for \code{as_env()}.
#' @param dict A vector with unique names which defines bindings
#'   (pairs of name and value). See \code{\link{is_dictionary}()}.
#' @param n The number of generations to go through.
#' @seealso \link{scoped_env}, \code{\link{env_has}()},
#'   \code{\link{env_assign}()}.
#' @export
#' @examples
#' # Get the environment of frame objects. If no argument is supplied,
#' # the current frame is used:
#' fn <- function() {
#'   list(
#'     env(call_frame()),
#'     env()
#'   )
#' }
#' fn()
#'
#' # Environment of closure functions:
#' env(fn)
#'
#' # There is also an assignment operator:
#' env(fn) <- base_env()
#' env(fn)
#'
#'
#' # env_new() creates by default an environment whose parent is the
#' # empty environment. Here we return a new environment that has
#' # the evaluation environment (or frame environment) of a function
#' # as parent:
#' fn <- function() {
#'   my_object <- "A"
#'   env_new(env())
#' }
#' frame_env <- fn()
#'
#' # The new environment is empty:
#' env_has(frame_env, "my_object")
#'
#' # But sees objects defined inside fn() by inheriting from its
#' # parent:
#' env_has(frame_env, "my_object", inherit = TRUE)
#'
#'
#' # Create a new environment with a particular scope by setting a
#' # parent. When inheriting from the empty environment (the default),
#' # the environment will have no object in scope at all:
#' env <- env_new()
#' env_has(env, "lapply", inherit = TRUE)
#'
#' # The base package environment is often a good default choice for a
#' # parent environment because it contains all standard base
#' # functions. Also note that it will never inherit from other loaded
#' # package environments since R keeps the base package at the tail
#' # of the search path:
#' env <- env_new(base_env())
#' env_has(env, "lapply", inherit = TRUE)
#'
#' # Note that all other package environments inherit from base_env()
#' # as well:
#' env <- env_new(pkg_env("rlang"))
#' env_has(env, "env_has", inherit = TRUE)
#' env_has(env, "lapply", inherit = TRUE)
#'
#'
#' # The parent argument of env_new() is passed to as_env() to provide
#' # handy shortcuts:
#' env <- env_new("rlang")
#' identical(env_parent(env), pkg_env("rlang"))
#'
#'
#' # Get the parent environment with env_parent():
#' env_parent(global_env())
#'
#' # Or the tail environment with env_tail():
#' env_tail(global_env())
#'
#'
#' # By default, env_parent() returns the parent environment of the
#' # current evaluation frame. If called at top-level (the global
#' # frame), the following two expressions are equivalent:
#' env_parent()
#' env_parent(global_env())
#'
#' # This default is more handy when called within a function. In this
#' # case, the enclosure environment of the function is returned
#' # (since it is the parent of the evaluation frame):
#' enclos_env <- env_new(pkg_env("rlang"))
#' fn <- with_env(enclos_env, function() env_parent())
#' identical(enclos_env, fn())
env <- function(env = caller_env()) {
  UseMethod("env")
}

#' @rdname env
#' @export
env.function <- function(env = caller_env()) {
  environment(env)
}
#' @rdname env
#' @export
env.formula <- function(env = caller_env()) {
  attr(env, ".Environment")
}
#' @rdname env
#' @export
env.frame <- function(env = caller_env()) {
  env$env
}
#' @rdname env
#' @export
env.environment <- function(env = caller_env()) {
  env
}
#' @rdname env
#' @export
env.default <- function(env = caller_env()) {
  # Default argument caller_env() gets dispatched here:
  if (is_env(env)) {
    env
  } else {
    stop("No applicable method for 'env'", call. = FALSE)
  }
}
#' @rdname env
#' @export
env.character <- function(env = caller_env()) {
  pkg_env(env)
}

#' Assignment operator for environments.
#' @param x An object with an \code{env_set} method.
#' @param value The new environment.
#' @export
`env<-` <- function(x, value) {
  env_set(x, value)
}

#' @rdname env
#' @export
env_new <- function(parent = NULL, dict = list()) {
  env <- new.env(parent = as_env(parent))
  env_bind(env, dict)
}

#' @rdname env
#' @export
env_parent <- function(env = caller_env(), n = 1) {
  env_ <- rlang::env(env)

  while (n > 0) {
    if (identical(env_, empty_env())) {
      return(env_)
    }
    n <- n - 1
    env_ <- parent.env(env_)
  }

  env_
}

#' @rdname env
#' @export
env_tail <- function(env = caller_env()) {
  env_ <- rlang::env(env)
  next_env <- parent.env(env_)

  while(!identical(next_env, empty_env())) {
    env_ <- next_env
    next_env <- parent.env(next_env)
  }

  env_
}


#' Coerce to an environment.
#'
#' This is a S3 generic. The default method coerces named vectors
#' (including lists) to an environment. It first checks that \code{x}
#' is a dictionary (see \code{\link{is_dictionary}()}). The method for
#' unnamed strings returns the corresponding package environment (see
#' \code{\link{pkg_env}()}).
#'
#' If \code{x} is an environment and \code{parent} is not \code{NULL},
#' the environment is duplicated before being set a new parent. The
#' return value is therefore a different environment than \code{x}.
#'
#' @param x An object to coerce.
#' @param parent A parent environment, \code{\link{empty_env}()} by
#'   default. Can be ignored with a warning for methods where it does
#'   not make sense to change the parent.
#' @export
#' @examples
#' # Coerce a named vector to an environment:
#' env <- as_env(mtcars)
#'
#' # By default it gets the empty environment as parent:
#' identical(env_parent(env), empty_env())
#'
#'
#' # With strings it is a handy shortcut for pkg_env():
#' as_env("base")
#' as_env("rlang")
#'
#' # With NULL it returns the empty environment:
#' as_env(NULL)
as_env <- function(x, parent = NULL) {
  UseMethod("as_env")
}

#' @rdname as_env
#' @export
as_env.NULL <- function(x, parent = NULL) {
  if (!is_null(parent)) {
    warning("`parent` ignored for empty environment", call. = FALSE)
  }
  empty_env()
}

#' @rdname as_env
#' @export
as_env.environment <- function(x, parent = NULL) {
  if (!is_null(parent)) {
    x <- env_clone(x, parent = parent)
  }
  x
}

#' @rdname as_env
#' @export
as_env.character <- function(x, parent = NULL) {
  if (length(x) > 1 || is_named(x)) {
    return(as_env.default(x, parent))
  }
  if (!is_null(parent)) {
    warning("`parent` ignored for named environments", call. = FALSE)
  }
  pkg_env(x)
}

#' @rdname as_env
#' @export
as_env.default <- function(x, parent = NULL) {
  stopifnot(is_dictionary(x))
  if (is_atomic(x)) {
    x <- as.list(x)
  }
  list2env(x, parent = parent %||% empty_env())
}


#' Set an environment.
#'
#' \code{env_set()} does not work by side effect. The input is copied
#' before being assigned an environment, and left unchanged. However,
#' \code{env_set_parent()} operates on the inner environment and does
#' have a side effect.
#'
#' @param env An environment or an object with a S3 method for
#'   \code{env_set()}.
#' @param new_env An environment to replace \code{env} with. Can be an
#'   object with an S method for \code{env()}.
#' @export
#' @examples
#' # Create a function with a given environment:
#' env <- env_new(base_env())
#' fn <- with_env(env, function() NULL)
#' identical(env(fn), env)
#'
#' # env_set() does not work by side effect. Setting a new environment
#' # for fn has no effect on the original function:
#' other_env <- env_new()
#' env_set(fn, other_env)
#' identical(env(fn), other_env)
#'
#' # env_set() returns a new function with a different environment, so
#' # you need to assign the returned function to the `fn` name:
#' fn <- env_set(fn, other_env)
#' identical(env(fn), other_env)
env_set <- function(env, new_env) {
  UseMethod("env_set")
}
#' @rdname env_set
#' @export
env_set.function <- function(env, new_env) {
  environment(env) <- rlang::env(new_env)
  env
}
#' @rdname env_set
#' @export
env_set.formula <- env_set.function
#' @rdname env_set
#' @export
env_set.environment <- function(env, new_env) {
  rlang::env(new_env)
}

#' @rdname env_set
#' @export
env_set_parent <- function(env, new_env) {
  env_ <- rlang::env(env)
  parent.env(env_) <- rlang::env(new_env)
  env
}


#' Assign objects to an environment.
#'
#' These functions create bindings in the specified environment. The
#' bindings are supplied as pairs of names and values, either directly
#' (\code{env_assign()}), in dots (\code{env_define()}), or from a
#' dictionary (\code{env_bind()}). See \code{\link{is_dictionary}()} for
#' the definition of a dictionary.
#'
#' These functions operate by side effect. For example, if you assign
#' bindings to a closure function, the environment of the function is
#' modified in place.
#'
#' @inheritParams env
#' @param nm The name of the binding.
#' @param x The value of the binding.
#' @param ... Pairs of unique names and R objects used to define new
#'   bindings.
#' @return The input object \code{env}, with its associated
#'   environment modified in place.
#' @export
#' @examples
#' # Create a function that uses undefined bindings:
#' fn <- function() list(a, b, c, d, e)
#' env(fn) <- env_new(base_env())
#'
#' # This would throw a scoping error if run:
#' # fn()
#'
#' dict <- stats::setNames(letters, letters)
#' env_bind(fn, dict)
#'
#' # fn() now sees the objects
#' fn()
#'
#' # Redefine new bindings:
#' fn <- env_assign(fn, "a", "1")
#' fn <- env_define(fn, b = "2", c = "3")
#' fn()
env_assign <- function(env = caller_env(), nm, x) {
  env_ <- rlang::env(env)
  base::assign(nm, x, envir = env_)
  env
}
#' @rdname env_assign
#' @export
env_bind <- function(env = caller_env(), dict = list()) {
  stopifnot(is_dictionary(dict))
  nms <- names(dict)

  env_ <- rlang::env(env)
  for (i in seq_along(dict)) {
    base::assign(nms[[i]], dict[[i]], envir = env_)
  }

  env
}
#' @rdname env_assign
#' @export
env_define <- function(env = caller_env(), ...) {
  env_bind(env, list(...))
}

#' Assign a promise to an environment.
#'
#' These functions let you create a promise in an environment. Such
#' promises behave just like lazily evaluated arguments. They are
#' evaluated whenever they are touched by code, but not when they are
#' passed as arguments.
#'
#' @inheritParams env_assign
#' @param expr An expression to capture for
#'   \code{env_assign_lazily()}, or a captured expression (either
#'   quoted or a formula) for the standard evaluation version
#'   \code{env_assign_lazily_()}. This expression is used to create a
#'   promise in \code{env}.
#' @param eval_env The environment where the promise will be evaluated
#'   when the promise gets forced. If \code{expr} is a formula, its
#'   environment is used instead. If not a formula and \code{eval_env}
#'   is not supplied, the promise is evaluated in the environment
#'   where \code{env_assign_lazily()} (or the underscore version) was
#'   called.
#' @seealso \code{\link{env_assign}()}
#' @export
#' @examples
#' env <- env_new()
#' env_assign_lazily(env, "name", cat("forced!\n"))
#' env$name
#'
#' # Use the standard evaluation version with quoted expressions:
#' f <- ~message("forced!")
#' env_assign_lazily_(env, "name2", f)
#' env$name2
env_assign_lazily <- function(env = caller_env(), nm, expr, eval_env = NULL) {
  f <- as_quoted_f(substitute(expr), eval_env)
  env_assign_lazily_(env, nm, f)
}
#' @rdname env_assign_lazily
#' @export
env_assign_lazily_ <- function(env = caller_env(), nm, expr, eval_env = NULL) {
  f <- as_quoted_f(expr, eval_env)

  args <- list(
    x = nm,
    value = f_rhs(f),
    eval.env = f_env(f),
    assign.env = rlang::env(env)
  )
  do.call("delayedAssign", args)
}

#' Bury bindings and define objects in new scope.
#'
#' \code{env_bury()} is like \code{env_bind()} but it creates the
#' bindings in a new child environment. Note that this function does
#' not modify its inputs.
#'
#' @inheritParams env_bind
#' @return An object associated with the new environment.
#' @export
#' @examples
#' scope <- env_new(base_env(), list(a = 10))
#' fn <- function() a
#' env(fn) <- scope
#'
#' # fn() sees a = 10:
#' fn()
#'
#' # env_bury() will bury the current scope of fn() behind a new
#' # environment:
#' fn <- env_bury(fn, list(a = 1000))
#' fn()
env_bury <- function(env = caller_env(), dict = list()) {
  env_ <- rlang::env(env)
  env_ <- new.env(parent = env_)

  env_bind(env_, dict)
  env_set(env, env_)
}

#' Remove bindings from an environment.
#'
#' \code{env_unbind()} is the complement of
#' \code{\link{env_bind}()}. Like \code{env_has()}, it ignores the
#' parent environments of \code{env} by default. Set \code{inherit} to
#' \code{TRUE} to track down bindings in parent environments.
#'
#' @inheritParams env_assign
#' @param nms A character vector containing the names of the bindings
#'   to remove.
#' @param inherit Whether to look for bindings in the parent
#'   environments.
#' @return The input object \code{env}, with its associated
#'   environment modified in place.
#' @export
#' @examples
#' dict <- stats::setNames(letters, letters)
#' env_bind(environment(), dict)
#' env_has(environment(), letters)
#'
#' # env_unbind() removes bindings:
#' env_unbind(environment(), letters)
#' env_has(environment(), letters)
#'
#' # With inherit = TRUE, it removes bindings in parent environments
#' # as well:
#' parent <- env_new(empty_env(), list(foo = "a"))
#' env <- env_new(parent, list(foo = "b"))
#' env_unbind(env, "foo", inherit = TRUE)
#' env_has(env, "foo", inherit = TRUE)
env_unbind <- function(env = caller_env(), nms, inherit = FALSE) {
  env_ <- rlang::env(env)
  if (inherit) {
    while(any(env_has(env_, nms, inherit = TRUE))) {
      rm(list = nms, envir = env, inherits = TRUE)
    }
  } else {
    rm(list = nms, envir = env)
  }
  env
}

#' Does an environment have or see bindings?
#'
#' \code{env_has()} is a vectorised predicate that queries whether an
#' environment owns bindings personally (with \code{inherit} set to
#' \code{FALSE}, the default), or sees them in its own environment or
#' in any of its parents (with \code{inherit = TRUE}).
#'
#' @inheritParams env_unbind
#' @return A logical vector as long as \code{nms}.
#' @export
#' @examples
#' parent <- env_new(empty_env(), list(foo = "foo"))
#' env <- env_new(parent, list(bar = "bar"))
#'
#' # env does not own `foo` but sees it in its parent environment:
#' env_has(env, "foo")
#' env_has(env, "foo", inherit = TRUE)
env_has <- function(env = caller_env(), nms, inherit = FALSE) {
  env_ <- rlang::env(env)
  vapply_lgl(nms, exists, envir = env_, inherits = inherit)
}

#' Get an object from an environment.
#'
#' \code{env_get()} extracts an object from an enviroment \code{env}.
#' By default, it does not look in the parent environments.
#'
#' @inheritParams env
#' @inheritParams env_has
#' @param nm The name of a binding.
#' @return An object if it exists. Otherwise, throws an error.
#' @export
#' @examples
#' parent <- env_new(empty_env(), list(foo = "foo"))
#' env <- env_new(parent, list(bar = "bar"))
#'
#' # This throws an error because `foo` is not directly defined in env:
#' # env_get(env, "foo")
#'
#' # However `foo` can be fetched in the parent environment:
#' env_get(env, "foo", inherit = TRUE)
env_get <- function(env = caller_env(), nm, inherit = FALSE) {
  env_ <- rlang::env(env)
  get(nm, envir = env, inherits = inherit)
}

#' Clone an environment.
#'
#' This creates a new environment containing exactly the same objects,
#' optionally with a new parent.
#'
#' @param x An environment to clone.
#' @param parent The parent of the cloned environment.
#' @export
#' @examples
#' env <- env_new(dict = mtcars)
#' clone <- env_clone(env)
#' identical(env$cyl, clone$cyl)
env_clone <- function(x, parent = env_parent(x)) {
  list2env(as.list(x, all.names = TRUE), parent = parent)
}


#' Scope environments
#'
#' Scope environments are named environments which form a
#' parent-child hierarchy called the search path. They define what
#' objects you can see (are in scope) from your workspace. They
#' typically are package environments, i.e. special environments
#' containing all exported functions from a package (and whose parent
#' environment is the package namespace, which also contains
#' unexported functions). Package environments are attached to the
#' search path with \code{\link[base]{library}()}. Note however that
#' any environment can be attached to the search path, for example
#' with the unrecommended \code{\link[base]{attach}()} base function
#' which transforms vectors to scoped environments.
#'
#' Scope environments form a chain with newly attached environments as
#' the childs of earlier ones. However, the global environment, where
#' everything you define at top-level ends up, is pinned as the head
#' of the linked list. Likewise, the base package environment is
#' always the tail of the chain. You can obtain those environments
#' with \code{global_env()} and \code{base_env()} respectively. The
#' global environment is always the environment of the very first
#' evaluation frame on the stack, see \code{\link{global_frame}()} and
#' \code{\link{eval_stack}()}.
#'
#' You can list all scoped environments with
#' \code{scoped_names()}. With \code{is_scoped()} you can check
#' whether a named environment is on the search
#' path. \code{pkg_env()} returns the scope environment of
#' packages if they are attached to the search path, and throws an
#' error otherwise.
#'
#' @param nm The name of an environment attached to the search
#'   path. Call \code{\link[base]{search}()} to see what is currently
#'   on the path.
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
#' # pkg_label() to create that name:
#' pkg_label("rlang")
#' scoped_env(pkg_label("rlang"))
#'
#' # Alternatively, get the scoped environment of a package with
#' # pkg_env():
#' pkg_env("utils")
scoped_env <- function(nm) {
  if (!is_scoped(nm)) {
    stop(paste0(nm, " is not in scope"), call. = FALSE)
  }
  as.environment(nm)
}
#' @rdname scoped_env
#' @param pkg The name of a package.
#' @export
pkg_env <- function(pkg) {
  pkg_name <- pkg_label(pkg)
  scoped_env(pkg_name)
}
#' @rdname scoped_env
#' @export
pkg_label <- function(pkg) {
  paste0("package:", pkg)
}

#' @rdname scoped_env
#' @export
scoped_names <- function() {
  search()
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

#' Get the empty environment.
#'
#' The empty environment is the only one that does not have a
#' parent. It is always used as the tail of a scope chain such as the
#' search path (see \code{\link{scoped_names}()}).
#'
#' @export
#' @examples
#' # Create environments with nothing in scope (the default):
#' env_new(parent = empty_env())
empty_env <- emptyenv

#' Get the namespace of a package.
#'
#' Namespaces are the environment where all the functions of a package
#' live. The parent environments of namespaces are the \code{imports}
#' environments, which contain all the functions imported from other
#' packages.
#' @param pkg The name of a package. If \code{NULL}, the surrounding
#'   namespace is returned, or an error is issued if not called within
#'   a namespace.
#' @seealso \code{\link{pkg_env}()}
#' @export
ns_env <- function(pkg = NULL) {
  if (!is_null(pkg)) {
    return(asNamespace(pkg))
  }

  bottom <- topenv(caller_env())
  if (!isNamespace(bottom)) {
    stop("not in a namespace", call. = FALSE)
  }
  bottom
}
#' @rdname ns_env
#' @export
ns_imports_env <- function(pkg = NULL) {
  env_parent(ns_env(pkg))
}


#' Evaluate an expression within a given environment.
#'
#' This function evaluate \code{expr} within \code{env}. It uses
#' \code{\link{expr_eval}()} which features a lighter evaluation
#' mechanism than base R \code{\link[base]{eval}()}, and which also
#' has some subtle implications when evaluting stack sensitive
#' functions (see help for \code{\link{expr_eval}()}).
#'
#' @inheritParams expr_eval
#' @param env An environment within which to evaluate \code{expr}. Can
#'   be an object with an \code{\link{env}()} method.
#' @export
#' @examples
#' # with_env() is handy to create formulas with a given environment:
#' env <- env_new("rlang")
#' f <- with_env(env, ~new_formula())
#' identical(f_env(f), env)
#'
#' # Or functions with a given enclosure:
#' fn <- with_env(env, function() NULL)
#' identical(env(fn), env)
#'
#'
#' # Unlike eval() it doesn't create duplicates on the evaluation
#' # stack. You can thus use it e.g. to create non-local returns:
#' fn <- function() {
#'   g(env())
#'   "normal return"
#' }
#' g <- function(env) {
#'   with_env(env, return("early return"))
#' }
#' fn()
#'
#'
#' # Since env is passed to env(), it can be any object with an env()
#' # method. For strings, the pkg_env() is returned:
#' with_env("base", ~mtcars)
with_env <- function(env, expr) {
  .Call(rlang_eval, substitute(expr), rlang::env(env))
}
