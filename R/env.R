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
#' \code{env()} is a s3 generic. Methods are provided for functions,
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
#' \code{\link{env_empty}()} as parent.
#'
#' @param env An environment or an object with a S3 method for
#'   \code{env()}. If missing, the environment of the current
#'   evaluation frame is returned.
#' @param parent A parent environment. Can be an object with a S3
#'   method for \code{env()}.
#' @param dict A vector with unique names which defines bindings
#'   (pairs of name and value). See \code{\link{is_dict}()}.
#' @param n The number of generations to go through.
#' @seealso \link{env_scoped}, \code{\link{env_has}()},
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
#' env(fn) <- env_base()
#' env(fn)
#'
#'
#' # env_new() creates by default an environment whose parent is the
#' # current environment. Here we return a new environment that has
#' # the evaluation environment (or frame environment) of a function
#' # as parent:
#' fn <- function() {
#'   my_object <- "A"
#'   env_new()
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
#' # parent. When inheriting from the empty environment, the
#' # environment will have no object in scope at all:
#' env <- env_new(env_empty())
#' env_has(env, "lapply", inherit = TRUE)
#'
#' # The base package environment is often a good default choice for a
#' # parent environment because it contains all standard base
#' # functions. Also note that it will never inherit from other loaded
#' # package environments since R keeps the base package at the tail
#' # of the search path:
#' env <- env_new(env_base())
#' env_has(env, "lapply", inherit = TRUE)
#'
#' # Note that all other package environments inherit from env_base()
#' # as well:
#' env <- env_new(env_package("rlang"))
#' env_has(env, "env_has", inherit = TRUE)
#' env_has(env, "lapply", inherit = TRUE)
#'
#'
#' # Get the parent environment with env_parent():
#' env_parent(env_global())
#'
#' # Or the tail environment with env_tail():
#' env_tail(env_global())
#'
#'
#' # By default, env_parent() returns the parent environment of the
#' # current evaluation frame. If called at top-level (the global
#' # frame), the following two expressions are equivalent:
#' env_parent()
#' env_parent(env_global())
#'
#' # This default is more handy when called within a function. In this
#' # case, the enclosure environment of the function is returned
#' # (since it is the parent of the evaluation frame):
#' enclos_env <- env_new()
#' fn <- with_env(enclos_env, function() env_parent())
#' identical(enclos_env, fn())
env <- function(env = env_caller()) {
  UseMethod("env")
}

#' @rdname env
#' @export
env.function <- function(env = env_caller()) {
  environment(env)
}
#' @rdname env
#' @export
env.formula <- function(env = env_caller()) {
  attr(env, ".Environment")
}
#' @rdname env
#' @export
env.frame <- function(env = env_caller()) {
  env$env
}
#' @rdname env
#' @export
env.environment <- function(env = env_caller()) {
  env
}
#' @rdname env
#' @export
env.default <- function(env = env_caller()) {
  # Default argument env_caller() gets dispatched here:
  if (is_env(env)) {
    env
  } else {
    stop("No applicable method for 'env'", call. = FALSE)
  }
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
env_new <- function(parent = env_caller(), dict = list()) {
  env <- new.env(parent = parent)
  env_bind(env, dict)
}

#' @rdname env
#' @export
env_parent <- function(env = env_caller(), n = 1) {
  env_ <- rlang::env(env)

  while (n > 0) {
    if (identical(env_, env_empty())) {
      stop("Not enough environments in scope", call. = FALSE)
    }
    n <- n - 1
    env_ <- parent.env(env_)
  }

  env_
}

#' @rdname env
#' @export
env_tail <- function(env = env_caller()) {
  env_ <- rlang::env(env)
  next_env <- parent.env(env_)

  while(!identical(next_env, env_empty())) {
    env_ <- next_env
    next_env <- parent.env(next_env)
  }

  env_
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
#' env <- env_new()
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
#' dictionary (\code{env_bind()}). See \code{\link{is_dict}()} for
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
#' env(fn) <- env_new(env_base())
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
env_assign <- function(env = env_caller(), nm, x) {
  env_ <- rlang::env(env)
  base::assign(nm, x, envir = env_)
  env
}
#' @rdname env_assign
#' @export
env_bind <- function(env = env_caller(), dict = list()) {
  stopifnot(is_dict(dict))
  nms <- names(dict)

  env_ <- rlang::env(env)
  for (i in seq_along(dict)) {
    base::assign(nms[[i]], dict[[i]], envir = env_)
  }

  env
}
#' @rdname env_assign
#' @export
env_define <- function(env = env_caller(), ...) {
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
env_assign_lazily <- function(env = env_caller(), nm, expr, eval_env = NULL) {
  f <- as_quoted_f(substitute(expr), eval_env)
  env_assign_lazily_(env, nm, f)
}
#' @rdname env_assign_lazily
#' @export
env_assign_lazily_ <- function(env = env_caller(), nm, expr, eval_env = NULL) {
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
#' scope <- env_new(env_base(), list(a = 10))
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
env_bury <- function(env = env_caller(), dict = list()) {
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
#' parent <- env_new(env_empty(), list(foo = "a"))
#' env <- env_new(parent, list(foo = "b"))
#' env_unbind(env, "foo", inherit = TRUE)
#' env_has(env, "foo", inherit = TRUE)
env_unbind <- function(env = env_caller(), nms, inherit = FALSE) {
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

#' Is an object a dictionary?
#'
#' @param x An object to test.
#' @export
is_dict <- function(x) {
  if (!length(x)) {
    return(!is.null(x))
  }

  nms <- names(x)
  if (is.null(nms)) {
    return(FALSE)
  }

  is_bad_nm <- is.na(nms) | nms == "" | duplicated(nms)
  !any(is_bad_nm)
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
#' parent <- env_new(env_empty(), list(foo = "foo"))
#' env <- env_new(parent, list(bar = "bar"))
#'
#' # env does not own `foo` but sees it in its parent environment:
#' env_has(env, "foo")
#' env_has(env, "foo", inherit = TRUE)
env_has <- function(env = env_caller(), nms, inherit = FALSE) {
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
#' parent <- env_new(env_empty(), list(foo = "foo"))
#' env <- env_new(parent, list(bar = "bar"))
#'
#' # This throws an error because `foo` is not directly defined in env:
#' # env_get(env, "foo")
#'
#' # However `foo` can be fetched in the parent environment:
#' env_get(env, "foo", inherit = TRUE)
env_get <- function(env = env_caller(), nm, inherit = FALSE) {
  env_ <- rlang::env(env)
  get(nm, envir = env, inherits = inherit)
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
#' with \code{env_global()} and \code{env_base()} respectively.
#'
#' You can list all scoped environments with
#' \code{env_scoped_names()}. With \code{is_scoped()} you can check
#' whether a named environment is on the search
#' path. \code{env_package()} returns the scope environment of
#' packages if they are attached to the search path, and throws an
#' error otherwise.
#'
#' @param nm The name of an environment attached to the search
#'   path. Call \code{\link[base]{search}()} to see what is currently
#'   on the path.
#' @export
#' @examples
#' # List the names of scoped environments:
#' nms <- env_scoped_names()
#' nms
#'
#' # The global environment is always the first in the chain:
#' env_scoped(nms[[1]])
#'
#' # And the scoped environment of the base package is always the last:
#' env_scoped(nms[[length(nms)]])
#'
#' # These two environments have their own shortcuts:
#' env_global()
#' env_base()
#'
#' # Get the scoped environment of a package:
#' env_package("utils")
env_scoped <- function(nm) {
  if (!is_scoped(nm)) {
    stop(paste0(nm, " is not in scope"), call. = FALSE)
  }
  as.environment(nm)
}
#' @rdname env_scoped
#' @param pkg The name of a package.
#' @export
env_package <- function(pkg) {
  pkg_name <- paste0("package:", pkg)
  env_scoped(pkg_name)
}

#' @rdname env_scoped
#' @export
env_scoped_names <- function() {
  search()
}

#' @rdname env_scoped
#' @export
is_scoped <- function(nm) {
  if (!is_scalar_character(nm)) {
    stop("`nm` must be a string", call. = FALSE)
  }
  nm %in% env_scoped_names()
}

#' @rdname env_scoped
#' @export
env_base <- baseenv
#' @rdname env_scoped
#' @export
env_global <- globalenv


#' Get the namespace of a package.
#'
#' Namespaces are the environment where all the functions of a package
#' live. The parent environments of namespaces are the \code{imports}
#' environments, which contain all the functions imported from other
#' packages.
#' @param pkg The name of a package.
#' @seealso \code{\link{env_package}()}
#' @export
env_namespace <- function(pkg) {
  asNamespace(pkg)
}
#' @rdname env_namespace
#' @export
env_imports <- function(pkg) {
  env_parent(env_namespace(pkg))
}

#' Get the empty environment.
#'
#' The empty environment is the only that does not have a parent. It
#' is always used as the tail of a scope chain such as the search
#' path.
#'
#' @export
#' @examples
#' # Create environments with nothing in scope:
#' env_new(parent = env_empty())
env_empty <- emptyenv

#' Get the environment of the caller frame.
#'
#' This is a shortcut for \code{\link{call_frame}(2)$env}.
#' @param n The number of generation to go back. Note that contrarily
#'   to \code{\link{call_frame}()}, 1 represents the parent frame
#'   rather than the current frame.
#' @seealso \code{\link{call_frame}()}
#' @export
env_caller <- function(n = 1) {
  parent.frame(n + 1)
}


#' Evaluate an expression within a given environment.
#'
#' These functions evaluate \code{expr} within \code{env}. The
#' difference with \code{\link[base]{eval}()} is that \code{expr} is
#' wrapped in an artifical promise. This is a lighter evaluation
#' mechanism which also has some subtle implications (see details).
#'
#' When \code{env} is an evaluation environment of a stack frame (see
#' \code{\link{eval_stack}()}), using \code{with_env()} rather than
#' \code{eval()} has subtle implications. The base function
#' \code{eval()} creates a new evaluation context with \code{env} as
#' frame environment. This means that when \code{expr} is executed,
#' there are actually two contexts with the same evaluation
#' environment on the stack. Thus, any command that looks up frames on
#' the stack may find the frame set up by \code{eval()} rather than
#' the original frame of \code{env}. This affects functions like
#' \code{\link[base]{return}()}, \code{\link[base]{parent.frame}()},
#' \code{\link[base]{sys.calls}()}, etc.
#'
#' @param env An environment within which to evaluate \code{expr}. Can
#'   be an object with an \code{\link{env}()} method.
#' @param expr An expression to evaluate. The underscored version
#'   \code{with_env_()} takes either a quoted expression or a formula
#'   from which the right-hand side will be extracted.
#' @seealso \code{\link{env_assign_lazily}()}
#' @export
#' @examples
#' env <- env_new(env_package("rlang"))
#'
#' # This function is basically a shortcut for assigning a promise and
#' # evaluating it right away:
#' env_assign_lazily(env, "promise", cat("promise forced!\n"))
#' env$promise
#'
#' # Or equivalently:
#' with_env(env, cat("promise forced!\n"))
#'
#'
#' # It is handy to create formulas with a given environment:
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
with_env <- function(env, expr) {
  with_env_(env, substitute(expr))
}
with_env_ <- function(env, expr) {
  f <- as_quoted_f(expr)
  env_assign_lazily_(environment(), "promise", f_rhs(f), rlang::env(env))
  promise
}
globalVariables("promise")
