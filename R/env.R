#' Get or set an environment.
#'
#' Environments are objects that create a scope for evaluation of R
#' code. The reification of scope is one of the most powerful feature
#' of the R language. It allows you to change what a function or
#' expression sees when it is evaluated. Each environment is defined
#' with a parent environment. An environment and its grand-parents
#' form together a linear hierarchy: the objects within the
#' grand-parents are in scope unless they are eclipsed by bindings
#' with the same names in child environments.
#'
#' \code{env()} and \code{env_set()} are s3 generics. Methods are
#' provided for functions, formulas and frames. They also work with
#' environments as this can simplify code in some situations.
#' \code{env_set()} does not work by side effect. The input is copied
#' before being assigned an environment, and left unchanged. However,
#' \code{env_set_next()} operates the inner environment which has a
#' side effect.
#'
#' \code{env_new()} creates a new environment. \code{env_next()}
#' returns the parent environment of \code{env} if called with \code{n
#' = 1}, the grand-parent with \code{n = 2}, etc. \code{env_tail()}
#' searches through the parents for the one which hase
#' \code{\link{env_empty}()} as parent.
#'
#' @param env An environment or an object associated with an
#'   environment (i.e., an object with an \code{env()} method
#'   defined).
#' @param new_env An environment to replace \code{env}.
#' @param parent A parent environment.
#' @param dict A vector with unique names which defines bindings
#'   (pairs of name and value). See \code{\link{is_dict}()}.
#' @param n The number of generations to go through.
#' @seealso \link{env_scope}, \code{\link{env_has}()},
#'   \code{\link{env_assign}()}.
#' @export
#' @examples
#' # Get the environment of frame objects:
#' fn <- function() {
#'   list(
#'     env(call_frame()),
#'     environment()
#'   )
#' }
#' fn()
#'
#' # Environment of closure functions:
#' env(fn)
#'
#' # There is also an assignment operator:
#' env(fn) <- baseenv()
#' env(fn)
#'
#' # Or equivalently:
#' fn <- env_set(fn, globalenv())
#' env(fn)
#'
#'
#' # Create a new environment with a given scope by setting a parent:
#' env <- env_new(parent = baseenv())
#' env_sees(env, "lapply")
#'
#' # Use the empty environment if the environment should not have
#' # anything in scope:
#' env <- env_new(parent = env_empty())
#' env_sees(env, "lapply")
#'
#'
#' # Get the parent environment with env_next():
#' env_next(env_scope_global())
#'
#' # Or the tail environment with env_tail():
#' env_tail(env_scope_global())
env <- function(env) {
  UseMethod("env")
}

#' @rdname env
#' @export
env.function <- function(env) {
  environment(env)
}
#' @rdname env
#' @export
env.formula <- function(env) {
  attr(env, ".Environment")
}
#' @rdname env
#' @export
env.frame <- function(env) {
  env$env
}
#' @rdname env
#' @export
env.environment <- function(env) {
  env
}
#' @rdname env
#' @export
env.default <- function(env) {
  if (missing(env)) {
    arg_info(env)$caller_frame$env
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
env_set <- function(env, new_env) {
  UseMethod("env_set")
}
#' @rdname env
#' @export
env_set.function <- function(env, new_env) {
  environment(env) <- new_env
  env
}
#' @rdname env
#' @export
env_set.formula <- env_set.function
#' @rdname env
#' @export
env_set.environment <- function(env, new_env) {
  new_env
}

#' @rdname env
#' @export
env_set_next <- function(env, parent) {
  env_ <- rlang::env(env)
  parent.env(env_) <- parent
  env
}

#' @rdname env
#' @export
env_new <- function(parent = env_caller(), dict = list()) {
  env <- new.env(parent = parent)
  env_bind(env, dict)
}

#' @rdname env
#' @export
env_next <- function(env, n = 1) {
  env_ <- rlang::env(env)

  while (n > 0) {
    if (identical(env_, env_empty())) {
      stop("not enough environments in scope", call. = FALSE)
    }
    n <- n - 1
    env_ <- parent.env(env_)
  }

  env_
}

#' @rdname env
#' @export
env_tail <- function(env) {
  env_ <- rlang::env(env)
  next_env <- parent.env(env_)

  while(!identical(next_env, env_empty())) {
    env_ <- next_env
    next_env <- parent.env(next_env)
  }

  env_
}


#' Assign objects to an environment.
#'
#' These functions create bindings in the specified environment. The
#' bindings are supplied as pairs of names and values, either directly
#' (\code{env_assign()}), in dots (\code{env_define()}), or from a
#' dictionary (\code{env_bind()}). See \code{\link{is_dict}()} for
#' the definition of a dictionary.
#'
#' Functions prefixed with \code{env_} operate by side effect. If you
#' assign bindings to a closure function, the environment of the
#' function is modified in place.
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
#' env(fn) <- env_new(env_scope_base())
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
env_assign <- function(env, nm, x) {
  env_ <- rlang::env(env)
  base::assign(nm, x, envir = env_)
  env
}
#' @rdname env_assign
#' @export
env_bind <- function(env, dict = list()) {
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
env_define <- function(env, ...) {
  env_bind(env, list(...))
}

#' Bury bindings and define objects in new scope.
#'
#' \code{env_bury()} is like \code{env_bind()} but it creates the
#' bindings in a new child environment. Note that this function does
#' not work by side effect.
#'
#' @inheritParams env_bind
#' @return An object associated with the new environment.
#' @export
#' @examples
#' scope <- env_new(env_scope_base(), list(a = 10))
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
env_bury <- function(env, dict = list()) {
  env_ <- rlang::env(env)
  env_ <- new.env(parent = env_)

  env_bind(env_, dict)
  env_set(env, env_)
}

#' Remove bindings from an environment.
#'
#' \code{env_unbind()} is the complement of
#' \code{\link{env_bind}()}. Like \code{env_has()}, it ignores the
#' parent environments of \code{env}. On the other hand,
#' \code{env_eliminate()} is akin to \code{env_sees()} and will track
#' down bindings in parent environments.
#'
#' @inheritParams env_assign
#' @param nms A character vector containing the names of the bindings
#'   to remove.
#' @return The input object \code{env}, with its associated
#'   environment modified in place.
#' @export
#' @examples
#' dict <- stats::setNames(letters, letters)
#' env_bind(environment(), dict)
#' env_has(environment(), letters)
#'
#' # env_unbind() will remove bindings:
#' env_unbind(environment(), letters)
#' env_has(environment(), letters)
#'
#' # env_eliminate() removes all bindings:
#' parent <- env_new(env_empty(), list(foo = "a"))
#' env <- env_new(parent, list(foo = "b"))
#' env_eliminate(env, "foo")
#' env_sees(env, "foo")
env_unbind <- function(env, nms) {
  env_ <- rlang::env(env)
  rm(list = nms, envir = env)
  env
}
#' @rdname env_unbind
#' @export
env_eliminate <- function(env, nms) {
  env_ <- rlang::env(env)
  while(any(env_sees(env_, nms))) {
    rm(list = nms, envir = env, inherits = TRUE)
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
#' These functions are vectorised predicates. \code{env_has()} queries
#' whether an environment owns bindings personally, while
#' \code{env_sees()} will also return \code{TRUE} for the bindings
#' that are owned by one of the parent environments.
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
#' env_sees(env, "foo")
env_has <- function(env, nms) {
  env_ <- rlang::env(env)
  vapply_lgl(nms, exists, envir = env_, inherits = FALSE)
}
#' @rdname env_has
#' @export
env_sees <- function(env, nms) {
  env_ <- rlang::env(env)
  vapply_lgl(nms, exists, envir = env_, inherits = TRUE)
}

#' Get an object from a scope.
#'
#' \code{env_grab()} looks up an object binded in \code{env} without
#' looking in its parent. \code{env_fetch()} will return objects from
#' the parents as well. In that respect, \code{env_grab()} is like
#' \code{env_has()} and \code{env_fetch()} is like \code{env_sees()}.
#'
#' @inheritParams env
#' @param nm The name of a binding.
#' @return An object if it exists. Otherwise, throws an error.
#' @examples
#' parent <- env_new(env_empty(), list(foo = "foo"))
#' env <- env_new(parent, list(bar = "bar"))
#'
#' # This throws an error because `foo` is not directly defined in env:
#' # env_grab(env, "foo")
#'
#' # However `foo` can be fetched in the parent environment:
#' env_fetch(env, "foo")
env_grab <- function(env, nm) {
  env_ <- rlang::env(env)
  get(nm, envir = env, inherits = FALSE)
}
#' @rdname env_grab
#' @export
env_fetch <- function(env, nm) {
  env_ <- rlang::env(env)
  get(nm, envir = env, inherits = TRUE)
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
#' with \code{env_scope_global()} and \code{env_scope_base()}
#' respectively.
#'
#' You can list all scoped environments with
#' \code{env_scope_names()}. With \code{is_scoped()} you can check
#' whether a named environment is on the search
#' path. \code{env_scope_package()} returns the scope environment of
#' packages if they are attached to the search path, and throws an
#' error otherwise.
#'
#' @param env_nm The name of an environment attached to the search
#'   path. Call \code{\link[base]{search}()} to see what is currently
#'   on the path.
#' @export
#' @examples
#' # List the names of scoped environments:
#' nms <- env_scope_names()
#' nms
#'
#' # The global environment is always the first in the chain:
#' env_scope(nms[[1]])
#'
#' # And the scope environment of the base package is always the last:
#' env_scope(nms[[length(nms)]])
#'
#' # These environments have their own shortcuts:
#' env_scope_global()
#' env_scope_base()
#'
#' # Get the scope environment of a package:
#' env_scope_package("utils")
env_scope <- function(env_nm) {
  if (!is_scoped(env_nm)) {
    stop(paste0(env_nm, " is not in scope"), call. = FALSE)
  }
  as.environment(env_nm)
}
#' @rdname env_scope
#' @param pkg The name of a package.
#' @export
env_scope_package <- function(pkg) {
  pkg_name <- paste0("package:", pkg)
  env_scope(pkg_name)
}

#' @rdname env_scope
#' @export
env_scope_names <- function() {
  search()
}

#' @rdname env_scope
#' @export
is_scoped <- function(env_nm) {
  if (!is_scalar_character(env_nm)) {
    stop("`env_nm` must be a string", call. = FALSE)
  }
  env_nm %in% env_scope_names()
}

#' @rdname env_scope
#' @export
env_scope_base <- baseenv
#' @rdname env_scope
#' @export
env_scope_global <- globalenv


#' Get the namespace of a package.
#'
#' Namespaces are the environment where all the functions of a package
#' live. The parent environments of namespaces are the \code{imports}
#' environments, which contain all the functions imported from other
#' packages.
#' @param pkg The name of a package.
#' @seealso \code{\link{env_scope_package}()}
#' @export
env_namespace <- function(pkg) {
  asNamespace(pkg)
}
#' @rdname env_namespace
#' @export
env_imports <- function(pkg) {
  env_next(env_namespace(pkg))
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
#' This is a shortcut for \code{\link{call_frame}()$env}.
#' @seealso \code{\link{call_frame}()}
#' @export
env_caller <- function() {
  parent.frame(2)
}
