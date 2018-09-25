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
#' @rdname x An environment or a search name.
#' @export
is_attached <- function(x) {
  if (is_string(x)) {
    return(x %in% search())
  }
  if (!is_environment(x)) {
    abort("`x` must be an environment or a name")
  }

  env <- global_env()
  while (!is_reference(env, empty_env())) {
    if (is_reference(x, env)) {
      return(TRUE)
    }
    env <- env_parent(env)
  }

  FALSE
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
