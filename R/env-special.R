#' Search path environments
#'
#' @description
#'
#' The search path is a chain of environments containing exported
#' functions of attached packages.
#'
#' The API includes:
#'
#' - [base::search()] to get the names of environments attached to the
#'   search path.
#'
#' - `search_envs()` returns the environments on the search path as a
#'   list.
#'
#' - `pkg_env_name()` takes a bare package name and prefixes it with
#'   `"package:"`. Attached package environments have search names of
#'   the form `package:name`.
#'
#' - `pkg_env()` takes a bare package name and returns the scoped
#'   environment of packages if they are attached to the search path,
#'   and throws an error otherwise. It is a shortcut for
#'   `search_env(pkg_env_name("pkgname"))`.
#'
#' - `global_env()` and `base_env()` (simple aliases for [globalenv()]
#'   and [baseenv()]). These are respectively the first and last
#'   environments of the search path.
#'
#' - `is_attached()` returns `TRUE` when its argument (a search name
#'   or a package environment) is attached to the search path.
#'
#'
#' @section The search path:
#'
#' This chain of environments determines what objects are visible from
#' the global workspace. It contains the following elements:
#'
#' - The chain always starts with `global_env()` and finishes with
#'   `base_env()` which inherits from the terminal environment
#'   `empty_env()`.
#'
#' - Each [base::library()] call attaches a new package environment to
#'   the search path. Attached packages are associated with a [search
#'   name][env_name].
#'
#' - In addition, any list, data frame, or environment can be attached
#'   to the search path with [base::attach()].
#'
#'
#' @param name The name of an environment attached to the search
#'   path. Call [base::search()] to get the names of environments
#'   currently attached to the search path. Note that the search name
#'   of a package environment is prefixed with `"package:"`.
#'
#' @keywords internal
#' @export
#' @examples
#' # List the search names of environments attached to the search path:
#' search()
#'
#' # Get the corresponding environments:
#' search_envs()
#'
#' # The global environment and the base package are always first and
#' # last in the chain, respectively:
#' envs <- search_envs()
#' envs[[1]]
#' envs[[length(envs)]]
#'
#' # These two environments have their own shortcuts:
#' global_env()
#' base_env()
#'
#' # Packages appear in the search path with a special name. Use
#' # pkg_env_name() to create that name:
#' pkg_env_name("rlang")
#' search_env(pkg_env_name("rlang"))
#'
#' # Alternatively, get the scoped environment of a package with
#' # pkg_env():
#' pkg_env("utils")
search_envs <- function() {
  env_parents(env(.GlobalEnv), last = base_env())
}
#' @rdname search_envs
#' @export
search_env <- function(name) {
  check_name(name)

  if (!is_attached(name)) {
    abort(paste_line(
      sprintf("`%s` is not attached.", name),
      "Do you need to prefix it with \"package:\"?"
    ))
  }
  as.environment(name)
}
#' @rdname search_envs
#' @param pkg The name of a package.
#' @export
pkg_env <- function(pkg) {
  search_env(pkg_env_name(pkg))
}
#' @rdname search_envs
#' @export
pkg_env_name <- function(pkg) {
  paste0("package:", pkg)
}
#' @rdname search_envs
#' @param x An environment or a search name.
#' @export
is_attached <- function(x) {
  if (is_string(x)) {
    return(x %in% search())
  }
  check_environment(x, what = "an environment or a name")

  env <- global_env()
  while (!is_reference(env, empty_env())) {
    if (is_reference(x, env)) {
      return(TRUE)
    }
    env <- env_parent(env)
  }

  FALSE
}

#' @rdname search_envs
#' @export
base_env <- baseenv
#' @rdname search_envs
#' @export
global_env <- globalenv

#' Get the empty environment
#'
#' The empty environment is the only one that does not have a parent.
#' It is always used as the tail of an environment chain such as the
#' search path (see [search_envs()]).
#'
#' @export
#' @examples
#' # Create environments with nothing in scope:
#' child_env(empty_env())
empty_env <- emptyenv

#' Get the namespace of a package
#'
#' Namespaces are the environment where all the functions of a package
#' live. The parent environments of namespaces are the `imports`
#' environments, which contain all the functions imported from other
#' packages.
#'
#' @param x
#'   * For `ns_env()`, the name of a package or an environment as a
#'     string.
#'   * An environment (the current environment by default).
#'   * A function.
#'
#'   In the latter two cases, the environment ancestry is searched for
#'   a namespace with [base::topenv()]. If the environment doesn't
#'   inherit from a namespace, this is an error.
#'
#' @seealso [pkg_env()]
#' @keywords internal
#' @export
ns_env <- function(x = caller_env()) {
  env <- switch(typeof(x),
    builtin = ,
    special = ns_env("base"),
    closure = topenv(fn_env(x)),
    environment = topenv(x),
    character = if (is_string(x)) asNamespace(x)
  )

  if (!is_namespace(env)) {
    stop_input_type(x, "a package name or a function inheriting from a namespace")
  }

  env
}
#' @rdname ns_env
#' @export
ns_imports_env <- function(x = caller_env()) {
  env_parent(ns_env(x))
}
#' @rdname ns_env
#' @param env A namespace environment.
#' @export
ns_env_name <- function(x = caller_env()) {
  env <- switch(typeof(x),
    environment = ,
    builtin = ,
    special = ,
    closure = ns_env(x),
    stop_input_type(x, "an environment or a function inheriting from a namespace")
  )
  unname(getNamespaceName(env))
}

ns_exports <- function(ns) getNamespaceExports(ns)
ns_imports <- function(ns) getNamespaceImports(ns)

ns_exports_has <- function(ns, name) {
  if (is_string(ns)) {
    if (!is_installed(ns)) {
      return(FALSE)
    }
    ns <- ns_env(ns)
  }
  if (is_reference(ns, ns_env("base"))) {
    exports <- base_pkg_env
  } else {
    exports <- ns$.__NAMESPACE__.$exports
  }
  !is_null(exports) && exists(name, envir = exports, inherits = FALSE)
}

ns_import_from <- function(ns, names, env = caller_env()) {
  objs <- env_get_list(ns_env(ns), names)
  env_bind(env, !!!objs)
}

#' Is an object a namespace environment?
#'
#' @param x An object to test.
#' @export
is_namespace <- function(x) {
  isNamespace(x)
}

env_type <- function(env) {
  if (is_reference(env, global_env())) {
    "global"
  } else if (is_reference(env, empty_env())) {
    "empty"
  } else if (is_reference(env, base_env())) {
    "base"
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
    abort("Unknown environment type.", .internal = TRUE)
  )
}

env_format <- function(env) {
  type <- env_type(env)

  if (type %in% c("frame", "local")) {
    addr <- obj_address(get_env(env))
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
#'   [package environments][search_envs] and the [imports
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
  check_environment(env)

  if (is_reference(env, global_env())) {
    return("global")
  }
  if (is_reference(env, base_env())) {
    return("package:base")
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
    obj_address(env)
  }
}

#' Return the namespace registry env
#'
#' Note that the namespace registry does not behave like a normal
#' environment because the parent is `NULL` instead of the empty
#' environment. This is exported for expert usage in development tools
#' only.
#'
#' @keywords internal
#' @export
ns_registry_env <- function() {
  .Call(ffi_ns_registry_env)
}

on_load({
  base_ns_env <- ns_env("base")
  base_pkg_env <- baseenv()
})
