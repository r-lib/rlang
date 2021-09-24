the <- new.env(parent = emptyenv())


# `on_load()` and `run_on_load()` are implemented in base-only compat
# style.
#
# Changelog:
#
# - 2021-09-24: Now base-only compat.
#
# - 2021-05-07: Added `on_package_load()`.
#
# - 2021-04-29: `expr` is now evaluated in caller environment rather
#   than the top environment.

#' Run expressions on load
#'
#' @description
#' `on_load()` registers expressions to be run on the user's machine
#' each time the package is loaded in memory. This is by contrast to
#' normal R package code which is run once at build time on the
#' packager's machine (e.g. CRAN). There are two main use cases for
#' running expressions on load:
#'
#' 1. When a side effect, such as registering a method with
#'   `s3_register()`, must occur oi the user session rather than the
#'   builder session machine.
#'
#' 2. To avoid hard-coding objects from other packages in your
#'    namespace. If you assign `foo::bar` or the result of
#'    `foo::baz()` in your package, they become constants. Any
#'    upstream changes in the `foo` package will not be reflected in
#'    the objects you've assigned in your namespace. This often breaks
#'    assumptions made by the authors of `foo` and causes all sorts of
#'    issues.
#'
#'    Recreating the foreign objects each time your package is loaded
#'    makes sure that any such changes will be taken into account. In
#'    technical terms, running an expression on load introduces
#'    _indirection_.
#'
#' `on_load()` expressions require `run_on_load()` to be called inside
#' [.onLoad()].
#'
#' @param expr An expression to run on load.
#' @param env The environment in which to evaluate `expr`. Defaults to
#'   the current environment, which is your package namespace if you
#'   run `on_load()` at top level.
#' @param ns The namespace in which to hook `expr`.
#'
#' @section Comparison with `.onLoad()`:
#' `on_load()` has the advantage that hooked expressions can appear in
#' any file, in context. This is unlike `.onLoad()` which gathers
#' disparate expressions in a single block.
#'
#' `on_load()` is implemented via `.onLoad()` and requires
#' `run_on_load()` to be called from that hook.
#'
#' @examples
#' quote({  # Not run
#' 
#' # First add `run_on_load()` to your `.onLoad()` hook,
#' # then use `on_load()` anywhere in your package
#' .onLoad <- function(lib, pkg) {
#'   run_on_load()
#' }
#'
#' # Register a method on load
#' on_load(s3_register("foo::bar", "my_class"))
#'
#' # Assign an object on load
#' var <- NULL
#' on_load(
#'   var <- foo()
#' )
#'
#' })
#' @export
on_load <- function(expr, env = parent.frame(), ns = topenv(env)) {
  expr <- substitute(expr)
  force(env)

  callback <- function() {
    # Evaluate with promise semantics rather than `base::eval()`
    do <- NULL
    do.call(delayedAssign, list("do", expr, env))
    do
  }
  ns$.__rlang_hook__. <- c(ns$.__rlang_hook__., list(callback))
}
#' @rdname on_load
#' @export
run_on_load <- function(ns = topenv(parent.frame())) {
  hook <- ns$.__rlang_hook__.
  rm(envir = ns, list = ".__rlang_hook__.")

  # FIXME: Transform to `while` loop to allow hooking into on-load
  # from an on-load hook?
  for (callback in hook) {
    callback()
  }
}

on_package_load <- function(pkg, expr) {
  if (isNamespaceLoaded(pkg)) {
    expr
  } else {
    thunk <- function(...) expr
    setHook(packageEvent(pkg, "onLoad"), thunk)
  }
}
