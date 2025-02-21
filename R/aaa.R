the <- new.env(parent = emptyenv())


# `on_load()`, `run_on_load()`, and `on_package_load()` are
# implemented in base-only compat style.
#
# Changelog:
#
# - 2021-09-24:
#   - Now base-only compat.
#   - `on_package_load()` checks that namespace is sealed.
#
# - 2021-05-07: Added `on_package_load()`.
#
# - 2021-04-29: `expr` is now evaluated in caller environment rather
#   than the top environment.

#' Run expressions on load
#'
#' @description
#' - `on_load()` registers expressions to be run on the user's machine
#'   each time the package is loaded in memory. This is by contrast to
#'   normal R package code which is run once at build time on the
#'   packager's machine (e.g. CRAN).
#'
#'   `on_load()` expressions require `run_on_load()` to be called
#'   inside [.onLoad()].
#'
#' - `on_package_load()` registers expressions to be run each time
#'   another package is loaded.
#'
#' `on_load()` is for your own package and runs expressions when the
#' namespace is not _sealed_ yet. This means you can modify existing
#' binding or create new ones. This is not the case with
#' `on_package_load()` which runs expressions after a foreign package
#' has finished loading, at which point its namespace is sealed.
#'
#' @param expr An expression to run on load.
#' @param env The environment in which to evaluate `expr`. Defaults to
#'   the current environment, which is your package namespace if you
#'   run `on_load()` at top level.
#' @param ns The namespace in which to hook `expr`.
#'
#' @section When should I run expressions on load?:
#' There are two main use cases for running expressions on load:
#'
#' 1. When a side effect, such as registering a method with
#'    `s3_register()`, must occur in the user session rather than the
#'    package builder session.
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
#' @section Comparison with `.onLoad()`:
#' `on_load()` has the advantage that hooked expressions can appear in
#' any file, in context. This is unlike `.onLoad()` which gathers
#' disparate expressions in a single block.
#'
#' `on_load()` is implemented via `.onLoad()` and requires
#' `run_on_load()` to be called from that hook.
#'
#' The expressions inside `on_load()` do not undergo static analysis
#' by `R CMD check`. Therefore, it is advisable to only use
#' simple function calls inside `on_load()`.
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
#' on_load({
#'   s3_register("foo::bar", "my_class")
#' })
#'
#' # Assign an object on load
#' var <- NULL
#' on_load({
#'   var <- foo()
#' })
#'
#' # To use `on_package_load()` at top level, wrap it in `on_load()`
#' on_load({
#'   on_package_load("foo", message("foo is loaded"))
#' })
#'
#' # In functions it can be called directly
#' f <- function() on_package_load("foo", message("foo is loaded"))
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

#' @rdname on_load
#' @param pkg Package to hook expression into.
#' @export
on_package_load <- function(pkg, expr, env = parent.frame()) {
  expr <- substitute(expr)
  force(env)

  run <- function(...) {
    # Evaluate with promise semantics rather than `base::eval()`
    do <- NULL
    do.call(delayedAssign, list("do", expr, env))
    do
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(pkg, "onLoad"), run)

  # For compatibility with R < 4.0 where base isn't locked
  is_sealed <- function(pkg) {
    identical(pkg, "base") || environmentIsLocked(asNamespace(pkg))
  }

  # Run right away if package is already loaded but only if its
  # namespace is locked. The registering package might be a dependency
  # of `package`. In that case, `package` might not be fully populated
  # yet (#1225).
  if (isNamespaceLoaded(pkg) && is_sealed(pkg)) {
    run()
  }
}
