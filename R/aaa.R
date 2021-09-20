the <- new.env(parent = emptyenv())

# Changelog:
#
# - 2021-05-07: Added `on_package_load()`.
#
# - 2021-04-29: `expr` is now evaluated in caller environment rather
#   than the top environment.

on_load <- function(expr, env = parent.frame(), ns = topenv(env)) {
  expr <- substitute(expr)
  force(env)

  callback <- function() eval_bare(expr, env)
  ns$.__rlang_hook__. <- c(ns$.__rlang_hook__., list(callback))
}

run_on_load <- function(env = parent.frame()) {
  ns <- topenv(env)

  hook <- ns$.__rlang_hook__.
  env_unbind(ns, ".__rlang_hook__.")

  # FIXME: Transform to `while` loop to allow hooking into on-load
  # from an on-load hook?
  for (callback in hook) {
    callback()
  }

  ns$.__rlang_hook__. <- NULL
}

on_package_load <- function(pkg, expr) {
  if (isNamespaceLoaded(pkg)) {
    expr
  } else {
    thunk <- function(...) expr
    setHook(packageEvent(pkg, "onLoad"), thunk)
  }
}
