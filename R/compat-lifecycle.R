# nocov start - compat-lifecycle (last updated: rlang 0.3.0.9000)

# This file serves as a reference for currently unexported rlang
# lifecycle functions. Please find the most recent version in rlang's
# repository. These functions expect rlang to be imported in your
# namespace.


#' Signal deprecation
#'
#' @description
#'
#' `signal_soft_deprecated()` warns only if option is set, the package
#'  is attached, or if called from the global environment.
#'  `warn_deprecated()` warns unconditionally. Both functions warn
#'  only once.
#'
#' Control the verbosity of retirement with scoped global options:
#'
#' * When `lifecycle_disable_warnings` is `TRUE`, no
#'   warnings are issued in any circumstances.
#'
#' * When `lifecycle_force_verbose_retirement` is `TRUE`,
#'   soft-deprecated functions always warn, unless
#'   `lifecycle_disable_warnings` is `TRUE`.
#'
#' @param msg The deprecation message.
#' @param id The id of the deprecation. A warning is issued only once
#'   for each `id`. Defaults to `msg`, but you should give a unique ID
#'   when the message is built programmatically and depends on inputs.
#' @param package The soft-deprecation warning is forced when this
#'   package is attached. Automatically detected from the caller
#'   environment.
#' @param env The environment in which the deprecated function was
#'   called.
#'
#' @noRd
NULL

signal_soft_deprecated <- function(msg,
                                   id = msg,
                                   package = NULL,
                                   env = caller_env(2)) {
  if (is_true(peek_option("lifecycle_disable_warnings"))) {
    return(invisible(NULL))
  }

  if (is_true(peek_option("lifecycle_force_verbose_retirement")) ||
      is_reference(env, global_env())) {
    warn_deprecated(msg, id)
    return(invisible(NULL))
  }

  if (package_attached(package, caller_env())) {
    warn_deprecated(msg, id)
    return(invisible(NULL))
  }

  signal(msg, "lifecycle_soft_deprecated")
}

package_attached <- function(package, env) {
  if (is_null(package)) {
    top <- topenv(env)
    if (!is_namespace(top)) {
      abort("`signal_soft_deprecated()` must be called from a package function")
    }
    package <- ns_env_name(top)
  } else {
    stopifnot(is_string(package))
  }

  pkg_env_name(package) %in% search()
}

warn_deprecated <- function(msg, id = msg) {
  if (is_true(peek_option("lifecycle_disable_warnings"))) {
    return(invisible(NULL))
  }
  if (env_has(deprecation_env, id)) {
    return(invisible(NULL))
  }

  env_poke(deprecation_env, id, TRUE);

  .Deprecated(msg = paste_line(
    msg,
    silver("This warning is displayed once per session.")
  ))
}
deprecation_env <- new.env(parent = emptyenv())

abort_defunct <- function(msg) {
  .Defunct(msg = msg)
}


#' Embed a lifecycle badge in documentation
#'
#' @description
#'
#' Use `lifecycle()` within a `Sexpr` macro to embed a
#' [lifecycle](https://www.tidyverse.org/lifecycle/) badge in your
#' documentation. The badge should appear first in the description:
#'
#' ```
#' \Sexpr[results=rd, stage=render]{mypkg:::lifecycle("questioning")}
#' ```
#'
#' The badge appears as an image in the HTML version of the
#' documentation. To make them available in your package, visit
#' <https://github.com/r-lib/rlang/tree/master/man/figures> and copy
#' all the files starting with `lifecycle-` in your `man/figures/`
#' folder.
#'
#' @param stage A lifecycle stage as a string, one of:
#'   `"experimental"`, `"maturing"`, `"stable"`, `"questioning"`,
#'   `"archived"`, `"soft-deprecated"`, `"deprecated"`, `"defunct"`.
#'
#' @noRd
NULL

lifecycle <- function(stage) {
  url <- paste0("https://www.tidyverse.org/lifecycle/#", stage)
  img <- lifecycle_img(stage, url)

  sprintf(
    "\\ifelse{html}{%s}{\\strong{%s}}",
    img,
    upcase1(stage)
  )
}

lifecycle_img <- function(stage, url) {
  file <- sprintf("lifecycle-%s.svg", stage)
  stage_alt <- upcase1(stage)

  switch(stage,

    experimental = ,
    maturing = ,
    stable = ,
    questioning = ,
    archived =
      sprintf(
        "\\out{<a href='%s'><img src='%s' alt='%s lifecycle'></a>}",
        url,
        file.path("figures", file),
        stage_alt
      )
   ,

    `soft-deprecated` = ,
    deprecated = ,
    defunct =
      sprintf(
        "\\figure{%s}{options: alt='%s lifecycle'}",
        file,
        stage_alt
      ),

    abort(sprintf("Unknown lifecycle stage `%s`", stage))

  )
}
upcase1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# nocov end
