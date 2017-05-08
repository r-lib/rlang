#' Extract dots with splicing semantics
#'
#' These functions evaluate all arguments contained in `...` and
#' return them as a list. They both splice their arguments if they
#' qualify for splicing. See [ll()] for information about splicing
#' and below for the kind of arguments that qualify for splicing.
#'
#' `dots_list()` has _explicit splicing semantics_: it splices lists
#' that are explicitly marked for [splicing][ll] with the
#' [splice()] adjective. `dots_splice()` on the other hand has _list
#' splicing semantics_: in addition to lists marked explicitly for
#' splicing, [bare][is_bare_list] lists are spliced as well.
#'
#' @inheritParams dots_values
#' @param ... Arguments with explicit (`dots_list()`) or list
#'   (`dots_splice()`) splicing semantics. The contents of spliced
#'   arguments are embedded in the returned list.
#' @return A list of arguments. This list is always named: unnamed
#'   arguments are named with the empty string `""`.
#' @seealso [exprs()] for extracting dots without evaluation.
#' @export
#' @examples
#' # Compared to simply using list(...) to capture dots, dots_list()
#' # splices explicitly:
#' x <- list(1, 2)
#' dots_list(!!! x, 3)
#'
#' # Unlike dots_splice(), it doesn't splice bare lists:
#' dots_list(x, 3)
#'
#' # Splicing is also helpful to workaround exact and partial matching
#' # of arguments. Let's create a function taking named arguments and
#' # dots:
#' fn <- function(data, ...) {
#'   dots_list(...)
#' }
#'
#' # You normally cannot pass an argument named `data` through the dots
#' # as it will match `fn`'s `data` argument. The splicing syntax
#' # provides a workaround:
#' fn(some_data, !!! list(data = letters))
dots_list <- function(..., .ignore_empty = c("trailing", "none", "all")) {
  dots <- dots_values(..., .ignore_empty = .ignore_empty)
  dots <- .Call(rlang_squash, dots, "list", is_spliced, 1L)
  names(dots) <- names2(dots)
  dots
}
#' @rdname dots_list
#' @export
#' @examples
#'
#' # dots_splice() splices lists marked with splice() as well as bare
#' # lists:
#' x <- list(1, 2)
#' dots_splice(!!! x, 3)
#' dots_splice(x, 3)
dots_splice <- function(..., .ignore_empty = c("trailing", "none", "all")) {
  dots <- dots_values(..., .ignore_empty = .ignore_empty)
  dots <- .Call(rlang_squash, dots, "list", is_spliced_bare, 1L)
  names(dots) <- names2(dots)
  dots
}

#' Evaluate dots with preliminary splicing
#'
#' This is a tool for advanced users. It captures dots, processes
#' unquoting and splicing operators, and evaluates them. Unlike
#' [dots_list()] and [dots_splice()], it does not flatten spliced
#' objects. They are merely attributed a `spliced` class (see
#' [splice()]). You can process spliced objects manually, perhaps with
#' a custom predicate (see [flatten_if()]).
#'
#' @param ... Arguments to evaluate and process splicing operators.
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty.
#' @export
#' @examples
#' dots <- dots_values(!!! list(1))
#' dots
#'
#' # Flatten the spliced objects:
#' flatten_if(dots, is_spliced)
dots_values <- function(..., .ignore_empty = c("trailing", "none", "all")) {
  dots <- dots_capture(..., `__quosured` = FALSE)
  dots <- dots_clean_empty(dots, function(x) is_missing(x$expr), .ignore_empty)
  if (is_null(dots)) {
    dots <- list(...)
    set_names(dots, names2(dots))
  } else {
    map(dots, function(dot) eval_bare(dot$expr, dot$env))
  }
}

dots_clean_empty <- function(dots, is_empty, ignore_empty) {
  n_dots <- length(dots)

  if (n_dots) {
    which <- match.arg(ignore_empty, c("trailing", "none", "all"))
    switch(which,
      trailing =
        if (is_empty(dots[[n_dots]])) {
          dots[[n_dots]] <- NULL
        },
      all = {
        dots <- discard(dots, is_empty)
      }
    )
  }

  dots
}


#' @rdname quosures
#' @export
dots_definitions <- function(..., .named = FALSE) {
  dots <- dots_enquose(..., `__interp_lhs` = FALSE)
  if (.named) {
    width <- quo_names_width(.named)
    dots <- quos_auto_name(dots, width)
  }

  is_def <- map_lgl(dots, function(dot) is_definition(f_rhs(dot)))
  defs <- map(dots[is_def], as_definition)

  list(dots = dots[!is_def], defs = defs)
}
as_definition <- function(def) {
  # The definition comes wrapped in a quosure
  env <- f_env(def)
  def <- f_rhs(def)

  list(
    lhs = new_quosure(f_lhs(def), env),
    rhs = new_quosure(f_rhs(def), env)
  )
}

is_dot_symbol <- function(x) {
  is_symbol(x) && is_dot_nm(as.character(x))
}
is_dot_nm <- function(nm) {
  grepl("^\\.\\.[0-9]+$", nm)
}

#' How many arguments are currently forwarded in dots?
#'
#' This returns the number of arguments currently forwarded in `...`
#' as an integer.
#'
#' @param ... Forwarded arguments.
#' @export
#' @examples
#' fn <- function(...) dots_n(..., baz)
#' fn(foo, bar)
dots_n <- function(...) {
  nargs()
}

dots_capture <- function(..., `__interp_lhs` = TRUE, `__quosured` = TRUE) {
  info <- captureDots(strict = `__quosured`)

  # No interpolation because dots were already evaluated
  if (is_null(info)) {
    return(NULL)
  }

  dots <- map(info, dot_interp, quosured = `__quosured`)

  # Flatten possibly spliced dots
  dots <- unlist(dots, FALSE) %||% set_names(list())

  if (`__interp_lhs`) {
    dots <- dots_interp_lhs(dots)
  }

  dots
}
dot_interp <- function(dot, quosured = TRUE) {
  if (is_missing(dot$expr)) {
    return(list(dot))
  }
  env <- dot$env
  expr <- dot$expr

  # Allow unquote-splice in dots
  if (is_splice(expr)) {
    dots <- call("alist", expr)
    dots <- .Call(rlang_interp, dots, env, quosured)
    dots <- eval_bare(dots)
    map(dots, function(expr) list(expr = expr, env = env))
  } else {
    expr <- .Call(rlang_interp, expr, env, quosured)
    list(list(expr = expr, env = env))
  }
}

dots_enquose <- function(..., `__interp_lhs` = TRUE) {
  dots <- dots_capture(..., `__interp_lhs` = `__interp_lhs`)
  map(dots, dot_enquose)
}
dot_enquose <- function(dot) {
  if (is_missing(dot$expr)) {
    new_quosure(missing_arg(), empty_env())
  } else {
    forward_quosure(dot$expr, dot$env)
  }
}

is_bang <- function(expr) {
  is_lang(expr) && identical(node_car(expr), quote(`!`))
}
is_splice <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }

  if (identical(node_car(expr), quote(UQS)) || identical(node_car(expr), quote(rlang::UQS))) {
    return(TRUE)
  }

  if (is_bang(expr) && is_bang(node_cadr(expr)) && is_bang(node_cadr(node_cadr(expr)))) {
    return(TRUE)
  }

  FALSE
}

dots_interp_lhs <- function(dots) {
  nms <- names2(dots)
  defs <- map_lgl(dots, function(dot) is_definition(dot$expr))

  for (i in which(defs)) {
    info <- dot_interp_lhs(nms[[i]], dots[[i]])
    dots[[i]] <- info$dot

    if (!is_null(info$name)) {
      nms[[i]] <- info$name
    }
  }

  names(dots) <- nms
  dots
}
dot_interp_lhs <- function(name, dot) {
  if (!is_null(name) && name != "") {
    warn("name ignored because a LHS was supplied")
  }

  lhs <- .Call(rlang_interp, f_lhs(dot$expr), dot$env, FALSE)
  if (is_symbol(lhs)) {
    lhs <- as_string(lhs)
  } else if (!is_string(lhs)) {
    abort("LHS must be a name or string")
  }

  dot <- list(expr = f_rhs(dot$expr), env = dot$env)
  list(name = lhs, dot = dot)
}
