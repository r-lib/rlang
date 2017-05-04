#' @rdname quosure
#' @export
enquo <- function(arg) {
  if (missing(arg)) {
    return(new_quosure(missing_arg(), empty_env()))
  }

  capture <- lang(captureArg, substitute(arg))
  arg <- eval_bare(capture, caller_env())
  expr <- .Call(rlang_interp, arg$expr, arg$env, TRUE)
  forward_quosure(expr, arg$env)
}
forward_quosure <- function(expr, env) {
  if (is_quosure(expr)) {
    expr
  } else if (is_definition(expr)) {
    as_quosureish(expr, env)
  } else if (is_symbolic(expr)) {
    new_quosure(expr, env)
  } else {
    new_quosure(expr, empty_env())
  }
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

  set_names(dots, nms)
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
