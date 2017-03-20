#' Standardise a call against formal arguments.
#'
#' This is a slower but more thorough version of
#' [lang_standardise()]. It is useful for tracing arguments across a
#' call stack (see [arg_inspect()] for an example).
#'
#' Compared to [match.call()], `lang_homogenise()`:
#'
#' * Always report dotted arguments as such: `call(..1, ..2)`.  In
#'   comparison, `match.call()` inlines literals: `call("foo", 10)`.
#'
#' * Provides an argument `enum_dots` to impose enumerated names on
#'   dotted arguments. This produces `call(..1 = x, ..2 = ..3)`
#'   instead of `call(foo = x, ..3)`.
#'
#' * Does not sort arguments according to the order of appearance in
#'   the function definition.
#'
#' * Standardises missing arguments as well if you specify
#'   `add_missings`: `call(x = , y = , )`.
#'
#' @param call Can be a call, a formula quoting a call in the
#'   right-hand side, or a frame object from which to extract the call
#'   expression. If not supplied, the calling frame is used. Note that
#'   [lang_homogenise()] needs access to the actual function
#'   corresponding to the call. It will retrieve it from the tidy
#'   quote or frame environment, or in the calling context.
#' @param dots_env Calling frame in which to look up call the contents
#'   of `...`. If not supplied and `call` is a frame object, it is
#'   retrieved from `call`.
#' @param enum_dots Whether to standardise the names of dotted
#'   arguments. If `TRUE`, this produces calls such as `f(..1 = ..2)`
#'   instead of `f(..2)`. The first form is mostly intended for code
#'   analysis tools as it makes it easier to climb arguments through
#'   nested calls. The latter form (the default) is more faithful to
#'   the actual call and is ready to be evaluated.
#' @param add_missings Whether to standardise missing arguments.
#' @export
lang_homogenise <- function(call = caller_frame(),
                            dots_env = NULL,
                            enum_dots = FALSE,
                            add_missings = FALSE) {

  if (is_frame(call)) {
    # Check for global frame
    if (call$pos == 0) {
      return(NULL)
    }
    dots_env <- sys_frame(call$caller_pos)
    fn <- call$fn
  } else {
    fn <- NULL
  }

  call <- as_quosure(call, caller_env())
  fn <- fn %||% lang_fn(call)

  lang_homogenise_(f_rhs(call), fn, dots_env, enum_dots, add_missings)
}

lang_homogenise_ <- function(call, fn, dots_env, enum_dots, add_missings) {
  call <- duplicate(call)
  call <- call_inline_dots(call, dots_env, enum_dots)
  call <- call_match_partial(call, fn)
  call <- call_match(call, fn, enum_dots, add_missings)
  call
}

arg_match_partial <- function(arg, formal) {
  formal <- substr(formal, 1, nchar(arg))
  arg == formal
}

call_match_partial <- function(call, fn) {
  actuals_nms <- lang_args_names(call)
  formals_nms <- fn_fmls_names(fn)

  is_empty <- actuals_nms == ""
  is_dup <- duplicated(actuals_nms) & !is_empty
  is_dup <- is_dup & actuals_nms %in% formals_nms
  if (any(is_dup)) {
    dups_nms <- actuals_nms[which(is_dup)]
    abort(paste0(
      "formal arguments matched by multiple actual arguments: ",
      paste0(dups_nms, collapse = ", ")
    ))
  }

  dots_pos <- match("...", formals_nms)

  # No partial-matching of args after dots
  if (!is.na(dots_pos)) {
    formals_nms <- formals_nms[seq_len(dots_pos) - 1]
  }
  formals_pool <- setdiff(formals_nms, actuals_nms)

  is_matched <- actuals_nms %in% formals_nms
  actuals_pat <- actuals_nms
  actuals_pat[is_matched | is_empty] <- NA

  matched <- list()
  for (formal in formals_pool) {
    matched_pos <- which(map_lgl(actuals_pat, arg_match_partial, formal))

    if (length(matched_pos) > 1) {
      abort(paste0(
        "formal argument `", formal,
        "` matched by multiple actual arguments"
      ))
    }
    if (length(matched_pos) && matched_pos %in% matched) {
      abort(paste0(
        "actual argument `", actuals_pat[matched_pos],
        "` matches multiple formal arguments"
      ))
    }

    matched <- append(matched, matched_pos)
    actuals_nms[matched_pos] <- formal
  }

  if (is.na(dots_pos)) {
    is_unused <- !actuals_nms %in% c(formals_nms, "")
    is_unused <- is_unused & !map_lgl(actuals_nms, is_dot_nm)
    if (any(is_unused)) {
      abort(paste0(
        "unused arguments: ",
        paste0(actuals_nms[is_unused], collapse = ", ")
      ))
    }
    if (length(actuals_nms) > length(formals_nms)) {
      abort("unused arguments")
    }
  }

  names(call) <- c("", actuals_nms)
  call
}

call_inline_dots <- function(call, dots_env, enum_dots) {
  d <- node_walk_nonnull(call, function(arg) {
    if (identical(node_cadr(arg), quote(...))) arg
  })
  if (is_null(d)) {
    return(call)
  }
  if (is_null(dots_env)) {
    abort("`dots_env` must be supplied to match dots")
  }

  if (env_has(dots_env, "...")) {
    dots <- node_cdr(substitute(alist(...), dots_env))
  } else {
    dots <- pairlist()
  }
  dots <- dots_enumerate_args(dots)

  # Attach remaining args to expanded dots
  remaining_args <- node_cddr(d)
  node_walk_nonnull(dots, function(arg) {
    if (is_null(node_cdr(arg))) set_node_cdr(arg, remaining_args)
  })

  # Replace dots symbol with actual dots and remaining args
  set_node_cdr(d, dots)

  call
}

call_match <- function(call, fn, enum_dots, add_missings) {
  args <- call[-1]
  args_nms <- names2(args)
  fmls_nms <- names2(fn_fmls(fn))

  dots_i <- which(fmls_nms == "...")
  if (length(dots_i)) {
    args_nms <- call_match_dotted(args_nms, fmls_nms, dots_i, enum_dots)
  } else {
    args_nms <- call_match_args(args_nms, fmls_nms)
  }
  names(call) <- c("", args_nms)

  if (add_missings) {
    missing_nms <- setdiff(fmls_nms, c(args_nms, "..."))
    missing_args <- rep(list(missing_arg()), length(missing_nms))
    missing_args <- as.pairlist(set_names(missing_args, missing_nms))
    call <- node_append(call, missing_args)
  }

  call
}

call_match_dotted <- function(args_nms, fmls_nms, dots_i, enum_dots) {
  # First match formals on the left of dots
  is_unmatched <- map_lgl(args_nms, `==`, "")
  candidates <- fmls_nms[seq_len(dots_i - 1)]
  candidates <- setdiff(candidates, args_nms[!is_unmatched])
  args_nms[is_unmatched] <- call_match_args(args_nms[is_unmatched], candidates)

  if (enum_dots) {
    is_matched <- map_lgl(args_nms, `%in%`, fmls_nms)
    n_dots <- sum(!is_matched)
    args_nms[!is_matched] <- paste0("..", seq_len(n_dots))
  }

  args_nms
}

call_match_args <- function(args_nms, fmls_nms) {
  is_unmatched <- map_lgl(args_nms, `==`, "")

  # Only match up to the number of formals
  n_fmls <- length(setdiff(fmls_nms, "..."))
  n_args <- length(args_nms)
  if (n_args > n_fmls) {
    is_ignored <- rep(TRUE, n_args)
    is_ignored[seq_len(n_fmls)] <- FALSE
    is_unmatched <- is_unmatched & !is_ignored
  }

  candidates <- setdiff(fmls_nms, args_nms[!is_unmatched])
  args_nms[is_unmatched] <- candidates[seq_len(sum(is_unmatched))]
  args_nms
}
