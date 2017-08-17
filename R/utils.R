
substitute_ <- function(x, env) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }

  call <- substitute(substitute(x, env), list(x = x))
  eval_bare(call)
}

drop_last <- function(x) {
  x[-length(x)]
}
drop_first <- function(x) {
  x[-1]
}
set_names2 <- function(x, nms = names2(x)) {
  empty <- nms == ""
  nms[empty] <- x[empty]
  names(x) <- nms
  x
}

imap <- function(.x, .f, ...) {
  idx <- names(.x) %||% seq_along(.x)
  out <- Map(.f, idx, .x, ...)
  names(out) <- names(.x)
  out
}
imap_chr <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "character")
}

map_around <- function(.x, .neighbour = c("right", "left"), .f, ...) {
  where <- arg_match(.neighbour)
  n <- length(.x)
  out <- vector("list", n)

  if (n == 0) {
    return(.x)
  }

  if (n == 1) {
    out[[1]] <- .f(.x[[1]], missing_arg(), ...)
    return(out)
  }

  if (n > 1 && where == "right") {
    neighbours <- .x[seq(2, n)]
    idx <- seq_len(n - 1)
    out[idx] <- Map(.f, .x[idx], neighbours, ...)
    out[[n]] <- .f(.x[[n]], missing_arg(), ...)
    return(out)
  }

  if (n > 1 && where == "left") {
    neighbours <- .x[seq(1, n - 1)]
    idx <- seq(2, n)
    out[idx] <- Map(.f, .x[idx], neighbours, ...)
    out[[1]] <- .f(.x[[1]], missing_arg(), ...)
    return(out)
  }

  stop("unimplemented")
}

discard_unnamed <- function(x) {
  if (is_env(x)) {
    x
  } else {
    discard(x, names2(x) == "")
  }
}

sxp_address <- function(x) {
  .Call(rlang_sxp_address, x)
}

captureArg <- function(x, strict = TRUE) {
  caller_env <- parent.frame()

  if (identical(caller_env, globalenv())) {
    stop("must be called in a function")
  }
  if (missing(x)) {
    stop("argument \"x\" is missing")
  }

  .Call(rlang_capturearg, NULL, NULL, pairlist(caller_env, strict), get_env())
}
captureDots <- function(strict = TRUE) {
  caller_env <- parent.frame()

  if (!exists("...", caller_env)) {
    stop("must be called in a function where dots exist")
  }

  .Call(rlang_capturedots, NULL, NULL, pairlist(caller_env, strict), get_env())
}

scoped_exit <- function(expr, frame = caller_env()) {
  expr <- enexpr(expr)

  # Inline everything so the call will succeed in any environment
  expr <- lang(on.exit, expr, add = TRUE)
  eval_bare(expr, frame)

  invisible(expr)
}

scoped_options <- function(..., .frame = caller_env()) {
  options <- dots_list(...)
  stopifnot(is_named(options))

  old <- options(options)
  options_lang <- lang(base::options, !!! old)
  scoped_exit(!! options_lang, frame = .frame)

  invisible(old)
}

poke_options <- function(...) {
  options(dots_list(...))
}
peek_options <- function(...) {
  options(chr(...))
}
peek_option <- function(option) {
  getOption(option)
}

slide <- function(.x, .f, ...) {
  n <- length(.x)
  if (n <= 1) {
    abort("Can't slide on a vector that is not at least length 2")
  }

  out <- list_len(n - 1)
  for (i in seq_len(n - 1)) {
    out[[i]] <- .f(.x[[i]], .x[[i + 1]], ...)
  }

  out
}
slide_lgl <- function(.x, .f, ...) {
  flatten_lgl(slide(.x, .f, ...))
}
