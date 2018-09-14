#' Capture a call trace.
#'
#' A call trace captures the sequence of calls that lead to the current function,
#' sometimes called the call stack. Because of lazy evaluation, the call
#' stack in R is actually a tree, which the print method of this object will
#' reveal.
#'
#' @param to If non-null, this should be a frame environment. The
#'   backtrace will only be recorded up to that frame. This is needed
#'   in particular when you call `trace_back()` indirectly or from a
#'   larger context, for example in tests or inside an RMarkdown
#'   document where you don't want all of the knitr evaluation mechanisms
#'   to appear in the backtrace.
#' @examples
#' # Trim backtraces automatically (this improves the generated
#' # documentation for the rlang website and the same trick can be
#' # useful within knitr documents):
#' options(rlang_trace_top_env = current_env())
#'
#' f <- function() g()
#' g <- function() h()
#' h <- function() trace_back()
#'
#' # When no lazy evaluation is involved the backtrace is linear
#' # (i.e. every call has only one child)
#' f()
#'
#' # Lazy evaluation introduces a tree like structure
#' identity(identity(f()))
#' identity(try(f()))
#' try(identity(f()))
#'
#' # When printing, you can request to simplify this tree to only show
#' # the direct sequence of calls that lead to `trace_back()`
#' x <- try(identity(f()))
#' x
#' print(x, simplify = "branch")
#'
#' # With a little cunning you can also use it to capture the
#' # tree from within a base NSE function
#' x <- NULL
#' with(mtcars, {x <<- f(); 10})
#' x
#'
#' # When code is executed indirectly, i.e. via source or within an
#' # RMarkdown document, you'll tend to get a lot of guff at the beginning
#' # related to the execution environment:
#' conn <- textConnection("f()")
#' source(conn, echo = TRUE)
#' close(conn)
#'
#' # To automatically strip this off, pass `to = globalenv()`.
#' # This will automatically trim off calls prior to the last appearance
#' # of the global environment on the stack
#' h <- function() trace_back(globalenv())
#'
#' conn <- textConnection("f()")
#' source(conn, echo = TRUE)
#' close(conn)
#'
#' # Restore defaults
#' options(rlang_trace_top_env = NULL)
#' @export
trace_back <- function(to = NULL) {
  calls <- as.list(sys.calls())
  parents <- normalise_parents(sys.parents())
  frames <- sys.frames()
  envs <- map(frames, env_label)

  calls <- add_pipe_pointer(calls, frames)

  trace <- new_trace(calls, parents, envs)
  trace <- trace_trim_env(trace, to)

  # remove call to self
  trace <- trace_subset(trace, -trace_length(trace))

  trace
}

# Assumes magrittr 1.5
add_pipe_pointer <- function(calls, frames) {
  pipe_begs <- which(map_lgl(calls, is_call, "%>%"))
  pipe_kinds <- map_int(pipe_begs, pipe_call_kind, calls)

  pipe_calls <- map2(pipe_begs, pipe_kinds, function(beg, kind) {
    call <- calls[[beg]]

    if (kind == 0L) {
      return(call)
    }

    if (kind == 1L) {
      v <- "i"
    } else if (kind == 2L) {
      v <- "k"
    }

    i <- beg + 5L
    structure(call, pipe_pointer = frames[[i]][[v]])
  })

  calls[pipe_begs] <- pipe_calls
  calls
}

pipe_call_kind <- function(beg, calls) {
  end1 <- beg + 6L
  end2 <- beg + 7L

  if (end2 > length(calls)) {
    return(0L)
  }

  # Uncomplete pipe call
  magrittr_call1 <- quote(function_list[[i]](value))

  # Last call of the pipe
  magrittr_call2 <- quote(function_list[[k]](value))

  if (identical(calls[[end1]], magrittr_call1)) {
    return(1L)
  }
  if (identical(calls[[end2]], magrittr_call2)) {
    return(2L)
  }
  0L
}

# Remove recursive frames which occur with quosures
normalise_parents <- function(parents) {
  recursive <- parents == seq_along(parents)
  parents[recursive] <- 0L
  parents
}

new_trace <- function(calls, parents, envs) {
  stopifnot(is.list(calls), is.integer(parents), length(calls) == length(parents))

  structure(
    list(
      calls = calls,
      parents = parents,
      envs = envs
    ),
    class = "rlang_trace"
  )
}

# Methods -----------------------------------------------------------------

#' @export
format.rlang_trace <- function(x,
                               ...,
                               simplify = c("collapse", "branch", "none"),
                               max_frames = NULL,
                               dir = getwd(),
                               srcrefs = NULL) {
  if (trace_length(x) == 0) {
    return(trace_root())
  }

  switch(arg_match(simplify),
    none = trace_format(x, max_frames, dir, srcrefs),
    collapse = trace_format_collapse(x, max_frames, dir, srcrefs),
    branch = trace_format_trail(x, max_frames, dir, srcrefs)
  )
}

trace_format <- function(trace, max_frames, dir, srcrefs) {
  if (!is_null(max_frames)) {
    msg <- "`max_frames` is currently only supported with `simplify = \"branch\"`"
    stop(msg, call. = FALSE)
  }

  tree <- trace_as_tree(trace, dir = dir, srcrefs = srcrefs)
  cli_tree(tree)
}
trace_format_collapse <- function(trace, max_frames, dir, srcrefs) {
  trace <- trace_simplify_collapse(trace)
  trace_format(trace, max_frames, dir, srcrefs)
}
trace_format_trail <- function(trace, max_frames, dir, srcrefs) {
  trace <- trace_simplify_branch(trace)
  trace <- trail_uncollapse_pipe(trace)
  tree <- trace_as_tree(trace, dir = dir, srcrefs = srcrefs)

  branch <- tree[-1, ][["call"]]
  cli_branch(branch, max_frames)
}

format_collapsed <- function(what, n) {
  if (n > 0L) {
    call_text <- pluralise_n(n, "call", "calls")
    n_text <- sprintf(" with %d more %s", n, call_text)
  } else {
    n_text <- ""
  }

  paste0(what, n_text)
}
format_collapsed_trail <- function(what, n, style = NULL) {
  style <- style %||% cli_box_chars()
  what <- sprintf(" %s %s", style$h, what)
  format_collapsed(what, n)
}

cli_branch <- function(lines, max = NULL, style = NULL) {
  if (!length(lines)) {
    return(chr())
  }

  style <- style %||% cli_box_chars()
  lines <- paste0(" ", style$h, lines)

  if (is_null(max)) {
    return(lines)
  }

  stopifnot(
    is_scalar_integerish(max, finite = TRUE),
    max > 0L
  )

  n <- length(lines)
  if (n <= max) {
    return(lines)
  }

  style <- style %||% cli_box_chars()
  n_collapsed <- n - max

  collapsed_line <- format_collapsed_trail("...", n_collapsed, style = style)

  if (max == 1L) {
    lines <- chr(
      lines[1L],
      collapsed_line
    )
    return(lines)
  }

  half <- max / 2L
  n_top <- ceiling(half)
  n_bottom <- floor(half)

  chr(
    lines[seq(1L, n_top)],
    collapsed_line,
    lines[seq(n - n_bottom + 1L, n)]
  )
}


#' @export
print.rlang_trace <- function(x,
                              ...,
                              simplify = c("collapse", "branch", "none"),
                              max_frames = NULL,
                              dir = getwd(),
                              srcrefs = NULL) {
  cat_line(format(x, ...,
    simplify = simplify,
    max_frames = max_frames,
    dir = dir,
    srcrefs = srcrefs
  ))
  invisible(x)
}

trace_length <- function(x) {
  length(x$calls)
}

trace_subset <- function(x, i) {
  stopifnot(is_integerish(i))
  if (!length(i)) {
    return(new_trace(list(), int(), list()))
  }

  n <- trace_length(x)

  if (all(i < 0L)) {
    i <- setdiff(seq_len(n), abs(i))
  }

  calls <- x$calls[i]
  envs <- x$envs[i]
  parents <- match(as.character(x$parents[i]), as.character(i), nomatch = 0)

  new_trace(calls, parents, envs)
}

# For internal use only
c.rlang_trace <- function(...) {
  traces <- list(...)

  calls <- flatten(map(traces, `[[`, "calls"))
  parents <- flatten_int(map(traces, `[[`, "parents"))
  envs <- flatten(map(traces, `[[`, "envs"))

  new_trace(calls, parents, envs)
}

# Trimming ----------------------------------------------------------------

trace_trim_env <- function(x, to = NULL) {
  to <- to %||% peek_option("rlang_trace_top_env")

  if (is.null(to)) {
    return(x)
  }

  is_top <- x$envs == env_label(to)
  if (!any(is_top)) {
    return(x)
  }

  start <- last(which(is_top)) + 1
  end <- length(x$envs)

  trace_subset(x, start:end)
}

set_trace_skipped <- function(trace, id, n) {
  attr(trace$calls[[id]], "collapsed") <- n
  trace
}
set_trace_collapsed <- function(trace, id, n) {
  attr(trace$calls[[id - n]], "collapsed") <- n
  trace
}
n_collapsed <- function(trace, id) {
  call <- trace$calls[[id]]

  if (is_call(call, c("eval", "evalq"), ns = c("", "base"))) {
    return(1L)
  }

  if (identical(call, quote(function_list[[i]](value)))) {
    return(6L)
  }

  if (identical(call, quote(function_list[[k]](value)))) {
    return(7L)
  }

  0L
}

pipe_collect_calls <- function(pipe) {
  node <- node_cdr(pipe)
  last_call <- pipe_add_dot(node_cadr(node))
  calls <- new_node(last_call, NULL)

  while (is_call(node_car(node), "%>%")) {
    node <- node_cdar(node)
    call <- pipe_add_dot(node_cadr(node))
    calls <- new_node(call, calls)
  }

  # The first call doesn't need a dot
  first_call <- node_car(node)
  if (is_call(first_call)) {
    calls <- new_node(first_call, calls)
    leading <- TRUE
  } else {
    leading <- FALSE
  }

  list(calls = as.list(calls), leading = leading)
}
pipe_add_dot <- function(call) {
  if (!is_call(call)) {
    return(call2(call, dot_sym))
  }

  node <- node_cdr(call)
  while (!is_null(node)) {
    if (identical(node_car(node), dot_sym)) {
      return(call)
    }
    node <- node_cdr(node)
  }

  args <- new_node(dot_sym, node_cdr(call))
  new_call(node_car(call), args)
}

has_pipe_pointer <- function(x) {
  !is_null(attr(x, "pipe_pointer"))
}

# Assumes a backtrace branch with collapsed pipe
trail_uncollapse_pipe <- function(trace) {
  while (idx <- detect_index(trace$calls, has_pipe_pointer)) {
    trace_before <- trace_subset(trace, seq2(1L, idx - 1L))
    trace_after <- trace_subset(trace, seq2(idx + 2L, trace_length(trace)))

    pipe <- trace$calls[[idx]]
    pipe_info <- pipe_collect_calls(pipe)
    pipe_calls <- pipe_info$calls

    pointer <- attr(pipe, "pipe_pointer")
    if (!is_scalar_integer(pointer)) {
      stop("Internal error: Invalid pipe pointer")
    }

    if (pipe_info$leading) {
      pointer <- inc(pointer)
    }

    incomplete <- seq2(pointer + 1L, length(pipe_calls))
    if (length(incomplete)) {
      pipe_calls <- pipe_calls[-incomplete]
    }

    parent <- trace$parents[[idx]]
    pipe_parents <- seq(parent, parent + pointer - 1L)

    # Assign the pipe frame as dummy envs for uncollapsed frames
    pipe_envs <- rep(trace$envs[idx], pointer)

    # Add the number of uncollapsed frames to children's
    # ancestry. This assumes a backtrace branch.
    trace_after$parents <- trace_after$parents + pointer

    trace$calls <- c(trace_before$calls, pipe_calls, trace_after$calls)
    trace$parents <- c(trace_before$parents, pipe_parents, trace_after$parents)
    trace$envs <- c(trace_before$envs, pipe_envs, trace$envs)
  }

  trace
}

trace_simplify_branch <- function(trace) {
  parents <- trace$parents
  path <- int()
  id <- length(parents)

  while (id != 0L) {
    n_collapsed <- n_collapsed(trace, id)

    if (n_collapsed) {
      trace <- set_trace_collapsed(trace, id, n_collapsed)
      next_id <- id - n_collapsed

      # Rechain child of collapsed parent to correct parent
      parents[[id + 1L]] <- next_id

      id <- next_id
    }

    if (!is_uninformative_call(trace$calls[[id]])) {
      path <- c(path, id)
    }

    id <- parents[id]
  }

  # Always include very first call
  path <- rev(path)
  if (length(path) && path[[1]] != 1L) {
    path <- c(1L, path)
  }

  trace$parents <- parents
  trace_subset(trace, path)
}

# Bypass calls with inlined functions
is_uninformative_call <- function(call) {
  fn <- call[[1]]

  # Inlined functions occur with active bindings
  if (is_function(fn)) {
    return(TRUE)
  }

  # If a call, might be wrapped in parentheses
  while (is_call(fn, "(")) {
    fn <- fn[[2]]
    if (is_call(fn, "function")) {
      return(TRUE)
    }
  }

  FALSE
}

trace_simplify_collapse <- function(trace) {
  parents <- trace$parents
  path <- int()
  id <- length(parents)

  while (id > 0L) {
    n_collapsed <- n_collapsed(trace, id)

    if (n_collapsed) {
      trace <- set_trace_collapsed(trace, id, n_collapsed)
      next_id <- id - n_collapsed

      # Rechain child of collapsed parent to correct parent
      parents[[id + 1L]] <- next_id

      id <- next_id
    }

    path <- c(path, id)
    parent_id <- parents[[id]]
    id <- dec(id)

    # Collapse intervening call branches
    n_skipped <- 0L
    while (id != parent_id) {
      sibling_parent_id <- parents[[id]]

      if (sibling_parent_id == parent_id) {
        trace <- set_trace_skipped(trace, id, n_skipped)
        path <- c(path, id)
        n_skipped <- 0L
      } else {
        n_skipped <- inc(n_skipped)
      }

      id <- dec(id)
    }
  }

  trace$parents <- parents
  trace_subset(trace, rev(path))
}


#' Last `abort()` error
#'
#' This returns the last error thrown with [abort()]. The error is
#' printed with a backtrace.
#'
#' @export
last_error <- function() {
  last_error_env$cnd
}


# Printing ----------------------------------------------------------------

trace_as_tree <- function(x, dir = getwd(), srcrefs = NULL) {
  nodes <- c(0, seq_along(x$calls))
  children <- map(nodes, function(id) seq_along(x$parents)[x$parents == id])

  calls <- as.list(x$calls)
  is_collapsed <- map(calls, attr, "collapsed")
  call_text <- map2_chr(calls, is_collapsed, trace_call_text)

  srcrefs <- srcrefs %||% peek_option("rlang_trace_format_srcrefs")
  srcrefs <- srcrefs %||% TRUE
  stopifnot(is_scalar_logical(srcrefs))
  if (srcrefs) {
    refs <- map(x$calls, attr, "srcref")
    src_locs <- map_chr(refs, src_loc, dir = dir)
    have_src_loc <- nzchar(src_locs)
    call_text[have_src_loc] <- paste0(call_text[have_src_loc], " ", src_locs[have_src_loc])
  }

  tree <- data.frame(id = as.character(nodes), stringsAsFactors = FALSE)
  tree$children <- map(children, as.character)
  tree$call <- c(trace_root(), call_text)

  tree
}

trace_call_text <- function(call, collapse) {
  if (is_null(collapse)) {
    return(expr_name(call))
  }

  if (is_call(call, "%>%")) {
    call <- call
  } else if (length(call) > 1L) {
    call <- call2(node_car(call), quote(...))
  }

  text <- expr_name(call)
  if (collapse > 0L) {
    n_collapsed_text <- sprintf(" ... +%d", collapse)
  } else {
    n_collapsed_text <- ""
  }

  format_collapsed(paste0("[ ", text, " ]"), collapse)

  ## sprintf("[ %s ]%s", text, n_collapsed_text)
}

src_loc <- function(srcref, dir = getwd()) {
  if (is.null(srcref)) {
    return("")
  }

  srcfile <- attr(srcref, "srcfile")
  if (is.null(srcfile)) {
    return("")
  }
  file <- srcfile$filename
  if (identical(file, "") || identical(file, "<text>")) {
    return("")
  }

  line <- srcref[[1]]
  column <- srcref[[5]] - 1L
  paste0(relish(file, dir = dir), ":", line, ":", column)
}

relish <- function(x, dir = getwd()) {
  if (substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }

  gsub(dir, "", x, fixed = TRUE)
}

trace_root <- function() {
  if (cli_is_utf8_output()) {
    "\u2588"
  } else {
    "x"
  }
}

# Misc --------------------------------------------------------------------

reprex_callstack <- function() {
  path <- tempfile(fileext = ".rds")

  code <- expr({
    f <- function() g()
    g <- function() h()
    h <- function() rlang::trace_back(globalenv())

    x <- try(identity(f()))
    saveRDS(x, !!path)
  })

  reprex <- getExportedValue("reprex", "reprex")
  reprex(input = expr_deparse(code), outfile = NULL, show = FALSE)

  readRDS(path)
}
