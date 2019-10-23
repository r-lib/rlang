#' Capture a backtrace
#'
#' A backtrace captures the sequence of calls that lead to the current
#' function, sometimes called the call stack. Because of lazy
#' evaluation, the call stack in R is actually a tree, which the
#' `summary()` method of this object will reveal.
#'
#' `trace_length()` returns the number of frames in a backtrace.
#'
#' @param top The first frame environment to be included in the
#'   backtrace. This becomes the top of the backtrace tree and
#'   represents the oldest call in the backtrace.
#'
#'   This is needed in particular when you call `trace_back()`
#'   indirectly or from a larger context, for example in tests or
#'   inside an RMarkdown document where you don't want all of the
#'   knitr evaluation mechanisms to appear in the backtrace.
#' @param bottom The last frame environment to be included in the
#'   backtrace. This becomes the rightmost leaf of the backtrace tree
#'   and represents the youngest call in the backtrace.
#'
#'   Set this when you would like to capture a backtrace without the
#'   capture context.
#'
#'   Can also be an integer that will be passed to [caller_env()].
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
#'
#' # Restore default top env for next example
#' options(rlang_trace_top_env = NULL)
#'
#' # When code is executed indirectly, i.e. via source or within an
#' # RMarkdown document, you'll tend to get a lot of guff at the beginning
#' # related to the execution environment:
#' conn <- textConnection("summary(f())")
#' source(conn, echo = TRUE, local = TRUE)
#' close(conn)
#'
#' # To automatically strip this off, specify which frame should be
#' # the top of the backtrace. This will automatically trim off calls
#' # prior to that frame:
#' top <- current_env()
#' h <- function() trace_back(top)
#'
#' conn <- textConnection("summary(f())")
#' source(conn, echo = TRUE, local = TRUE)
#' close(conn)
#' @export
trace_back <- function(top = NULL, bottom = NULL) {
  frames <- sys.frames()
  idx <- trace_find_bottom(bottom, frames)

  frames <- frames[idx]
  parents <- sys.parents()[idx]
  calls <- as.list(sys.calls()[idx])

  calls <- map(calls, call_fix_car)
  calls <- add_pipe_pointer(calls, frames)
  calls <- map2(calls, seq_along(calls), maybe_add_namespace)

  parents <- normalise_parents(parents)
  ids <- map_chr(frames, env_label)

  trace <- new_trace(calls, parents, ids)
  trace <- trace_trim_env(trace, top)

  trace
}

trace_find_bottom <- function(bottom, frames) {
  if (is_null(bottom)) {
    return(seq_len(sys.parent(2L)))
  }

  if (is_environment(bottom)) {
    top <- detect_index(frames, is_reference, bottom)
    if (!top) {
      if (is_reference(bottom, global_env())) {
        return(int())
      }
      abort("Can't find `bottom` on the call tree")
    }

    return(seq_len(top))
  }

  if (is_integerish(bottom, n = 1)) {
    return(seq_len(sys.parent(bottom + 1L)))
  }

  abort("`bottom` must be `NULL`, a frame environment, or an integer")
}

# Work around R bug causing promises to leak in frame calls
call_fix_car <- function(call) {
  if (typeof(node_car(call)) == "promise") {
    node_poke_car(call, eval_bare(node_car(call)))
  }
  call
}

# Assumes magrittr 1.5
add_pipe_pointer <- function(calls, frames) {
  pipe_begs <- which(map_lgl(calls, is_call2, "%>%"))
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

    frame <- frames[[beg + 5L]]
    pointer <- frame[[v]]

    fn <- frame[["function_list"]][[1]]
    info <- pipe_collect_calls(call, fn_env(fn))

    structure(call, pipe_pointer = pointer, pipe_info = info)
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

maybe_add_namespace <- function(call, fn) {
  if (is_quosure(call)) {
    call <- quo_get_expr(call)
  }

  if (call_print_fine_type(call) != "call") {
    return(call)
  }

  # Checking for bare symbols covers the `::` and `:::` cases
  sym <- node_car(call)
  if (!is_symbol(sym)) {
    return(call)
  }

  nm <- as_string(sym)
  if (is_environment(fn)) {
    fn <- get(nm, envir = fn, mode = "function")
  } else if (is_function(fn)) {
    fn <- fn
  } else {
    fn <- sys.function(fn)
  }

  env <- fn_env(fn)
  top <- topenv(env)
  if (is_reference(env, global_env())) {
    prefix <- "global"
    op <- "::"
  } else if (is_namespace(top)) {
    prefix <- ns_env_name(top)
    op <- if (nm %in% ns_exports(top)) "::" else ":::"
  } else {
    return(call)
  }

  namespaced_sym <- call(op, sym(prefix), sym)
  call[[1]] <- namespaced_sym
  call
}

# Remove recursive frames which occur with quosures
normalise_parents <- function(parents) {
  recursive <- parents == seq_along(parents)
  parents[recursive] <- 0L
  parents
}

new_trace <- function(calls, parents, ids, indices = NULL) {
  indices <- indices %||% seq_along(calls)

  n <- length(calls)
  stopifnot(
    is_list(calls),
    is_integer(parents, n),
    is_integer(indices, n)
  )

  structure(
    list(
      calls = calls,
      parents = parents,
      ids = ids,
      indices = indices
    ),
    class = "rlang_trace"
  )
}

trace_reset_indices <- function(trace) {
  trace$indices <- seq_len(trace_length(trace))
  trace
}

# Methods -----------------------------------------------------------------

# For internal use only
c.rlang_trace <- function(...) {
  traces <- list(...)

  calls <- flatten(map(traces, `[[`, "calls"))
  parents <- flatten_int(map(traces, `[[`, "parents"))
  ids <- flatten_chr(map(traces, `[[`, "ids"))
  indices <- flatten_int(map(traces, `[[`, "indices"))

  new_trace(calls, parents, ids, indices)
}

#' @export
format.rlang_trace <- function(x,
                               ...,
                               simplify = c("none", "collapse", "branch"),
                               max_frames = NULL,
                               dir = getwd(),
                               srcrefs = NULL) {
  x <- trace_reset_indices(x)

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
  if (!trace_length(trace)) {
    return(trace_root())
  }

  tree <- trace_as_tree(trace, dir = dir, srcrefs = srcrefs)
  cli_tree(tree, indices = trace$indices)
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
  cli_branch(branch, max = max_frames, indices = trace$indices)
}

format_collapsed <- function(what, n) {
  if (n > 0L) {
    call_text <- pluralise_n(n, "call", "calls")
    n_text <- sprintf(" with %d more %s", n, call_text)
    n_text <- silver(n_text)
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

cli_branch <- function(lines, max = NULL, style = NULL, indices = NULL) {
  if (!length(lines)) {
    return(chr())
  }

  numbered <- length(indices)
  if (numbered) {
    indices <- pad_spaces(as.character(indices))
    indices <- paste0(" ", indices, ". ")
    padding <- nchar(indices[[1]])
    lines <- paste0(silver(indices), lines)
  } else {
    style <- style %||% cli_box_chars()
    lines <- paste0(" ", style$h, lines)
  }

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

  if (numbered) {
    collapsed_line <- paste0(spaces(padding), "...")
  } else {
    collapsed_line <- format_collapsed_trail("...", n_collapsed, style = style)
  }

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
                              simplify = c("none", "branch", "collapse"),
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
#' @export
summary.rlang_trace <- function(object,
                                ...,
                                max_frames = NULL,
                                dir = getwd(),
                                srcrefs = NULL) {
  cat_line(format(object, ...,
    simplify = "none",
    max_frames = max_frames,
    dir = dir,
    srcrefs = srcrefs
  ))
  invisible(object)
}

#' @rdname trace_back
#' @param trace A backtrace created by `trace_back()`.
#' @export
trace_length <- function(trace) {
  length(trace$calls)
}

trace_subset <- function(x, i) {
  if (!length(i)) {
    return(new_trace(list(), int(), list()))
  }
  stopifnot(is_integerish(i))

  n <- trace_length(x)

  if (all(i < 0L)) {
    i <- setdiff(seq_len(n), abs(i))
  }

  parents <- match(as.character(x$parents[i]), as.character(i), nomatch = 0)

  new_trace(
    calls = x$calls[i],
    parents = parents,
    ids = x$ids[i],
    indices = x$indices[i]
  )
}

# Subsets sibling nodes, at the level of the rightmost leaf by
# default. Supports full vector subsetting semantics (negative values,
# missing index, etc).
trace_subset_across <- function(trace, i, n = NULL) {
  level <- trace_level(trace, n)
  level_n <- length(level)
  i <- validate_index(i, level_n)

  indices <- unlist(map(level[i], chain_indices, trace$parents))
  trace_subset(trace, indices)
}
trace_level <- function(trace, n = NULL) {
  n <- n %||% trace_length(trace)
  parents <- trace$parents
  which(parents == parents[[n]])
}

chain_indices <- function(i, parents) {
  c(
    parents_indices(i, parents),
    children_indices(i, parents)
  )
}
children_indices <- function(i, parents) {
  n <- length(parents)
  age <- parents[[i]]
  ages <- parents[1:n]

  non_children <- parents <= age
  non_children[seq(1, i)] <- FALSE
  non_children <- which(non_children)

  if (length(non_children)) {
    end <- non_children[[1]] - 1
  } else {
    end <- n
  }

  seq2(i + 1L, end)
}
parents_indices <- function(i, parents) {
  path <- int()

  while (i != 0) {
    path <- c(path, i)
    i <- parents[i]
  }

  rev(path)
}


# Trimming ----------------------------------------------------------------

trace_trim_env <- function(x, to = NULL) {
  to <- to %||% peek_option("rlang_trace_top_env")

  if (is.null(to)) {
    return(x)
  }

  is_top <- x$ids == env_label(to)
  if (!any(is_top)) {
    return(x)
  }

  start <- last(which(is_top)) + 1
  end <- length(x$ids)

  trace_subset(x, seq2(start, end))
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

  if (is_eval_call(call)) {
    # When error occurs inside eval()'s frame at top level, there
    # might be only one frame and nothing to collapse
    if (id > 1L && is_eval_call(trace$calls[[id - 1L]])) {
      n <- 1L
    } else {
      n <- 0L
    }
    return(n)
  }

  if (identical(call, quote(function_list[[i]](value)))) {
    return(6L)
  }

  if (identical(call, quote(function_list[[k]](value)))) {
    return(7L)
  }

  0L
}

is_eval_call <- function(call) {
  is_call2(call, c("eval", "evalq"), ns = c("", "base"))
}

pipe_collect_calls <- function(pipe, env) {
  node <- node_cdr(pipe)

  last_call <- pipe_add_dot(node_cadr(node))
  last_call <- maybe_add_namespace(last_call, env)

  calls <- new_node(last_call, NULL)

  while (is_call2(node_car(node), "%>%")) {
    node <- node_cdar(node)
    call <- pipe_add_dot(node_cadr(node))
    call <- maybe_add_namespace(call, env)
    calls <- new_node(call, calls)
  }

  first_call <- node_car(node)
  if (is_call2(first_call)) {
    # The first call doesn't need a dot
    first_call <- maybe_add_namespace(first_call, env)
    calls <- new_node(first_call, calls)
    leading <- TRUE
  } else {
    leading <- FALSE
  }

  list(calls = as.list(calls), leading = leading)
}
pipe_add_dot <- function(call) {
  if (!is_call2(call)) {
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

    pointer <- attr(pipe, "pipe_pointer")
    if (!is_scalar_integer(pointer)) {
      stop("Internal error: Invalid pipe pointer")
    }

    pipe_info <- attr(pipe, "pipe_info")
    pipe_calls <- pipe_info$calls

    if (pipe_info$leading) {
      pointer <- inc(pointer)
    }

    incomplete <- seq2(pointer + 1L, length(pipe_calls))
    if (length(incomplete)) {
      pipe_calls <- pipe_calls[-incomplete]
    }

    parent <- trace$parents[[idx]]
    pipe_parents <- seq(parent, parent + pointer - 1L)

    # Assign the pipe frame as dummy ids for uncollapsed frames
    pipe_ids <- rep(trace$ids[idx], pointer)

    # Add the number of uncollapsed frames to children's
    # ancestry. This assumes a backtrace branch.
    trace_after$parents <- trace_after$parents + pointer

    trace$calls <- c(trace_before$calls, pipe_calls, trace_after$calls)
    trace$parents <- c(trace_before$parents, pipe_parents, trace_after$parents)
    trace$ids <- c(trace_before$ids, pipe_ids, trace$ids)
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
  if (!is_call2(call)) {
    return(FALSE)
  }

  fn <- call[[1]]

  # Inlined functions occur with active bindings
  if (is_function(fn)) {
    return(TRUE)
  }

  # If a call, might be wrapped in parentheses
  while (is_call2(fn, "(")) {
    fn <- fn[[2]]
    if (is_call2(fn, "function")) {
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
    src_locs <- silver(src_locs[have_src_loc])
    call_text[have_src_loc] <- paste0(call_text[have_src_loc], " ", src_locs)
  }

  tree <- data.frame(id = as.character(nodes), stringsAsFactors = FALSE)
  tree$children <- map(children, as.character)
  tree$call <- c(trace_root(), call_text)

  tree
}

# FIXME: Add something like call_deparse_line()
trace_call_text <- function(call, collapse) {
  if (is_null(collapse)) {
    return(as_label(call))
  }

  if (is_call2(call, "%>%")) {
    call <- call
  } else if (length(call) > 1L) {
    call <- call2(node_car(call), quote(...))
  }

  text <- as_label(call)
  if (collapse > 0L) {
    n_collapsed_text <- sprintf(" ... +%d", collapse)
  } else {
    n_collapsed_text <- ""
  }

  format_collapsed(paste0("[ ", text, " ]"), collapse)
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
  if (!file.exists(file) && is_null(peek_option("rlang_trace__force_dangling_srcrefs"))) {
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
