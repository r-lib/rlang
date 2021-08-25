#' Capture a backtrace
#'
#' A backtrace captures the sequence of calls that lead to the current
#' function, sometimes called the call stack. Because of lazy
#' evaluation, the call stack in R is actually a tree, which the
#' `print()` method for this object will reveal.
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
#'
#'   If not supplied, the `rlang_trace_top_env` global option is
#'   consulted. This makes it possible to trim the embedding context
#'   for all backtraces created while the option is set. If knitr is
#'   in progress, the default value for this option is
#'   `knitr::knit_global()` so that the knitr context is trimmed out
#'   of backtraces.
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

  # FIXME: namespace should be added at print time
  calls <- map2(calls, seq_along(calls), maybe_add_namespace)

  parents <- normalise_parents(parents)

  trace <- new_trace(calls, parents)
  trace <- add_winch_trace(trace)
  trace <- trace_trim_env(trace, frames, top)

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

maybe_add_namespace <- function(call, fn) {
  if (is_quosure(call)) {
    call <- quo_get_expr(call)
    if (!is_call(call)) {
      return(call)
    }
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
    if (ns_exports_has(top, nm)) {
      op <- "::"
    } else {
      op <- ":::"
    }
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

# Can't use new_environment() here
winch_available_env <- new.env(parent = emptyenv())

add_winch_trace <- function(trace) {
  avail <- winch_available_env$installed
  if (is_null(avail)) {
    avail <- rlang::is_installed("winch")
    winch_available_env$installed <- avail
  }

  if (!avail) {
    return(trace)
  }

  use_winch <- peek_option("rlang_trace_use_winch") %||% FALSE
  if (!is_true(as.logical(use_winch))) {
    return(trace)
  }

  winch::winch_add_trace_back(trace)
}


# Construction ------------------------------------------------------------

new_trace <- function(calls,
                      parents,
                      ...,
                      visible = NULL,
                      class = NULL) {
  new_trace0(
    calls,
    parents,
    ...,
    visible = visible,
    class = c(class, "rlang_trace", "rlib_trace")
  )
}
new_trace0 <- function(calls,
                       parents,
                       ...,
                       visible = NULL,
                       class = NULL) {
  stopifnot(
    is_bare_list(calls),
    is_bare_integer(parents)
  )

  df <- df_list(
    call = calls,
    parent = parents,
    visible = visible %||% TRUE,
    ...
  )
  new_data_frame(df, .class = c(class, "tbl"))
}


# Operations --------------------------------------------------------------

#' @rdname trace_back
#' @param trace A backtrace created by `trace_back()`.
#' @export
trace_length <- function(trace) {
  nrow(trace)
}

trace_slice <- function(trace, i) {
  # FIXME: Needs a compat
  i <- vctrs::vec_as_location(i, trace_length(trace))

  parent <- match(trace$parent, i, nomatch = 0)

  out <- vec_slice(trace, i)
  out$parent <- parent[i]

  out
}


# Methods -----------------------------------------------------------------

# For internal use only
c.rlang_trace <- function(...) {
  traces <- list(...)

  calls <- flatten(map(traces, `[[`, "call"))
  parents <- flatten_int(map(traces, `[[`, "parent"))

  new_trace(calls, parents)
}

#' @export
format.rlang_trace <- function(x,
                               ...,
                               simplify = c("none", "collapse", "branch"),
                               max_frames = NULL,
                               dir = getwd(),
                               srcrefs = NULL) {
  switch(
    arg_match(simplify),
    none = trace_format(x, max_frames, dir, srcrefs),
    collapse = trace_format_collapse(x, max_frames, dir, srcrefs),
    branch = trace_format_branch(x, max_frames, dir, srcrefs)
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

  cli_tree(trace_as_tree(trace, dir = dir, srcrefs = srcrefs))
}

trace_format_collapse <- function(trace, max_frames, dir, srcrefs) {
  trace <- trace_simplify_collapse(trace)
  trace_format(trace, max_frames, dir, srcrefs)
}
trace_format_branch <- function(trace, max_frames, dir, srcrefs) {
  trace <- trace_simplify_branch(trace)
  tree <- trace_as_tree(trace, dir = dir, srcrefs = srcrefs)

  # Remove root in the branch view
  tree <- tree[-1, ]

  cli_branch(tree$call, max = max_frames, indices = tree$id)
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
format_collapsed_branch <- function(what, n, style = NULL) {
  style <- style %||% cli_box_chars()
  what <- sprintf(" %s %s", style$h, what)
  format_collapsed(what, n)
}

cli_branch <- function(lines,
                       max = NULL,
                       style = NULL,
                       indices = seq_along(lines)) {
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
    collapsed_line <- format_collapsed_branch("...", n_collapsed, style = style)
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
  simplify <- arg_match(simplify)
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

# Subsets sibling nodes, at the level of the rightmost leaf by
# default. Supports full vector subsetting semantics (negative values,
# missing index, etc).
trace_subset_across <- function(trace, i, n = NULL) {
  level <- trace_level(trace, n)
  level_n <- length(level)
  i <- validate_index(i, level_n)

  indices <- unlist(map(level[i], chain_indices, trace$parent))
  trace_slice(trace, indices)
}
trace_level <- function(trace, n = NULL) {
  n <- n %||% trace_length(trace)
  parents <- trace$parent
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

trace_trim_env <- function(x, frames, to) {
  to <- to %||% peek_option("rlang_trace_top_env")

  # Trim knitr context if available
  if (is_null(to) && is_true(peek_option('knitr.in.progress'))) {
    to <- knitr::knit_global()
  }

  if (is.null(to)) {
    return(x)
  }

  is_top <- map_lgl(frames, is_reference, to)
  if (!any(is_top)) {
    return(x)
  }

  start <- last(which(is_top)) + 1L
  end <- trace_length(x)

  trace_slice(x, seq2(start, end))
}

set_trace_skipped <- function(trace, id, n) {
  trace$collapsed[[id]] <- n
  trace
}
set_trace_collapsed <- function(trace, id, n) {
  trace$collapsed[[id - n]] <- n
  trace
}
n_collapsed <- function(trace, id) {
  call <- trace$call[[id]]

  if (is_eval_call(call)) {
    # When error occurs inside eval()'s frame at top level, there
    # might be only one frame and nothing to collapse
    if (id > 1L && is_eval_call(trace$call[[id - 1L]])) {
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

trace_simplify_branch <- function(trace) {
  parents <- trace$parent

  old_visible <- trace$visible
  visible <- rep_along(old_visible, FALSE)

  id <- trace_length(trace)

  trace$collapsed <- 0L

  while (id != 0L) {
    n_collapsed <- n_collapsed(trace, id)

    if (n_collapsed) {
      trace <- set_trace_collapsed(trace, id, n_collapsed)
      next_id <- id - n_collapsed

      # Rechain child of collapsed parent to correct parent
      parents[[id + 1L]] <- next_id

      id <- next_id
    }

    if (old_visible[[id]] && !is_uninformative_call(trace$call[[id]])) {
      visible[[id]] <- TRUE
    }

    id <- parents[id]
  }

  # Always include first visible call
  first <- detect_index(old_visible, is_true)
  if (first) {
    visible[[first]] <- TRUE
  }

  trace$visible <- visible
  trace$parent <- parents

  trace
}

# Bypass calls with inlined functions
is_uninformative_call <- function(call) {
  if (!is_call2(call)) {
    return(FALSE)
  }

  fn <- call[[1]]

  if (is_winch_frame(fn)) {
    return(TRUE)
  }

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

# To be replaced with a more structured way of disabling frames in
# various displays
is_winch_frame <- function(call) {
  if (!is_call(call, "::")) {
    return(FALSE)
  }

  lhs <- call[[2]]
  if (!is_symbol(lhs)) {
    return(FALSE)
  }

  name <- as_string(lhs)
  grepl("^[/\\\\].+[.]", name)
}

trace_simplify_collapse <- function(trace) {
  parents <- trace$parent

  old_visible <- trace$visible
  visible <- rep_along(old_visible, FALSE)

  id <- trace_length(trace)

  trace$collapsed <- 0L

  while (id > 0L) {
    n_collapsed <- n_collapsed(trace, id)

    if (n_collapsed) {
      trace <- set_trace_collapsed(trace, id, n_collapsed)
      next_id <- id - n_collapsed

      # Rechain child of collapsed parent to correct parent
      parents[[id + 1L]] <- next_id

      id <- next_id
    }

    visible[[id]] <- TRUE
    parent_id <- parents[[id]]
    id <- dec(id)

    # Collapse intervening call branches
    n_skipped <- 0L
    while (id != parent_id) {
      sibling_parent_id <- parents[[id]]

      if (sibling_parent_id == parent_id) {
        trace <- set_trace_skipped(trace, id, n_skipped)
        visible[[id]] <- TRUE
        n_skipped <- 0L
      } else {
        n_skipped <- inc(n_skipped)
      }

      id <- dec(id)
    }
  }

  trace$visible <- visible
  trace$parent <- parents

  trace
}


# Printing ----------------------------------------------------------------

trace_as_tree <- function(trace, dir = getwd(), srcrefs = NULL) {
  id <- c(0, seq_along(trace$call))
  children <- map(id, function(id) seq_along(trace$parent)[trace$parent == id])

  collapsed <- trace$collapsed %||% 0L
  call_text <- map2_chr(trace$call, collapsed, trace_call_text)

  srcrefs <- srcrefs %||% peek_option("rlang_trace_format_srcrefs")
  srcrefs <- srcrefs %||% TRUE
  stopifnot(is_scalar_logical(srcrefs))
  if (srcrefs) {
    refs <- map(trace$call, attr, "srcref")
    src_locs <- map_chr(refs, src_loc, dir = dir)
    have_src_loc <- nzchar(src_locs)
    src_locs <- silver(src_locs[have_src_loc])
    call_text[have_src_loc] <- paste0(call_text[have_src_loc], " ", src_locs)
  }

  tree <- data.frame(id = as.character(id), stringsAsFactors = FALSE)
  tree$children <- map(children, as.character)
  tree$call <- c(trace_root(), call_text)

  # Subset out hidden frames
  tree <- vec_slice(tree, c(TRUE, trace$visible))
  tree$children <- map(tree$children, intersect, tree$id)

  tree
}

# FIXME: Add something like call_deparse_line()
trace_call_text <- function(call, collapse) {
  if (!collapse) {
    return(as_label(call))
  }

  if (is_call2(call, "%>%")) {
    call <- call
  } else if (length(call) > 1L) {
    call <- call2(node_car(call), quote(...))
  }

  text <- as_label(call)
  n_collapsed_text <- sprintf(" ... +%d", collapse)

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

  if (is_null(peek_option("rlang:::disable_trim_srcref"))) {
    file <- path_trim_prefix(file, 3)
  }

  line <- srcref[[1]]
  column <- srcref[[5]] - 1L

  paste0(file, ":", line, ":", column)
}

trace_root <- function() {
  if (cli_is_utf8_output()) {
    "\u2586"
  } else {
    "x"
  }
}

#' Trace functions from a set of packages
#'
#' @param pkgs One or more package names whose functions should be traced.
#' @param max_level Maximum nesting level of call stack.
#' @param regexp Regular expression passed to [base::grepl()] to
#'   select which functions should be traced. Can be a single string
#'   or a vector as long as `pkgs`.
#' @param ... These dots are for future extensions and should be empty.
#'
#' @author Iñaki Ucar (ORCID: 0000-0001-6403-5550)
#' @noRd
trace_pkgs <- function(pkgs, max_level = Inf, ..., regexp = NULL) {
  check_dots_empty()

  # Avoids namespace loading issues
  lapply(pkgs, requireNamespace, quietly = TRUE)

  trace_level <- 0

  # Create a thunk because `trace()` sloppily transforms functions into calls
  tracer <- call2(function() {
    trace_level <<- trace_level + 1

    if (trace_level > max_level) {
      return()
    }

    # Work around sys.foo() weirdness
    get_fn <- call2(function(fn = sys.function(sys.parent())) fn)
    fn <- eval(get_fn, envir = parent.frame())

    try(silent = TRUE, {
      call <- evalq(base::match.call(), envir = parent.frame())
      call <- call_add_namespace(call, fn)

      indent <- paste0(rep(" ", (trace_level - 1) * 2), collapse = "")
      line <- paste0(indent, as_label(call))
      cat(line, "\n")
    })
  })

  exit <- call2(function() {
    trace_level <<- trace_level - 1
  })

  if (length(regexp) == 1) {
    regexp <- rep_along(pkgs, regexp)
  }

  for (i in seq_along(pkgs)) {
    pkg <- pkgs[[i]]
    ns <- ns_env(pkg)
    ns_fns <- names(keep(as.list(ns), is.function))

    if (!is_null(regexp)) {
      ns_fns <- ns_fns[grepl(regexp[[i]], ns_fns)]
    }

    suppressMessages(trace(
      ns_fns,
      tracer = tracer,
      exit = exit,
      print = FALSE,
      where = ns
    ))

    message(sprintf(
      "Tracing %d functions in %s.",
      length(ns_fns),
      pkg
    ))
  }
}

call_add_namespace <- function(call, fn) {
  if (!is.call(call) || !is.symbol(call[[1]])) {
    return(call)
  }

  sym <- call[[1]]
  nm <- as_string(sym)

  if (nm %in% c("::", ":::")) {
    return(call)
  }

  env <- environment(fn)
  top <- topenv(env)

  if (is_reference(env, globalenv())) {
    prefix <- "global"
    op <- "::"
  } else if (is_namespace(top)) {
    prefix <- ns_env_name(top)
    if (ns_exports_has(top, nm)) {
      op <- "::"
    } else {
      op <- ":::"
    }
  } else {
    return(call)
  }

  namespaced_sym <- call(op, as.symbol(prefix), sym)
  call[[1]] <- namespaced_sym
  call
}

is_trace <- function(x) {
  inherits_any(x, c("rlang_trace", "rlib_trace"))
}
