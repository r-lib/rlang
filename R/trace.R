#' Capture a backtrace
#'
#' @description
#' A backtrace captures the sequence of calls that lead to the current
#' function (sometimes called the call stack). Because of lazy
#' evaluation, the call stack in R is actually a tree, which the
#' `print()` method for this object will reveal.
#'
#' Users rarely need to call `trace_back()` manually. Instead,
#' signalling an error with [abort()] or setting up [global_entrace()]
#' is the most common way to create backtraces when an error is
#' thrown. Inspect the backtrace created for the most recent error
#' with [last_error()].
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
  # FIXME: Include this in the `trace_back()` UI?
  visible_bottom <- peek_option("rlang:::visible_bottom")

  frames <- sys.frames()

  idx <- trace_find_bottom(bottom, frames)
  visible_idx <- trace_find_bottom(visible_bottom, frames)

  visible_idx <- intersect(visible_idx, idx)
  is_visible <- seq_along(idx) %in% visible_idx

  frames <- frames[idx]
  parents <- sys.parents()[idx]
  calls <- as.list(sys.calls()[idx])

  calls <- map(calls, call_fix_car)
  calls <- map(calls, call_zap_inline)

  context <- map2(calls, seq_along(calls), call_trace_context)
  context <- inject(vec_rbind(empty_trace_context(), !!!context))

  parents <- normalise_parents(parents)

  trace <- new_trace(
    calls,
    parents,
    namespace = context$namespace,
    scope = context$scope,
    visible = is_visible
  )
  trace <- add_winch_trace(trace)
  trace <- trace_trim_env(trace, frames, top)

  trace
}

trace_find_bottom <- function(bottom,
                              frames,
                              arg = caller_arg(bottom),
                              call = caller_env()) {
  if (is_null(bottom)) {
    return(seq_len(sys.parent(2L)))
  }

  if (is_environment(bottom)) {
    top <- detect_index(frames, is_reference, bottom)
    if (!top) {
      if (is_reference(bottom, global_env())) {
        return(int())
      }
      msg <- sprintf(
        "Can't find %s on the call tree.",
        format_arg(arg)
      )
      abort(msg, call = call)
    }

    return(seq_len(top))
  }

  if (is_integerish(bottom, n = 1)) {
    if (bottom < 0) {
      msg <- sprintf(
        "%s must be a positive integer.",
        format_arg(arg)
      )
      abort(msg, call = call)
    }
    if (inherits(bottom, "AsIs")) {
      return(seq_len(bottom))
    } else {
      return(seq_len(sys.parent(bottom + 1L)))
    }
  }

  msg <- sprintf(
    "%s must be `NULL`, a frame environment, or an integer.",
    format_arg(arg)
  )
  abort(msg, call = call)
}

# Work around R bug causing promises to leak in frame calls
call_fix_car <- function(call) {
  if (typeof(node_car(call)) == "promise") {
    node_poke_car(call, eval_bare(node_car(call)))
  }
  call
}

call_trace_context <- function(call, fn) {
  if (is_quosure(call)) {
    call <- quo_get_expr(call)
    if (!is_call(call)) {
      return(trace_context())
    }
  }

  if (call_print_fine_type(call) != "call") {
    return(trace_context())
  }

  namespace <- call_ns(call)
  name <- call_name(call)

  if (is_null(name)) {
    return(trace_context())
  }

  if (!is_null(namespace)) {
    return(trace_context(
      namespace = namespace,
      scope = as_string(call[[1]][[1]])
    ))
  }

  if (is_environment(fn)) {
    fn <- get(name, envir = fn, mode = "function")
  } else if (is_function(fn)) {
    fn <- fn
  } else {
    fn <- sys.function(fn)
  }

  env <- fn_env(fn)
  top <- topenv(env)
  if (is_reference(env, global_env())) {
    namespace <- NA
    scope <- "global"
  } else if (is_namespace(top)) {
    namespace <- ns_env_name(top)
    if (ns_exports_has(top, name)) {
      scope <- "::"
    } else if (env_has(top, name)) {
      scope <- ":::"
    } else {
      scope <- "local"
    }
  } else {
    namespace <- NA
    scope <- NA
  }

  trace_context(
    namespace = namespace,
    scope = scope
  )
}

trace_context <- function(namespace = NA, scope = NA) {
  data_frame(
    namespace = namespace,
    scope = scope
  )
}
empty_trace_context <- function() {
  trace_context(chr(), chr())
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

new_trace <- function(call,
                      parent,
                      ...,
                      visible = TRUE,
                      namespace = na_chr,
                      scope = na_chr,
                      class = NULL) {
  new_trace0(
    call,
    parent,
    ...,
    visible = visible,
    namespace = namespace,
    scope = scope,
    class = c(class, "rlang_trace", "rlib_trace")
  )
}
new_trace0 <- function(call,
                       parent,
                       ...,
                       visible = TRUE,
                       namespace = NA,
                       scope = NA,
                       class = NULL) {
  if (is_pairlist(call)) {
    call <- as.list(call)
  }
  stopifnot(
    is_bare_list(call),
    is_bare_integer(parent)
  )

  df <- df_list(
    call = call,
    parent = parent,
    visible = visible,
    namespace = namespace,
    scope = scope,
    ...
  )
  new_data_frame(
    df,
    .class = c(class, "tbl"),
    version = 2L
  )
}


# Operations --------------------------------------------------------------

#' @rdname trace_back
#' @param trace A backtrace created by `trace_back()`.
#' @export
trace_length <- function(trace) {
  nrow(trace)
}

trace_slice <- function(trace, i) {
  i <- vec_as_location(i, trace_length(trace))

  parent <- match(trace$parent, i, nomatch = 0)

  out <- vec_slice(trace, i)
  out$parent <- parent[i]

  out
}

trace_bind <- function(...) {
  traces <- compact(list2(...))
  n <- length(traces)

  if (!every(traces, inherits, "rlang_trace")) {
    abort("`...` must contain backtraces.")
  }

  if (n == 0L) {
    return(new_trace(list(), int()))
  }
  if (n == 1L) {
    return(traces[[1]])
  }

  out <- reduce(traces, function(x, y) {
    if (identical(x$call, y$call)) {
      return(x)
    }
    y$parent <- y$parent + nrow(x)
    vec_rbind(as.data.frame(x), as.data.frame(y))
  })

  new_data_frame(out, .class = c("rlang_trace", "rlib_trace", "tbl"))
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
    none = trace_format_full(x, max_frames, dir, srcrefs),
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

trace_format_full <- function(trace, max_frames, dir, srcrefs) {
  if (length(trace$visible)) {
    trace$visible <- TRUE
  }
  trace_format(trace, max_frames, dir, srcrefs)
}
trace_format_collapse <- function(trace, max_frames, dir, srcrefs) {
  trace <- trace_simplify_collapse(trace)
  trace_format(trace, max_frames, dir, srcrefs)
}
trace_format_branch <- function(trace, max_frames, dir, srcrefs) {
  trace <- trace_simplify_branch(trace)
  tree <- trace_as_tree(trace, dir = dir, srcrefs = srcrefs)

  # Remove root in the branch view
  tree <- vec_slice(tree, -1)

  cli_branch(tree, max = max_frames)
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

cli_branch <- function(tree,
                       max = NULL,
                       style = NULL) {
  lines <- tree$call_text

  if (!length(lines)) {
    return(chr())
  }

  indices <- tree$id
  indices <- pad_spaces(as.character(indices))
  indices <- paste0(" ", indices, ". ")
  padding <- spaces(nchar(indices[[1]]))

  lines <- paste0(silver(indices), lines)

  src_locs <- tree$src_loc
  src_locs <- map_if(src_locs, nzchar, ~ paste0(padding, "  at ", .x))
  src_locs <- style_locs(src_locs)

  lines <- zip_chr(lines, src_locs)

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

  collapsed_line <- paste0(padding, "...")

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

style_locs <- function(locs) {
  chr(map_if(locs, nzchar, col_grey))
}
zip_chr <- function(xs, ys) {
  flatten_chr(map2(xs, ys, function(x, y) {
    if (nzchar(y)) {
      c(x, y)
    } else {
      x
    }
  }))
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
  idx <- trace_trim_env_idx(trace_length(x), frames, to)
  trace_slice(x, idx)
}
trace_trim_env_idx <- function(n, frames, to) {
  to <- to %||% peek_option("rlang_trace_top_env")

  # Trim knitr context if available
  if (is_null(to) && is_true(peek_option('knitr.in.progress'))) {
    to <- knitr::knit_global()
  }

  if (is.null(to)) {
    return(TRUE)
  }

  is_top <- map_lgl(frames, is_reference, to)
  if (!any(is_top)) {
    return(TRUE)
  }

  start <- last(which(is_top)) + 1L
  seq2(start, n)
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
  if (!trace_length(trace)) {
    return(trace)
  }

  parents <- trace$parent

  old_visible <- trace$visible
  visible <- rep_along(old_visible, FALSE)

  old_visible_loc <- which(old_visible)
  if (length(old_visible_loc)) {
    id <- last(old_visible_loc)
  } else {
    id <- 0L
  }

  trace$collapsed <- 0L

  while (id != 0L) {
    n_collapsed <- n_collapsed(trace, id)

    if (n_collapsed) {
      trace <- set_trace_collapsed(trace, id, n_collapsed)
      next_id <- id - n_collapsed
      id <- next_id
    }

    # Set `old_visible` to avoid uninformative calls in position 1 to
    # be included (see below)
    if (is_uninformative_call(trace$call[[id]])) {
      old_visible[[id]] <- FALSE
    } else if (old_visible[[id]]) {
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
  if (!trace_length(trace)) {
    return(trace)
  }

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

  trace$visible <- visible & old_visible
  trace$parent <- parents

  trace
}


# Printing ----------------------------------------------------------------

trace_as_tree <- function(trace, dir = getwd(), srcrefs = NULL) {
  if (is_null(trace$collapsed)) {
    trace$collapsed <- vec_recycle(0L, trace_length(trace))
  }
  call_text_data <- trace[c("call", "collapsed", "namespace", "scope")]
  call_text <- chr(!!!pmap(call_text_data, trace_call_text))

  srcrefs <- srcrefs %||% peek_option("rlang_trace_format_srcrefs") %||% TRUE
  stopifnot(is_scalar_logical(srcrefs))

  if (srcrefs) {
    refs <- map(trace$call, attr, "srcref")
    src_locs <- map_chr(refs, src_loc)
    trace$src_loc <- src_locs
  } else {
    trace$src_loc <- vec_recycle("", trace_length(trace))
  }

  id <- c(0, seq_along(trace$call))
  children <- map(id, function(id) seq_along(trace$parent)[trace$parent == id])

  root <- data_frame(
    call = list(NULL),
    parent = 0L,
    visible = TRUE,
    namespace = NA,
    scope = NA,
    src_loc = ""
  )
  trace <- vec_rbind(root, trace)

  tree_data <- data_frame(
    id = as.character(id),
    children = children,
    call_text = c(trace_root(), call_text)
  )
  tree <- vec_cbind(tree_data, trace)

  # Subset out hidden frames
  tree <- vec_slice(tree, tree$visible)
  tree$children <- map(tree$children, intersect, tree$id)

  if (has_crayon()) {
    # Detect runs of namespaces/global
    ns <- tree$namespace
    ns <- ifelse(is.na(ns) & tree$scope == "global", "global", ns)
    ns[[1]] <- "_root"
    starts <- detect_run_starts(ns)

    # Embolden first occurrences in runs of namespaces/global
    tree$call_text <- map2_chr(tree$call_text, starts, function(text, start) {
      if (is_true(start)) {
        text <- sub(
          "^([a-zA-Z]+)(::|:::| )",
          sprintf("%s\\1%s\\2", open_bold(), close_bold()),
          text
        )
      }
      text
    })
  }

  tree
}

# FIXME: Add something like call_deparse_line()
trace_call_text <- function(call, collapsed, namespace, scope) {
  if (collapsed && length(call) > 1L) {
    call <- call2(call[[1]], quote(...))
  }

  if (is_call(call) && is_symbol(call[[1]])) {
    if (scope %in% c("::", ":::") && !is_na(namespace)) {
      call[[1]] <- call(scope, sym(namespace), call[[1]])
    }
  }

  text <- as_label(call)

  if (is_string(scope, "global")) {
    text <- paste0("global ", text)
  } else if (is_string(scope, "local") && !is_na(namespace)) {
    text <- paste0(namespace, " ", text)
  }

  if (collapsed) {
    n_collapsed_text <- sprintf(" ... +%d", collapsed)
    text <- format_collapsed(paste0("[ ", text, " ]"), collapsed)
  }

  text
}

src_loc <- function(srcref) {
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

  namespaced_sym <- call(op, sym(prefix), sym)
  call[[1]] <- namespaced_sym
  call
}

is_trace <- function(x) {
  inherits_any(x, c("rlang_trace", "rlib_trace"))
}

#' Backtrace specification
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#'
#' @section Structure:
#'
#' An r-lib backtrace is a data frame that contains the following
#' columns:
#'
#' - `call`: List of calls. These may carry `srcref` objects.
#'
#' - `visible`: Logical vector. If `FALSE`, the corresponding call
#'   will be hidden from simplified backtraces.
#'
#' - `parent`: Integer vector of parent references (see
#'   [sys.parents()]) as row numbers. 0 is global.
#'
#' - `namespace`: Character vector of namespaces. `NA` for global or
#'   no namespace
#'
#' - `scope`: Character vector of strings taking values `"::"`,
#'   `":::"`, `"global"`, or `"local"`.
#'
#' A backtrace data frame may contain extra columns. If you add
#' additional columns, make sure to prefix their names with the name
#' of your package or organisation to avoid potential conflicts with
#' future extensions of this spec, e.g. `"mypkg_column"`.
#'
#'
#' @section Operations:
#'
#' - **Length**. The length of the backtrace is the number of rows of
#'   the underlying data.
#'
#' - **Concatenation**. Performed by row-binding two backtraces.  The
#'   `parent` column of the RHS is shifted by `nrow(LHS)` so that the
#'   last call of the LHS takes place of the global frame of the RHS.
#'
#' - **Subsetting**. Performed by slicing the backtrace. After the
#'   data frame is sliced, the `parent` column is adjusted to the new
#'   row indices. Any `parent` value that no longer exists in the
#'   sliced backtrace is set to 0 (the global frame).
#'
#'
#' @name rlib_trace_spec
#' @keywords internal
NULL
