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
#' print(x, simplify = "trail")
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
  calls <- sys.calls()
  parents <- normalise_parents(sys.parents())
  envs <- map(sys.frames(), env_label)

  trace <- new_trace(calls, parents, envs)
  trace <- trace_trim_env(trace, to)
  trace <- trace[-length(trace)] # remove call to self

  trace
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
                               simplify = c("collapse", "trail", "none"),
                               max_frames = NULL,
                               dir = getwd(),
                               srcrefs = NULL) {
  if (length(x) == 0) {
    return(trace_root())
  }

  x <- trace_simplify(x, simplify)
  tree <- trace_as_tree(x, dir = dir, srcrefs = srcrefs)

  if (arg_match(simplify) == "trail") {
    cli_branch(tree[-1, ][["call"]], max_frames)
  } else {
    if (!is_null(max_frames)) {
      abort("`max_frames` is currently only supported with `simplify = \"trail\"`")
    }
    cli_tree(tree)
  }
}

cli_branch <- function(lines, max = NULL, style = NULL) {
  style <- style %||% cli_box_chars()
  lines <- paste0(" ", style$h, lines)
  cli_branch_truncate(lines, max)
}

cli_branch_truncate <- function(lines, max = NULL) {
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

  if (max == 1L) {
    lines <- chr(
      lines[1L],
      " .",
      sprintf(" . +%s", n - max)
    )
    return(lines)
  }

  half <- max / 2L
  n_top <- ceiling(half)
  n_bottom <- floor(half)

  chr(
    lines[seq(1, n_top)],
    " .",
    sprintf(" . +%s", n - max),
    " .",
    lines[seq(n - n_bottom, n)]
  )
}


#' @export
print.rlang_trace <- function(x,
                              ...,
                              simplify = c("collapse", "trail", "none"),
                              max_frames = NULL,
                              dir = getwd(),
                              srcrefs = NULL) {
  meow(format(x, ...,
    simplify = simplify,
    max_frames = max_frames,
    dir = dir,
    srcrefs = srcrefs
  ))
  invisible(x)
}

#' @export
length.rlang_trace <- function(x) {
  length(x$calls)
}

#' @export
`[.rlang_trace` <- function(x, i, ...) {
  stopifnot(is_integerish(i))

  if (all(i < 0L)) {
    i <- setdiff(seq_along(x), abs(i))
  }

  calls <- x$calls[i]
  envs <- x$envs[i]
  parents <- match(as.character(x$parents[i]), as.character(i), nomatch = 0)

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

  x[start:end]
}

trace_simplify <- function(x, simplify = c("trail", "collapse", "none")) {
  switch(arg_match(simplify),
    none = x,
    trail = trace_simplify_trail(x),
    collapse = trace_simplify_collapsed(x)
  )
}

trace_simplify_trail <- function(trace) {
  parents <- trace$parents
  path <- int()
  id <- length(parents)

  while (id != 0L) {
    path <- c(path, id)
    id <- parents[id]
  }

  trace[rev(path)]
}

trace_simplify_collapsed <- function(trace) {
  parents <- trace$parents
  path <- int()
  id <- length(parents)

  while (id > 0L) {
    parent_id <- parents[[id]]
    path <- c(path, id)
    id <- id - 1L

    # Collapse intervening call branches
    if (id != parent_id) {
      n_skipped <- 0L

      while (id != parent_id) {
        sibling_parent_id <- parents[[id]]

        if (sibling_parent_id == parent_id) {
          attr(trace$calls[[id]], "collapsed") <- n_skipped
          n_skipped <- 0L
          path <- c(path, id)
        } else {
          n_skipped <- n_skipped + 1L
        }

        id <- id - 1L
      }
    }
  }

  trace[rev(path)]
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

  if (length(call) > 1L) {
    call <- call2(node_car(call), quote(...))
  }

  text <- expr_name(call)
  if (collapse > 0L) {
    n_collapsed_text <- sprintf(" ... +%d", collapse)
  } else {
    n_collapsed_text <- ""
  }

  sprintf("[ %s ]%s", text, n_collapsed_text)
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
