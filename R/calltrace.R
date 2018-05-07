#' Capture a call trace.
#'
#' A call trace captures sequence of calls that lead to the current function,
#' sometimes called the call stack. Because of lazy evaluation, the call
#' stack in R is actually a tree, which the print method of this object will
#' reveal.
#'
#' @param top_env If non-null, this will be used to automatically trim the
#'   call trace to only show calls after the last occurence of this enviroment.
#'   This is needed when the code that calls `calltree()` is run indirectly,
#'   for example in tests, or inside an RMarkdown document.
#' @examples
#' f <- function() g()
#' g <- function() h()
#' h <- function() calltrace()
#'
#' # When no lazy evaluation is involved the calltrack is linear
#' # (i.e. every call has only one child)
#' f()
#'
#' # Lazy evaluation introduces a tree like structure
#' identity(identity(f()))
#' identity(try(f()))
#' try(identity(f()))
#'
#' # When printing, you can request to simplify this tree to only show
#' # the direct sequence of calls that lead to `calltrace()`
#' x <- try(identity(f()))
#' x
#' print(x, simplify = TRUE)
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
#' source(textConnection("f()"), echo = TRUE)
#'
#' # To automatically strip this off, pass `top_env = globalenv()`
#' # That will automatically trim of calls prior to the last appearance
#' # of the global environment on the stack
#' h <- function() calltrace(globalenv())
#' source(textConnection("f()"), echo = TRUE)
#' @export
calltrace <- function(top_env = NULL) {
  calls <- sys.calls()
  parents <- sys.parents()
  envs <- map(sys.frames(), env_label)

  funs <- map(1:sys.nframe(), sys.function)
  refs <- map(funs, attr, "srcref")

  trace <- new_calltrace(calls, parents, envs, refs)
  trace <- trace_trim_env(trace, top_env)
  trace <- trace[-length(trace)] # remove call to self

  trace
}

new_calltrace <- function(calls, parents, envs, refs) {
  stopifnot(is.list(calls), is.integer(parents), length(calls) == length(parents))

  structure(
    list(
      calls = calls,
      parents = parents,
      envs = envs,
      refs = refs
    ),
    class = "calltrace"
  )
}

# Methods -----------------------------------------------------------------

#' @export
format.calltrace <- function(x, simplify = FALSE, dir = getwd(), ...) {
  if (length(x) == 0) {
    return(trace_root())
  }

  if (simplify) {
    x <- trace_simplify(x)
  }
  tree <- trace_as_tree(x, dir = dir)
  cli_tree(tree)
}

#' @export
print.calltrace <- function(x, simplify = FALSE, dir = getwd(), ...) {
  meow(format(x, ..., simplify = simplify, dir = dir))
  invisible(x)
}

#' @export
length.calltrace <- function(x) {
  length(x$calls)
}

#' @export
`[.calltrace` <- function(x, i, ...) {
  stopifnot(is.integer(i))

  if (all(i < 0L)) {
    i <- setdiff(seq_along(x), abs(i))
  }

  calls <- x$calls[i]
  envs <- x$envs[i]
  parents <- match(as.character(x$parents[i]), as.character(i), nomatch = 0)
  refs <- x$refs[i]

  new_calltrace(calls, parents, envs, refs)
}

# Trimming ----------------------------------------------------------------

trace_trim_env <- function(x, top_env = globalenv()) {
  if (is.null(top_env)) {
    return(x)
  }

  is_top <- x$envs == env_label(top_env)
  if (!any(is_top)) {
    return(x)
  }

  start <- last(which(is_top)) + 1
  end <- length(x$envs)

  x[start:end]
}

trace_simplify <- function(x) {
  path <- integer()
  id <- length(x$parents)

  while (id != 0L) {
    path <- c(path, id)
    id <- x$parents[id]
  }

  x[rev(path)]
}

# Printing ----------------------------------------------------------------

trace_as_tree <- function(x, dir = getwd()) {
  nodes <- c(0, seq_along(x$calls))
  children <- map(nodes, function(id) seq_along(x$parents)[x$parents == id])

  call_text <- map_chr(as.list(x$calls), expr_name)
  src_locs <- map_chr(x$refs, src_loc, dir = dir)
  call_text <- paste0(call_text, " ", src_locs)

  tree <- data.frame(id = as.character(nodes), stringsAsFactors = FALSE)
  tree$children <- map(children, as.character)
  tree$call <- c(trace_root(), call_text)

  tree
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

  paste0(relish(file, dir = dir), ":", srcref[[1]], ":", srcref[[5]])
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
    h <- function() rlang::calltrace(globalenv())

    x <- try(identity(f()))
    saveRDS(x, !!path)
  })

  reprex <- getExportedValue("reprex", "reprex")
  reprex(input = expr_deparse(code), outfile = NULL, show = FALSE)

  readRDS(path)
}
