# TODO:
# * environments -> names
# * Add tests
# * Finish rewrite of cli::tree()
# * Trim to include all top-level children
# * Add options to print (and hence to lobstr::cst)
# * trim_self to remove calltrace()
# * check printing + subsetting for empty tree

#' @examples
#'
#'
#' x <- NULL
#' try(with(mtcars, {x <<- try(f()); 10}))
#' x
#' trim_trailing(x)
#'
#' rmd <- reprex_callstack()
#' rmd
#' trim_env(rmd)
#' trim_trailing(rmd)
#'
#' j <- function(i) k(i)
#' k <- function(i) l(i)
#' l <- function(i) eval(quote(calltrace()), parent.frame(i))
#'
#' trim_trailing(j(1))
#' trim_trailing(j(2))
#' trim_trailing(j(3))
#'
#' @export
calltrace <- function(scope = caller_env()) {
  calls <- sys.calls()
  parents <- sys.parents()
  envs <- lapply(sys.frames(), env_label)

  funs <- lapply(1:sys.nframe(), sys.function)
  refs <- lapply(funs, attr, "srcref")

  new_calltrace(calls, parents, envs, refs)
}

reprex_callstack <- function() {
  path <- tempfile(fileext = ".rds")

  code <- expr({
    f <- function() g()
    g <- function() h()
    h <- function() rlang::calltrace()

    x <- try(identity(f()))
    saveRDS(x, !!path)
  })

  reprex <- getExportedValue("reprex", "reprex")
  reprex(input = expr_deparse(code), outfile = NULL, show = FALSE)

  readRDS(path)
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

src_loc <- function(x) {
  if (is.null(x))
    return("")

  srcfile <- attr(x, "srcfile")
  if (is.null(srcfile)) {
    return("")
  }
  file <- srcfile$filename
  if (identical(file, "")) {
    return("")
  }

  if (length(srcref) == 6L)
    srcref <- c(srcref, srcref[c(1L, 3L)])

  paste0(relish(file), ":", x[[1]], ":", x[[5]])
}

relish <- function(x, dir = getwd()) {
  if (substr(dir, nchar(dir), nchar(dir)) != "/") {
    dir <- paste0(dir, "/")
  }

  gsub(dir, "", x, fixed = TRUE)
}

#' @export
print.calltrace <- function(x) {
  tree <- as_tree(x)
  print(cli::tree(tree))

  invisible(x)
}

#' @export
`[.calltrace` <- function(x, i, ...) {
  stopifnot(is.integer(i))

  calls <- x$calls[i]
  envs <- x$envs[i]
  parents <- match(as.character(x$parents[i]), as.character(i), nomatch = 0)
  refs <- x$refs[i]

  new_calltrace(calls, parents, envs, refs)
}

as_tree <- function(x) {
  nodes <- c(0, seq_along(x$calls))
  children <- lapply(nodes, function(id) seq_along(x$parents)[x$parents == id])

  call_text <- vapply(as.list(x$calls), expr_name, character(1))
  src_loc <- vapply(x$refs, src_loc, character(1))
  call_text <- paste0(call_text, " ", src_loc)

  tree <- data.frame(id = as.character(nodes), stringsAsFactors = FALSE)
  tree$children <- lapply(children, as.character)
  tree$call <- c("\u2588", call_text)

  tree
}

# Find all children after specified environment
trim_env <- function(x, topenv = globalenv()) {
  is_top <- x$envs == env_label(topenv)
  if (!any(is_top)) {
    return(x)
  }

  start <- last(which(is_top)) + 1
  end <- length(x$envs)

  x[start:end]
}

# Find all components of last branch
trim_trailing <- function(x) {
  path <- integer()
  id <- length(x$parents)

  while (id != 0) {
    path <- c(path, id)
    id <- x$parents[id]
  }

  x[rev(path)]
}


f <- function() g()
g <- function() h()
h <- function() calltrace()


# Tree --------------------------------------------------------------------

# Compared to cli::tree
# * doesn't respect width -
cat_tree <- function(id, children, label1, label2 = NULL) {
  style <- box_chars()

  res <- character()

  pt <- function(root, n = integer(), mx = integer()) {

    num_root <- match(root, data[[1]])

    level <- length(n) - 1
    prefix <- vcapply(seq_along(n), function(i) {
      if (n[i] < mx[i]) {
        if (i == length(n)) {
          paste0(style$j, style$h)
        } else {
          paste0(style$v, " ")
        }
      } else if (n[i] == mx[i] && i == length(n)) {
        paste0(style$l, style$h)
      } else {
        "  "
      }
    })

    res <<- c(res, paste0(paste(prefix, collapse = ""), labels[[num_root]]))

    children <- children[[num_root]]
    for (d in seq_along(children)) {
      pt(children[[d]], c(n, d), c(mx, length(children)))
    }
  }

  pt(root)

  if (!is.null(label2)) {
    res <- paste0(format(res), label2)
  }

  cat(res, sep = "\n")
}

