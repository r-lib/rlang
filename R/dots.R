#' Collect dots tidily
#'
#' @description
#'
#' `list2()` is equivalent to `list(...)` but provides tidy
#' dots semantics:
#'
#' - You can splice other lists with the
#'   [unquote-splice][quasiquotation] `!!!` operator.
#'
#' - You can unquote names by using the [unquote][quasiquotation]
#'   operator `!!` on the left-hand side of `:=`.
#'
#' We call quasiquotation support in dots **tidy dots** semantics and
#' functions taking dots with `list2()` tidy dots functions.
#' Quasiquotation is an alternative to `do.call()` idioms and gives
#' the users of your functions an uniform syntax to supply a variable
#' number of arguments or a variable name.
#'
#' `dots_list()` is a lower-level version of `list2()` that offers
#' additional parameters for dots capture.
#'
#'
#' @details
#'
#' Note that while all tidy eval [quoting functions][quotation] have
#' tidy dots semantics, not all tidy dots functions are quoting
#' functions. `list2()` is for standard functions, not quoting
#' functions.
#'
#'
#' @section Life cycle:
#'
#' One difference of `dots_list()` with `list2()` is that it always
#' allocates a vector of names even if no names were supplied. In this
#' case, the names are all empty `""`. This is for consistency with
#' [enquos()] and [enexprs()] but can be quite costly when long lists
#' are spliced in the results. For this reason we plan to parameterise
#' this behaviour with a `.named` argument and possibly change the
#' default. `list2()` does not have this issue.
#'
#'
#' @param ... Arguments to collect with `!!!` support.
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty.
#' @param .preserve_empty Whether to preserve the empty arguments that
#'   were not ignored. If `TRUE`, empty arguments are stored with
#'   [missing_arg()] values. If `FALSE` (the default) an error is
#'   thrown when an empty argument is detected.
#' @param .homonyms How to treat arguments with the same name. The
#'   default, `"keep"`, preserves these arguments. Set `.homonyms` to
#'   `"first"` to only keep the first occurrences, to `"last"` to keep
#'   the last occurrences, and to `"error"` to raise an informative
#'   error and indicate what arguments have duplicated names.
#' @param .check_assign Whether to check for `<-` calls passed in
#'   dots. When `TRUE` and a `<-` call is detected, a warning is
#'   issued to advise users to use `=` if they meant to match a
#'   function parameter, or wrap the `<-` call in braces otherwise.
#'   This ensures assignments are explicit.
#' @return A list of arguments. This list is always named: unnamed
#'   arguments are named with the empty string `""`.
#'
#' @seealso [exprs()] for extracting dots without evaluation.
#' @name tidy-dots

#' @rdname tidy-dots
#' @export
#' @examples
#' # Let's create a function that takes a variable number of arguments:
#' numeric <- function(...) {
#'   dots <- list2(...)
#'   num <- as.numeric(dots)
#'   set_names(num, names(dots))
#' }
#' numeric(1, 2, 3)
#'
#' # The main difference with list(...) is that list2(...) enables
#' # the `!!!` syntax to splice lists:
#' x <- list(2, 3)
#' numeric(1, !!! x, 4)
#'
#' # As well as unquoting of names:
#' nm <- "yup!"
#' numeric(!!nm := 1)
#'
#'
#' # One useful application of splicing is to work around exact and
#' # partial matching of arguments. Let's create a function taking
#' # named arguments and dots:
#' fn <- function(data, ...) {
#'   list2(...)
#' }
#'
#' # You normally cannot pass an argument named `data` through the dots
#' # as it will match `fn`'s `data` argument. The splicing syntax
#' # provides a workaround:
#' fn("wrong!", data = letters)  # exact matching of `data`
#' fn("wrong!", dat = letters)   # partial matching of `data`
#' fn(some_data, !!!list(data = letters))  # no matching
#'
#'
#' # Empty arguments trigger an error by default:
#' try(fn(, ))
#'
#' # You can choose to preserve empty arguments instead:
#' list3 <- function(...) dots_list(..., .preserve_empty = TRUE)
#'
#' # Note how the last empty argument is still ignored because
#' # `.ignore_empty` defaults to "trailing":
#' list3(, )
#'
#' # The list with preserved empty arguments is equivalent to:
#' list(missing_arg())
#'
#'
#' # Arguments with duplicated names are kept by default:
#' list2(a = 1, a = 2, b = 3, b = 4, 5, 6)
#'
#' # Use the `.homonyms` argument to keep only the first of these:
#' dots_list(a = 1, a = 2, b = 3, b = 4, 5, 6, .homonyms = "first")
#'
#' # Or the last:
#' dots_list(a = 1, a = 2, b = 3, b = 4, 5, 6, .homonyms = "last")
#'
#' # Or raise an informative error:
#' try(dots_list(a = 1, a = 2, b = 3, b = 4, 5, 6, .homonyms = "error"))
#'
#'
#' # dots_list() can be configured to warn when a `<-` call is
#' # detected:
#' my_list <- function(...) dots_list(..., .check_assign = TRUE)
#' my_list(a <- 1)
#'
#' # There is no warning if the assignment is wrapped in braces.
#' # This requires users to be explicit about their intent:
#' my_list({ a <- 1 })
dots_list <- function(...,
                      .ignore_empty = c("trailing", "none", "all"),
                      .preserve_empty = FALSE,
                      .homonyms = c("keep", "first", "last", "error"),
                      .check_assign = FALSE) {
  dots <- .Call(rlang_dots_list,
    frame_env = environment(),
    named = FALSE,
    ignore_empty = .ignore_empty,
    preserve_empty = .preserve_empty,
    unquote_names = TRUE,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
  names(dots) <- names2(dots)
  dots
}

dots_split <- function(...,
                       .n_unnamed = NULL,
                       .ignore_empty = c("trailing", "none", "all"),
                       .preserve_empty = FALSE,
                       .homonyms = c("keep", "first", "last", "error"),
                       .check_assign = FALSE) {
  dots <- .Call(rlang_dots_list,
    frame_env = environment(),
    named = FALSE,
    ignore_empty = .ignore_empty,
    preserve_empty = .preserve_empty,
    unquote_names = TRUE,
    homonyms = .homonyms,
    check_assign = .check_assign
  )

  if (is_null(names(dots))) {
    if (length(dots)) {
      unnamed_idx <- TRUE
    } else {
      unnamed_idx <- lgl()
    }
    n <- length(dots)
  } else {
    unnamed_idx <- names(dots) == ""
    n <- sum(unnamed_idx)
  }

  if (!is_null(.n_unnamed) && all(n != .n_unnamed)) {
    ns <- chr_enumerate(.n_unnamed)
    abort(sprintf("Expected %s unnamed arguments in `...`", ns))
  }

  unnamed <- dots[unnamed_idx]
  named <- dots[!unnamed_idx]

  # Remove empty names vector
  names(unnamed) <- NULL

  list(named = named, unnamed = unnamed)
}

#' Splice lists
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("questioning")}
#'
#' - `splice` marks an object to be spliced. It is equivalent to using
#'   `!!!` in a function with [tidy dots semantics][tidy-dots].
#'
#' - `dots_splice()` is like [dots_list()] but automatically splices
#'   list inputs.
#'
#'
#' @section Standard splicing versus quoting splicing:
#'
#' The `!!!` operator works differently in _standard_ functions taking
#' dots with `dots_list()` than in _quoting_ functions taking dots
#' with [enexprs()] or [enquos()].
#'
#' * In quoting functions `!!!` disaggregates its argument (let's call
#'   it `x`) into as many objects as there are elements in
#'   `x`. E.g. `quo(foo(!!! c(1, 2)))` is completely equivalent to
#'   `quo(foo(1, 2))`. The creation of those separate objects has an
#'   overhead but is typically not important when manipulating calls
#'   because function calls typically take a small number of
#'   arguments.
#'
#' * In standard functions, disaggregating the spliced collection
#'   would have a negative performance impact in cases where
#'   `dots_list()` is used to build up data structures from user
#'   inputs. To avoid this spliced inputs are marked with [splice()]
#'   and the final list is built with (the equivalent of)
#'   `flatten_if(dots, is_spliced)`.
#'
#' Most of the time you should not care about the difference. However
#' if you use a standard function taking tidy dots within a quoting
#' function, the `!!!` operator will disaggregate its argument because
#' the behaviour of the quasiquoting function has priority. You might
#' then observe some performance cost in edge cases. Here is one
#' example where this would happen:
#'
#' ```
#' purrr::rerun(10, dplyr::bind_rows(!!! x))
#' ```
#'
#' `purrr::rerun()` is a quoting function and `dplyr::bind_rows()` is
#' a standard function. Because `bind_rows()` is called _inside_
#' `rerun()`, the list `x` will be disaggregated into a pairlist of
#' arguments. To avoid this you can use `splice()` instead:
#'
#' ```
#' purrr::rerun(10, dplyr::bind_rows(splice(x)))
#' ```
#'
#'
#' @section Life cycle:
#'
#' * `dots_splice()` is in the questioning stage. It is part of our
#'   experiments with dots semantics. Compared to `dots_list()`,
#'   `dots_splice()` automatically splices lists. We now lean towards
#'   adopting a single type of dots semantics (those of `dots_list()`)
#'   where splicing is explicit.
#'
#' * `splice()` is in the questioning stage. It is not clear whether it is
#'   really needed as there are other ways to avoid the performance
#'   issue discussed above.
#'
#'
#' @param x A list to splice.
#'
#' @keywords internal
#' @export
splice <- function(x) {
  .Call(rlang_new_splice_box, x)
}
#' @rdname splice
#' @export
is_spliced <- function(x) {
  .Call(rlang_is_splice_box, x)
}
#' @rdname splice
#' @export
is_spliced_bare <- function(x) {
  is_bare_list(x) || is_spliced(x)
}
#' @export
print.rlang_box_splice <- function(x, ...) {
  cat_line("<spliced>")
  print(unbox(x))
}

#' @rdname splice
#' @inheritParams tidy-dots
#' @export
dots_splice <- function(...,
                        .ignore_empty = c("trailing", "none", "all"),
                        .preserve_empty = FALSE,
                        .homonyms = c("keep", "first", "last", "error"),
                        .check_assign = FALSE) {
  dots <- .Call(rlang_dots_flat_list,
    frame_env = environment(),
    named = FALSE,
    ignore_empty = .ignore_empty,
    preserve_empty = .preserve_empty,
    unquote_names = TRUE,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
  names(dots) <- names2(dots)
  dots
}


#' Evaluate dots with preliminary splicing
#'
#' This is a tool for advanced users. It captures dots, processes
#' unquoting and splicing operators, and evaluates them. Unlike
#' [dots_list()], it does not flatten spliced objects, instead they
#' are attributed a `spliced` class (see [splice()]). You can process
#' spliced objects manually, perhaps with a custom predicate (see
#' [flatten_if()]).
#'
#' @inheritParams tidy-dots
#' @param ... Arguments to evaluate and process splicing operators.
#' @export
#' @examples
#' dots <- dots_values(!!! list(1, 2), 3)
#' dots
#'
#' # Flatten the objects marked as spliced:
#' flatten_if(dots, is_spliced)
dots_values <- function(...,
                        .ignore_empty = c("trailing", "none", "all"),
                        .preserve_empty = FALSE,
                        .homonyms = c("keep", "first", "last", "error"),
                        .check_assign = FALSE) {
  .External2(rlang_ext2_dots_values,
    named = FALSE,
    ignore_empty = .ignore_empty,
    preserve_empty = .preserve_empty,
    unquote_names = TRUE,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
}

# Micro optimisation: Inline character vectors in formals list
formals(dots_values) <- pairlist(
  ... = quote(expr = ),
  .ignore_empty = c("trailing", "none", "all"),
  .preserve_empty = FALSE,
  .homonyms = c("keep", "first", "last", "error"),
  .check_assign = FALSE
)

#' Capture definition objects
#'
#' @section Life cycle:
#'
#' `dots_definitions()` is experimental. Expect API changes.
#'
#' @inheritParams quotation
#'
#' @keywords internal
#' @export
dots_definitions <- function(...,
                             .named = FALSE,
                             .ignore_empty = c("trailing", "none", "all")) {
  dots <- .Call(rlang_quos_interp,
    frame_env = environment(),
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = FALSE,
    homonyms = "keep",
    check_assign = FALSE
  )

  is_def <- map_lgl(dots, function(dot) is_definition(quo_get_expr(dot)))
  defs <- map(dots[is_def], as_definition)

  list(dots = dots[!is_def], defs = defs)
}
as_definition <- function(def) {
  # The definition comes wrapped in a quosure
  env <- quo_get_env(def)
  def <- quo_get_expr(def)

  list(
    lhs = new_quosure(f_lhs(def), env),
    rhs = new_quosure(f_rhs(def), env)
  )
}

dots_node <- function(...) {
  node_cdr(sys.call())
}

#' How many arguments are currently forwarded in dots?
#'
#' This returns the number of arguments currently forwarded in `...`
#' as an integer.
#'
#' @param ... Forwarded arguments.
#' @export
#' @examples
#' fn <- function(...) dots_n(..., baz)
#' fn(foo, bar)
dots_n <- function(...) {
  nargs()
}

abort_dots_homonyms <- function(dots, dups) {
  nms <- names(dots)

  # This includes the first occurrence as well
  dups_all <- nms %in% nms[dups]

  dups_nms <- unique(nms[dups_all])
  dups_n <- length(dups_nms)

  if (!dups_n) {
    abort("Internal error: Expected dots duplicates")
  }

  if (dups_n == 1L) {
    nm <- dups_nms
    enum <- homonym_enum(nm, dups_all, nms)
    pos_msg <- sprintf(
      "We found multiple arguments named `%s` at positions %s",
      nm,
      enum
    )
    abort(paste_line(
      "Arguments can't have the same name.",
      pos_msg
    ))
  }

  enums <- map(dups_nms, homonym_enum, dups_all, nms)
  line <- "* Multiple arguments named `%s` at positions %s"
  enums_lines <- map2(dups_nms, enums, sprintf, fmt = line)

  abort(paste_line(
    "Arguments can't have the same name. We found these problems:",
    !!!enums_lines
  ))
}

homonym_enum <- function(nm, dups, nms) {
  dups[nms != nm] <- FALSE
  chr_enumerate(as.character(which(dups)), final = "and")
}

check_dots_empty <- function(...) {
  if (nargs()) {
    abort("These `...` must be empty")
  }
}
