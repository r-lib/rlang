#' Dynamic dots features
#'
#' @description
#'
#' The base `...` syntax supports:
#'
#' - __Forwarding__ arguments from function to function, matching them
#'   along the way to arguments.
#'
#' - __Collecting__ arguments inside data structures, e.g. with [c()] or
#'   [list()].
#'
#' Dynamic dots offer a few additional features,
#' [injection][topic-inject] in particular:
#'
#' 1. You can __splice__ arguments saved in a list with the splice
#'    operator [`!!!`][splice-operator].
#'
#' 2. You can __inject__ names with [glue syntax][glue-operators] on
#'    the left-hand side of `:=`.
#'
#' 3. Trailing commas are ignored, making it easier to copy and paste
#'    lines of arguments.
#'
#'
#' @section Add dynamic dots support in your functions:
#'
#' If your function takes dots, adding support for dynamic features is
#' as easy as collecting the dots with [list2()] instead of [list()].
#' See also [dots_list()], which offers more control over the collection.
#'
#' In general, passing `...` to a function that supports dynamic dots
#' causes your function to inherit the dynamic behaviour.
#'
#' In packages, document dynamic dots with this standard tag:
#'
#' ```
#'  @@param ... <[`dynamic-dots`][rlang::dyn-dots]> What these dots do.
#' ```
#'
#' @name dyn-dots
#' @aliases tidy-dots doc_dots_dynamic
#'
#' @examples
#' f <- function(...) {
#'   out <- list2(...)
#'   rev(out)
#' }
#'
#' # Trailing commas are ignored
#' f(this = "that", )
#'
#' # Splice lists of arguments with `!!!`
#' x <- list(alpha = "first", omega = "last")
#' f(!!!x)
#'
#' # Inject a name using glue syntax
#' if (is_installed("glue")) {
#'   nm <- "key"
#'   f("{nm}" := "value")
#'   f("prefix_{nm}" := "value")
#' }
NULL

#' @rdname dyn-dots
#' @usage NULL
#' @export
`:=` <- function(x, y) {
  abort("`:=` can only be used within dynamic dots.", call = caller_env())
}


#' Collect dynamic dots in a list
#'
#' `list2(...)` is equivalent to `list(...)` with a few additional
#' features, collectively called [dynamic dots][dyn-dots]. While
#' `list2()` hard-code these features, `dots_list()` is a lower-level
#' version that offers more control.
#'
#' @param ... Arguments to collect in a list. These dots are
#'   [dynamic][dyn-dots].
#' @return A list containing the `...` inputs.
#'
#' @export
list2 <- function(...) {
  .Call(
    ffi_dots_list,
    frame_env = environment(),
    named = NULL,
    ignore_empty = "trailing",
    preserve_empty = FALSE,
    unquote_names = TRUE,
    homonyms = "keep",
    check_assign = FALSE
  )
}
#' @rdname list2
#' @usage NULL
#' @export
ll <- list2

# Preserves empty arguments
list3 <- function(...) {
  .Call(
    ffi_dots_list,
    frame_env = environment(),
    named = NULL,
    ignore_empty = "trailing",
    preserve_empty = TRUE,
    unquote_names = TRUE,
    homonyms = "keep",
    check_assign = FALSE
  )
}


#' @rdname list2
#' @param .named If `TRUE`, unnamed inputs are automatically named
#'   with [as_label()]. This is equivalent to applying
#'   [exprs_auto_name()] on the result. If `FALSE`, unnamed elements
#'   are left as is and, if fully unnamed, the list is given minimal
#'   names (a vector of `""`). If `NULL`, fully unnamed results are
#'   left with `NULL` names.
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
#' @param .check_assign Whether to check for `<-` calls. When `TRUE` a
#'   warning recommends users to use `=` if they meant to match a
#'   function parameter or wrap the `<-` call in curly braces otherwise.
#'   This ensures assignments are explicit.
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
                      .named = FALSE,
                      .ignore_empty = c("trailing", "none", "all"),
                      .preserve_empty = FALSE,
                      .homonyms = c("keep", "first", "last", "error"),
                      .check_assign = FALSE) {
  .Call(
    ffi_dots_list,
    frame_env = environment(),
    named = .named,
    ignore_empty = .ignore_empty,
    preserve_empty = .preserve_empty,
    unquote_names = TRUE,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
}

dots_split <- function(...,
                       .n_unnamed = NULL,
                       .ignore_empty = c("trailing", "none", "all"),
                       .preserve_empty = FALSE,
                       .homonyms = c("keep", "first", "last", "error"),
                       .check_assign = FALSE) {
  dots <- .Call(
    ffi_dots_list,
    frame_env = environment(),
    named = NULL,
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
#' `r lifecycle::badge("questioning")`
#'
#' - `splice` marks an object to be spliced. It is equivalent to using
#'   `!!!` in a function taking [dynamic dots][dyn-dots].
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
  .Call(ffi_new_splice_box, x)
}
#' @rdname splice
#' @export
is_spliced <- function(x) {
  .Call(ffi_is_splice_box, x)
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
#' @inheritParams dots_list
#' @export
dots_splice <- function(...,
                        .ignore_empty = c("trailing", "none", "all"),
                        .preserve_empty = FALSE,
                        .homonyms = c("keep", "first", "last", "error"),
                        .check_assign = FALSE) {
  dots <- .Call(
    ffi_dots_flat_list,
    frame_env = environment(),
    named = NULL,
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
#' @inheritParams dots_list
#' @param ... Arguments to evaluate and process splicing operators.
#'
#' @keywords internal
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
  .External(
    ffi_dots_values,
    env = environment(),
    named = NULL,
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

#' How many arguments are currently forwarded in dots?
#'
#' This returns the number of arguments currently forwarded in `...`
#' as an integer.
#'
#' @param ... Forwarded arguments.
#' @keywords internal
#' @export
#' @examples
#' fn <- function(...) dots_n(..., baz)
#' fn(foo, bar)
dots_n <- function(...) {
  nargs()
}

abort_dots_homonyms <- function(dots, dups) {
  .__error_call__. <- "caller"

  nms <- names(dots)

  # This includes the first occurrence as well
  dups_all <- nms %in% nms[dups]

  dups_nms <- unique(nms[dups_all])
  dups_n <- length(dups_nms)

  if (!dups_n) {
    abort("Internal error: Expected dots duplicates")
  }

  enums <- map(dups_nms, homonym_enum, dups_all, nms)
  line <- "Multiple arguments named `%s` at positions %s."
  enums_lines <- map2_chr(dups_nms, enums, sprintf, fmt = line)

  abort(c(
    "Arguments in `...` must have unique names.",
    set_names(enums_lines, "x")
  ))
}

homonym_enum <- function(nm, dups, nms) {
  dups[nms != nm] <- FALSE
  chr_enumerate(as.character(which(dups)), final = "and")
}


# This helper is used when splicing S3 or S4 objects found
# in `!!!`. It is similar to `as.list()`, but the names of
# `x` always end up on the names of the output list,
# unlike `as.list.factor()`.
rlang_as_list <- function(x) {
  if ("list" %in% class(x)) {
    # `x` explicitly inherits from `"list"`, which we take it to mean
    # that it has list storage (i.e. it's not a class like POSIXlt,
    # it's not proxied, and it's not a scalar object like `"lm"`)
    out <- vec_unstructure(x)
  } else if (is.list(x)) {
    out <- rlang_as_list_from_list_impl(x)
  } else {
    out <- rlang_as_list_impl(x)
  }

  names(out) <- names(x)

  out
}

rlang_as_list_impl <- function(x) {
  n <- length(x)
  out <- vector("list", n)

  for (i in seq_len(n)) {
    out[[i]] <- x[[i]]
  }

  out
}

# Special handling if `x` is already a list.
# This avoids the potential for `out[[i]] <- NULL`,
# which shortens the list.
rlang_as_list_from_list_impl <- function(x) {
  n <- length(x)
  out <- vector("list", n)

  for (i in seq_len(n)) {
    elt <- x[[i]]

    if (is.null(elt)) {
      next
    }

    out[[i]] <- elt
  }

  out
}


#' Development notes - `dots.R`
#'
#' @section `.__error_call__.` flag in dots collectors:
#'
#' Dots collectors like [dots_list()] are a little tricky because they
#' may error out in different situations. Do we want to forward the
#' context, i.e. set the call flag to the calling environment?
#' Collectors throw errors in these cases:
#'
#' 1. While checking their own parameters, in which case the relevant
#'    context is the collector itself and we don't forward.
#'
#' 2. While collecting the dots, during evaluation of the supplied
#'    arguments. In this case forwarding or not is irrelevant because
#'    expressions in `...` are evaluated in their own environment
#'    which is not connected to the collector's context.
#'
#' 3. While collecting the dots, during argument constraints checks
#'    such as determined by the `.homonyms` argument. In this case we
#'    want to forward the context because the caller of the dots
#'    collector is the one who determines the constraints for its
#'    users.
#'
#' @keywords internal
#' @name dev-notes-dots
NULL
