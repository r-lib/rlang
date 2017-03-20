#' Inspect an argument
#'
#' `arg_inspect()` provides argument introspection in the context of
#' lazy evaluation. Compared to [catch_quosure()], the returned
#' information is more complete and takes R's lazy evaluation
#' semantics into account: if an argument is passed around without
#' being evaluated, `arg_inspect()` is able to return the expression
#' at the original call site as well as the relevant scoping
#' environment in which this expression is supposed to be evaluated
#' when the argument is forced. To accomplish this, `arg_inspect()`
#' climbs the call stack to find where an argument was first supplied,
#' with which expression, in which evaluation environment.
#' `arg_inspect_()` is the standard-evaluation version of
#' `arg_inspect()` and takes a symbol and a call stack object.
#'
#' `arg_inspect()` should be used with two caveats in mind. First, it
#' is slower than [catch_quosure()] and `lazyeval::lazy()`. Thus you
#' should probably avoid using it in functions that might be used in
#' tight loops (such as a loop over the rows of data frame). Second,
#' `arg_inspect()` ignores all reassignment of arguments. It has no
#' way of detecting that an inspected argument got reassigned along
#' the way, and will continue to climb the calls looking for an
#' earlier call site. These two limitations are inherent to the stack
#' climbing approach that powers this function.
#'
#' @param x An argument to a function.
#' @return A list containing:
#'
#'   \item{caller_frame}{The original calling frame. This is the frame
#'     in which `expr` should be evaluated, unless the argument is
#'     missing (see below).}
#'
#'   \item{expr}{The expression provided in the original call. If the
#'     argument was missing, `expr` is the default argument of the
#'     function; if there was no default, `expr` is the missing
#'     argument (see [missing_arg()]).}
#'
#'   \item{name}{The name of the formal argument to which `expr` was
#'     originally supplied.}
#'
#'   \item{ctxt_frame}{The frame providing the scope for `expr`, which
#'     should normally be evaluated in `ctxt_frame$env`.  This is
#'     normally the original calling frame, unless the argument was
#'     missing. In that case, `ctxt_frame` is the evaluation frame of
#'     the called function. The difference reflects the evaluation
#'     rules of R, where default arguments are scoped within the
#'     called function rather than the calling frame.}
#' @export
arg_inspect <- function(x) {
  stack <- call_stack()
  expr <- quote(x)
  arg_inspect_(expr, stack)
}

#' @rdname arg_inspect
#' @inheritParams dots_inspect
#' @param expr A quoted symbol giving the name of the argument to
#'   inspect.
#' @param stack A `call_stack` object as returned by [call_stack()].
#' @export
arg_inspect_ <- function(expr, stack, only_dots = FALSE) {
  stopifnot(is_call_stack(stack))
  stopifnot(length(stack) > 1)

  # In this loop `expr` is the argument of the frame just before
  # the current `i`th frame, the tentative caller frame
  caller_frame <- stack[[1]]
  ctxt_frame <- stack[[1]]
  formal_name <- NULL

  for (i in seq_len(length(stack) - 1)) {

    call <- lang_homogenise(stack[[i]],
      enum_dots = TRUE, add_missings = TRUE)

    # If argument introspection does not have lazy evaluation scope,
    # we've necessarily reached the call site unless we are dealing
    # with a ..n symbol (which is always the case during the first
    # iteration).
    if (only_dots &&  !is_null(formal_name) && !is_dot_symbol(expr)) {
      if (is_dot_nm(formal_name)) {
        formal_name <- NA_character_
      }
      break
    }

    # The `caller_expr` is always matched and valid during the first
    # iteration of the loop
    arg_i <- arg_match(expr, call)
    caller_expr <- call[[arg_i]]

    # If no match in the call, we have reached the call site.
    if (is.na(arg_i)) {
      break
    }

    # The matched argument is missing, either implicitely or
    # explicitely. The evaluation frame of missing arguments is the
    # current frame, but the caller is the next one
    if (missing(caller_expr)) {
      formal_name <- as.character(expr)
      expr <- fml_default(expr, ctxt_frame$fn)
      caller_frame <- stack[[i + 1]]
      break
    }

    # If `caller_expr` is a complex expression, we have reached the
    # callee frame, and the next frame is both the caller and
    # evaluation frame
    if (!is_symbol(caller_expr)) {
      formal_name <- as.character(expr)
      expr <- caller_expr
      caller_frame <- stack[[i + 1]]
      ctxt_frame <- stack[[i + 1]]
      break
    }

    # If the argument matched in the caller signature is another
    # symbol, record it and move on to next frame
    formal_name <- as.character(expr)
    expr <- caller_expr

    caller_frame <- stack[[i + 1]]
    ctxt_frame <- stack[[i + 1]]
  }

  list(
    expr = maybe_missing(expr),
    name = formal_name,
    ctxt_frame = ctxt_frame,
    caller_frame = caller_frame
  )
}

arg_match <- function(sym, call) {
  arg_nm <- as.character(sym)
  match(arg_nm, names2(call))
}
fml_default <- function(expr, fn) {
  nm <- as.character(expr)
  fmls <- formals(fn)
  if (nm %in% names(fmls)) {
    fmls[[nm]]
  } else {
    missing_arg()
  }
}


#' Generate or handle a missing argument
#'
#' These functions help using the missing argument as a regular R
#' object. It is valid to generate a missing argument and assign it in
#' the current environment or in a list. However, once assigned in the
#' environment, the missing argument normally cannot be touched.
#' `maybe_missing()` checks whether the object is the missing
#' argument, and regenerate it if needed to prevent R from throwing a
#' missing error. In addition, `is_missing()` lets you check for a
#' missing argument in a larger range of situations than
#' [base::missing()] (see examples).
#' @param x An object that might be the missing argument.
#' @export
#' @examples
#' # The missing argument can be useful to generate calls
#' quosure(f(x = !! missing_arg()))
#' quosure(f(x = !! NULL))
#'
#'
#' # It is perfectly valid to generate and assign the missing
#' # argument.
#' x <- missing_arg()
#' l <- list(missing_arg())
#'
#' # Note that accessing a missing argument contained in a list does
#' # not trigger an error:
#' l[[1]]
#' is.null(l[[1]])
#'
#' # But if the missing argument is assigned in the current
#' # environment, it is no longer possible to touch it. The following
#' # lines would all return errors:
#' #> x
#' #> is.null(x)
#'
#' # In these cases, you can use maybe_missing() to manipulate an
#' # object that might be the missing argument without triggering a
#' # missing error:
#' maybe_missing(x)
#' is.null(maybe_missing(x))
#' is_missing(maybe_missing(x))
#'
#'
#' # base::missing() does not work well if you supply an
#' # expression. The following lines would throw an error:
#'
#' #> missing(missing_arg())
#' #> missing(l[[1]])
#'
#' # while is_missing() will work as expected:
#' is_missing(missing_arg())
#' is_missing(l[[1]])
missing_arg <- function() {
  quote(expr = )
}

#' @rdname missing_arg
#' @export
is_missing <- function(x) {
  expr <- substitute(x)
  if (is_symbol(expr) && missing(x)) {
    TRUE
  } else {
    identical(x, missing_arg())
  }
}

#' @rdname missing_arg
#' @export
maybe_missing <- function(x) {
  if (is_missing(x)) {
    missing_arg()
  } else {
    x
  }
}
