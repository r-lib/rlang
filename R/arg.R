#' Capture an expression and evaluation environment.
#'
#' A powerful feature in R is the ability of capturing the expression
#' supplied in a function call. Once captured, this expression can be
#' inspected and evaluated, possibly within an altered scope
#' environment (for instance a scope where all elements of a data
#' frame are directly accessible). The tricky part of capturing an
#' expression for rescoping purposes is to keep the original
#' evaluation environment accessible so that other variables defined
#' at the call site are available when the expression gets
#' evaluated. It is thus necessary to capture not only the R
#' expression supplied as argument in a function call, but also the
#' evaluation environment of the call site. \code{arg_capture()} and
#' \code{dots_capture()} make it easy to record this information
#' within formulas.
#'
#' The two main purposes for capturing arguments are labelling and
#' rescoping. With labelling, the normal R evaluation rules are kept
#' unchanged. The captured expression is only used to provide default
#' names (or labels) to output elements, such as column names for a
#' function that manipulates data frames or axis labels for a function
#' that creates graphics. In the case of rescoping however, evaluation
#' rules are altered. Functions that capture and rescope arguments are
#' said to use non-standard evaluation, or NSE. The approach we
#' recommend in rlang is to always create two versions of such
#' functions: a NSE version that captures arguments, and another that
#' work with captured arguments with standard evaluation rules (see
#' \code{decorate_nse()}). Providing a standard evaluation
#' version simplifies programming tasks. Also, it makes it possible to
#' forward named arguments across function calls (see below). See
#' \code{vignette("nse")} for more information on NSE.
#'
#' @section Forwarding arguments: You have to be a bit careful when
#'   you pass arguments between introspective functions as only the
#'   most immediate call site is captured. For this reason, named
#'   arguments should be captured by an NSE function at the outermost
#'   level, and then passed around to SE versions that handle
#'   pre-captured arguments. See \code{\link{arg_info}()} for another
#'   approach to introspecting arguments with which it is possible to
#'   capture expressions at the outermost call site. This approach may
#'   be harder to reason about and has some limitations.
#'
#'   Dots are different from named arguments in that they are
#'   implicitely forwarded. Forwarding dots does not create a new call
#'   site. The expression is passed on as is. That's why you can
#'   easily capture them with \code{\link[base]{substitute}()}. By the
#'   same token, you don't need to capture dots before passing them
#'   along in another introspective function. You do need to be a bit
#'   careful when you rescope expressions captured from dots because
#'   those expressions were not necessarily supplied in the last call
#'   frame. In general, the call site of argument passed through dots
#'   can be anywhere between the current and global frames. For this
#'   reason, it is recommended to always use \code{dots_capture()}
#'   rather than \code{substitute()} and \code{env_caller()} or
#'   \code{parent.frame()}, since the former will encode the
#'   appropriate evaluation environments within the formulas.
#'
#' @param x,... Arguments to capture.
#' @export
#' @return \code{arg_capture()} returns a formula; \code{dots_capture()}
#'   returns a list of formulas, one for each dotted argument.
#' @seealso \code{\link{arg_expr}()}, \code{\link{arg_label}()} and
#'   \code{\link{arg_text}()} provide labelling information in
#'   alternative forms.
#' @examples
#' # arg_capture() returns a formula:
#' fn <- function(foo) arg_capture(foo)
#' fn(a + b)
#'
#' # Capturing an argument only works for the most direct call:
#' g <- function(bar) fn(bar)
#' g(a + b)
#'
#'
#' # Dots on the other hand are forwarded all the way to
#' # dots_capture() and can be captured across levels:
#' fn <- function(...) dots_capture(y = a + b, ...)
#' fn(z = a + b)
#'
#' # Note that if you pass a named argument in dots, only the
#' # expression at the dots call site is captured:
#' fn <- function(x = a + b) dots_capture(x = x)
#' fn()
arg_capture <- function(x) {
  x_expr <- substitute(x)
  x_env <- env_caller(1)

  arg_expr <- substitute_(x_expr, x_env)
  arg_env <- env_caller(2)
  f_new(arg_expr, env = arg_env)
}


#' Find the expression associated with an argument
#'
#' \code{arg_expr()} returns the full expression ans is equivalent to
#' the base function \code{\link[base]{substitute}()};
#' \code{arg_text()} turns the expression into a single string;
#' \code{arg_label()} formats it nicely for use in messages.
#'
#' @param x Argument to capture.
#' @export
#' @examples
#' arg_label(10)
#'
#' # Names a quoted with ``
#' arg_label(x)
#'
#' # Strings are encoded
#' arg_label("a\nb")
#'
#' # Expressions are captured
#' arg_label(a + b + c)
#'
#' # Long expressions are collapsed
#' arg_label(foo({
#'   1 + 2
#'   print(x)
#' }))
arg_expr <- function(x) {
  x_expr <- substitute(x)
  x_env <- env_caller(1)
  substitute_(x_expr, x_env)
}

#' @export
#' @rdname arg_expr
arg_label <- function(x) {
  arg_label_(arg_expr(x))
}

arg_label_ <- function(x) {
  if (is.character(x)) {
    encodeString(x, quote = '"')
  } else if (is.atomic(x)) {
    format(x)
  } else if (is.name(x)) {
    paste0("`", as.character(x), "`")
  } else {
    chr <- deparse(x)
    if (length(chr) > 1) {
      dot_call <- call_new(x[[1]], quote(...))
      chr <- paste(deparse(dot_call), collapse = "\n")
    }
    paste0("`", chr, "`")
  }
}

#' @export
#' @rdname arg_expr
#' @param width Width of each line.
#' @param nlines Maximum number of lines to extract.
arg_text <- function(x, width = 60L, nlines = Inf) {
  arg_text_(arg_expr(x), width = width, nlines = nlines)
}

arg_text_ <- function(x, width = 60L, nlines = Inf) {
  str <- deparse(x, width.cutoff = width)

  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }

  paste0(str, collapse = "\n")
}


#' Inspect an argument
#'
#' \code{arg_info()} provides argument introspection in the context of
#' lazy evaluation. Compared to \code{\link{arg_capture}()}, the
#' returned information is more complete and takes R's lazy evaluation
#' semantics into account: if an argument is passed around without
#' being evaluated, \code{arg_info()} is able to return the expression
#' at the original call site as well as the relevant scoping
#' environment in which this expression is supposed to be evaluated
#' when the argument is forced. To accomplish this, \code{arg_info()}
#' climbs the call stack to find where an argument was first supplied,
#' with which expression, in which evaluation environment.
#' \code{arg_info_()} is the standard-evaluation version of
#' \code{arg_info()} and takes a symbol and a call stack object.
#'
#' \code{arg_info()} should be used with two caveats in mind. First,
#' it is slower than \code{\link{arg_capture}()} and
#' \code{lazyeval::lazy()}. Thus you should probably avoid using it in
#' functions that might be used in tight loops (such as a loop over
#' the rows of data frame). Second, \code{arg_info()} ignores all
#' reassignment of arguments. It has no way of detecting that an
#' inspected argument got reassigned along the way, and will continue
#' to climb the calls looking for an earlier call site. These two
#' limitations are inherent to the stack climbing approach that powers
#' this function.
#'
#' @param x An argument to a function.
#' @return A list containing:
#'   \item{caller_frame}{The original calling frame. This is the frame
#'     in which \code{expr} should be evaluated, unless the argument is
#'     missing (see below).}
#'
#'   \item{expr}{The expression provided in the original call. If the
#'     argument was missing, \code{expr} is the default argument of
#'     the function; if there was no default, \code{expr} is the
#'     missing argument (see \code{\link{arg_missing}()}).}
#'
#'   \item{name}{The name of the formal argument to which \code{expr}
#'     was originally supplied.}
#'
#'   \item{eval_frame}{The frame providing the scope for \code{expr},
#'     which should normally be evaluated in \code{eval_frame$env}.
#'     This is normally the original calling frame, unless the
#'     argument was missing. In that case, \code{eval_frame} is the
#'     evaluation frame of the called function. The difference
#'     reflects the evaluation rules of R, where default arguments are
#'     scoped within the called function rather than the calling
#'     frame.}
#' @seealso \code{\link{arg_label}()}, \code{\link{arg_expr}()}.
#' @export
arg_info <- function(x) {
  stack <- call_stack()
  expr <- quote(x)
  arg_info_(expr, stack)
}

#' @rdname arg_info
#' @param expr A quoted symbol giving the name of the argument to
#'   inspect.
#' @param stack A \code{call_stack} object as returned by
#'   \code{\link{call_stack}()}.
#' @export
arg_info_ <- function(expr, stack) {
  stopifnot(is_call_stack(stack))
  stopifnot(length(stack) > 1)

  # In this loop `expr` is the argument of the frame just before
  # the current `i`th frame, the tentative caller frame
  caller_frame <- stack[[1]]
  eval_frame <- stack[[1]]

  for (i in seq_len(length(stack) - 1)) {

    call <- call_standardise(stack[[i]],
      enum_dots = TRUE, add_missings = TRUE)

    # The `caller_expr` is always matched and valid during the first
    # iteration of the loop
    arg_i <- arg_match(expr, call)
    caller_expr <- call[[arg_i]]

    # If no match in the call, we have reached the caller frame.
    if (is.na(arg_i)) {
      break
    }

    # The matched argument is missing, either implicitely or
    # explicitely. The evaluation frame of missing arguments is the
    # current frame, but the caller is the next one
    if (missing(caller_expr)) {
      formal_name <- as.character(expr)
      expr <- fml_default(expr, eval_frame$fn)
      caller_frame <- stack[[i + 1]]
      break
    }

    # If `caller_expr` is a complex expression, we have reached the
    # callee frame, and the next frame is both the caller and
    # evaluation frame
    if (!is.symbol(caller_expr)) {
      formal_name <- as.character(expr)
      expr <- caller_expr
      caller_frame <- stack[[i + 1]]
      eval_frame <- stack[[i + 1]]
      break
    }

    # If the argument matched in the caller signature is another
    # symbol, record it and move on to next frame
    formal_name <- as.character(expr)
    expr <- caller_expr

    caller_frame <- stack[[i + 1]]
    eval_frame <- stack[[i + 1]]
  }

  list(
    expr = maybe_missing(expr),
    name = formal_name,
    eval_frame = eval_frame,
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
    arg_missing()
  }
}


#' Generate or handle a missing argument
#'
#' These functions help using the missing argument as a regular R
#' object. It is valid to generate a missing argument and assign it in
#' the current environment or in a list. However, once assigned in the
#' environment, the missing argument normally cannot be
#' touched. \code{maybe_missing()} checks whether the object is the
#' missing argument, and regenerate it if needed to prevent R from
#' throwing a missing error. In addition, \code{is_missing()} lets you
#' check for a missing argument in a larger range of situations than
#' \code{\link[base]{missing}()} (see examples).
#' @param x An object that might be the missing argument.
#' @export
#' @examples
#' # The missing argument can be useful to generate calls
#' f_interp(~f(x = uq(arg_missing())))
#' f_interp(~f(x = uq(NULL)))
#'
#'
#' # It is perfectly valid to generate and assign the missing
#' # argument.
#' x <- arg_missing()
#' l <- list(arg_missing())
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
#' #> missing(arg_missing())
#' #> missing(l[[1]])
#'
#' # while is_missing() will work as expected:
#' is_missing(arg_missing())
#' is_missing(l[[1]])
arg_missing <- function() {
  quote(expr = )
}

#' @rdname arg_missing
#' @export
is_missing <- function(x) {
  expr <- substitute(x)
  if (is.symbol(expr) && missing(x)) {
    TRUE
  } else {
    identical(x, arg_missing())
  }
}

#' @rdname arg_missing
#' @export
maybe_missing <- function(x) {
  if (is_missing(x)) {
    arg_missing()
  } else {
    x
  }
}
