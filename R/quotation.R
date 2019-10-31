#' Defusing R expressions
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' When a function argument is defused, R doesn't return its value
#' like it normally would. Instead, it returns the R expression
#' describing how to make the value as an inert object. These defused
#' expressions are like blueprints for computing values.
#'
#' The main purpose of preventing evaluation of an expression is to
#' avoid "object not found" errors when the expression involves
#' data-variables that only exist in a data frame. The expression is
#' defused so it can be resumed later on, in a context where the
#' data-variables have been defined.
#'
#' Defusing prevents the evaluation of R code, but you can still force
#' evaluation inside a defused expression with the [forcing
#' operators][quasiquotation] `!!` and `!!!`.
#'
#'
#' @section Types of defused expressions:
#'
#' * __Calls__ represent the action of calling a function to compute a
#'   new value, such as a vector.
#'
#' * __Symbols__ represent named objects. When the object pointed to
#'   by the symbol was defined in a function or in the global
#'   environment, we call it an environment-variable. When the object
#'   is a column in a data frame, we call it a data-variable.
#'
#' You can create new call or symbol objects by using the defusing
#' function `expr()`:
#'
#' ```
#' # Create a symbol representing objects called `foo`
#' expr(foo)
#'
#' # Create a call representing the computation of the mean of `foo`
#' expr(mean(foo, na.rm = TRUE))
#' ```
#'
#' Defusing is not the only way to create defused expressions. You can
#' also assemble them from data:
#'
#' ```
#' # Assemble a symbol from a string
#' var <- "foo"
#' sym(var)
#'
#' # Assemble a call from strings, symbols, and other objects
#' call("mean", sym(var), na.rm = TRUE)
#' ```
#'
#'
#' @section Defusing function arguments:
#'
#' There are two points of view when it comes to defusing an
#' expression:
#'
#' * You can defuse expressions that _you_ supply with `expr()`. This
#'   is one way of creating symbols and calls (see previous section).
#'
#' * You can defuse the expressions supplied by _the user_ of your
#'   function with the operators starting with `en` like `ensym()`,
#'   `enquo()` and their plural variants. They defuse function
#'   arguments .
#'
#'
#' @section Defused arguments and quosures:
#'
#' If you inspect the return values of `expr()` and `enquo()`, you'll
#' notice that the latter doesn't return a raw expression like the
#' former. Instead it returns a __quosure__, a wrapper containing an
#' expression and an environment. R needs information about the
#' environment to properly evaluate the argument expression because it
#' comes from a different context than the current function.
#'
#' See the [quosure] help topic about tools to work with quosures.
#'
#'
#' @section Comparison to base R:
#'
#' * The defusing operator `expr()` is similar to [quote()]. Like
#'   [bquote()], it allows [forcing][quotation] evaluation of parts
#'   of an expression.
#'
#' * The plural variant `exprs()` is similar to [alist()].
#'
#' * The argument-defusing operator `enquo()` is similar to
#'   [substitute()].
#'
#'
#' @inheritParams tidy-dots
#' @param expr An expression.
#' @param arg A symbol representing an argument. The expression
#'   supplied to that argument will be captured instead of being
#'   evaluated.
#' @param ... For `enexprs()`, `ensyms()` and `enquos()`, names of
#'   arguments to capture without evaluation (including `...`). For
#'   `exprs()` and `quos()`, the expressions to capture unevaluated
#'   (including expressions contained in `...`).
#' @param .named Whether to ensure all dots are named. Unnamed
#'   elements are processed with [quo_name()] to build a default
#'   name. See also [quos_auto_name()].
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty. Note that `"trailing"`
#'   applies only to arguments passed in `...`, not to named
#'   arguments. On the other hand, `"all"` also applies to named
#'   arguments.
#' @param .unquote_names Whether to treat `:=` as `=`. Unlike `=`, the
#'   `:=` syntax supports `!!` unquoting on the LHS.
#' @name quotation
#' @examples
#' # expr() and exprs() capture expressions that you supply:
#' expr(symbol)
#' exprs(several, such, symbols)
#'
#' # enexpr() and enexprs() capture expressions that your user supplied:
#' expr_inputs <- function(arg, ...) {
#'   user_exprs <- enexprs(arg, ...)
#'   user_exprs
#' }
#' expr_inputs(hello)
#' expr_inputs(hello, bonjour, ciao)
#'
#' # ensym() and ensyms() provide additional type checking to ensure
#' # the user calling your function has supplied bare object names:
#' sym_inputs <- function(...) {
#'   user_symbols <- ensyms(...)
#'   user_symbols
#' }
#' sym_inputs(hello, "bonjour")
#' ## sym_inputs(say(hello))  # Error: Must supply symbols or strings
#' expr_inputs(say(hello))
#'
#'
#' # All these quoting functions have quasiquotation support. This
#' # means that you can unquote (evaluate and inline) part of the
#' # captured expression:
#' what <- sym("bonjour")
#' expr(say(what))
#' expr(say(!!what))
#'
#' # This also applies to expressions supplied by the user. This is
#' # like an escape hatch that allows control over the captured
#' # expression:
#' expr_inputs(say(!!what), !!what)
#'
#'
#' # Finally, you can capture expressions as quosures. A quosure is an
#' # object that contains both the expression and its environment:
#' quo <- quo(letters)
#' quo
#'
#' get_expr(quo)
#' get_env(quo)
#'
#' # Quosures can be evaluated with eval_tidy():
#' eval_tidy(quo)
#'
#' # They have the nice property that you can pass them around from
#' # context to context (that is, from function to function) and they
#' # still evaluate in their original environment:
#' multiply_expr_by_10 <- function(expr) {
#'   # We capture the user expression and its environment:
#'   expr <- enquo(expr)
#'
#'   # Then create an object that only exists in this function:
#'   local_ten <- 10
#'
#'   # Now let's create a multiplication expression that (a) inlines
#'   # the user expression as LHS (still wrapped in its quosure) and
#'   # (b) refers to the local object in the RHS:
#'   quo(!!expr * local_ten)
#' }
#' quo <- multiply_expr_by_10(2 + 3)
#'
#' # The local parts of the quosure are printed in colour if your
#' # terminal is capable of displaying colours:
#' quo
#'
#' # All the quosures in the expression evaluate in their original
#' # context. The local objects are looked up properly and we get the
#' # expected result:
#' eval_tidy(quo)
NULL

#' @rdname quotation
#' @export
expr <- function(expr) {
  enexpr(expr)
}
#' @rdname quotation
#' @export
enexpr <- function(arg) {
  .Call(rlang_enexpr, substitute(arg), parent.frame())
}

#' @rdname quotation
#' @export
exprs <- function(...,
                  .named = FALSE,
                  .ignore_empty = c("trailing", "none", "all"),
                  .unquote_names = TRUE) {
  .Call(rlang_exprs_interp,
    frame_env = environment(),
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = "keep",
    check_assign = FALSE
  )
}
#' @rdname quotation
#' @export
enexprs <- function(...,
                   .named = FALSE,
                   .ignore_empty = c("trailing", "none", "all"),
                   .unquote_names = TRUE,
                   .homonyms = c("keep", "first", "last", "error"),
                   .check_assign = FALSE) {
  endots(
    call = sys.call(),
    frame_env = parent.frame(),
    capture_arg = rlang_enexpr,
    capture_dots = rlang_exprs_interp,
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
}

#' @rdname quotation
#' @export
ensym <- function(arg) {
  .Call(rlang_ensym, substitute(arg), parent.frame())
}
#' @rdname quotation
#' @export
ensyms <- function(...,
                   .named = FALSE,
                   .ignore_empty = c("trailing", "none", "all"),
                   .unquote_names = TRUE,
                   .homonyms = c("keep", "first", "last", "error"),
                   .check_assign = FALSE) {
  exprs <- endots(
    call = sys.call(),
    frame_env = parent.frame(),
    capture_arg = rlang_enexpr,
    capture_dots = rlang_exprs_interp,
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
  map(exprs, function(expr) {
    if (is_quosure(expr)) {
      expr <- quo_get_expr(expr)
    }
    sym(expr)
  })
}


#' @rdname quotation
#' @export
quo <- function(expr) {
  enquo(expr)
}
#' @rdname quotation
#' @export
enquo <- function(arg) {
  .Call(rlang_enquo, substitute(arg), parent.frame())
}

#' @rdname quotation
#' @export
quos <- function(...,
                 .named = FALSE,
                 .ignore_empty = c("trailing", "none", "all"),
                 .unquote_names = TRUE) {
  .Call(rlang_quos_interp,
    frame_env = environment(),
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = "keep",
    check_assign = FALSE
  )
}
#' @rdname quotation
#' @export
enquos <- function(...,
                   .named = FALSE,
                   .ignore_empty = c("trailing", "none", "all"),
                   .unquote_names = TRUE,
                   .homonyms = c("keep", "first", "last", "error"),
                   .check_assign = FALSE) {
  quos <- endots(
    call = sys.call(),
    frame_env = parent.frame(),
    capture_arg = rlang_enquo,
    capture_dots = rlang_quos_interp,
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
  structure(quos, class = "quosures")
}


capture_args <- c(
  ".named",
  ".ignore_empty",
  ".unquote_names",
  ".homonyms",
  ".check_assign"
)

endots <- function(call,
                   frame_env,
                   capture_arg,
                   capture_dots,
                   named,
                   ignore_empty,
                   unquote_names,
                   homonyms,
                   check_assign) {
  ignore_empty <- arg_match(ignore_empty, c("trailing", "none", "all"))
  syms <- as.list(node_cdr(call))

  if (!is_null(names(syms))) {
    is_arg <- names(syms) %in% capture_args
    syms <- syms[!is_arg]
  }

  # Avoid note about registration problems
  dot_call <- .Call

  splice_dots <- FALSE
  dots <- map(syms, function(sym) {
    if (!is_symbol(sym)) {
      abort("Inputs to capture must be argument names")
    }
    if (identical(sym, dots_sym)) {
      splice_dots <<- TRUE
      splice(dot_call(capture_dots,
        frame_env = frame_env,
        named = named,
        ignore_empty = ignore_empty,
        unquote_names = unquote_names,
        homonyms = homonyms,
        check_assign = check_assign
      ))
    } else {
      dot_call(capture_arg, sym, frame_env)
    }
  })

  if (splice_dots) {
    dots <- flatten_if(dots, is_spliced)
  }

  if (ignore_empty == "all") {
    if (identical(capture_arg, rlang_enquo)) {
      dot_is_missing <- quo_is_missing
    } else {
      dot_is_missing <- is_missing
    }
    dots <- keep(dots, negate(dot_is_missing))
  }

  if (named) {
    dots <- quos_auto_name(dots)
  }
  names(dots) <- names2(dots)

  dots
}

#' Ensure that all elements of a list of expressions are named
#'
#' This gives default names to unnamed elements of a list of
#' expressions (or expression wrappers such as formulas or
#' quosures). `exprs_auto_name()` deparses the expressions with
#' [expr_name()] by default. `quos_auto_name()` deparses with
#' [quo_name()].
#'
#' @param exprs A list of expressions.
#' @param width Deprecated. Maximum width of names.
#' @param printer Deprecated. A function that takes an expression
#'   and converts it to a string. This function must take an
#'   expression as the first argument and `width` as the second
#'   argument.
#' @export
exprs_auto_name <- function(exprs, width = NULL, printer = NULL) {
  if (!is_null(width)) {
    warn_deprecated(paste_line(
      "The `width` argument is deprecated as of rlang 0.3.0."
    ))
  }

  if (!is_null(printer)) {
    warn_deprecated(paste_line(
      "The `printer` argument is deprecated as of rlang 0.3.0."
    ))
  }

  have_name <- have_name(exprs)
  if (any(!have_name)) {
    nms <- map_chr(exprs[!have_name], as_label)
    names(exprs)[!have_name] <- nms
  }

  exprs
}
#' @rdname exprs_auto_name
#' @param quos A list of quosures.
#' @export
quos_auto_name <- function(quos, width = NULL) {
  exprs_auto_name(quos, width = width)
}
