#' Quotation
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' Quotation is a mechanism by which an expression supplied as
#' argument is captured by a function. Instead of seeing the value of
#' the argument, the function sees the recipe (the R code) to make
#' that value. This is possible because R [expressions][is_expr] are
#' representable as regular objects in R:
#'
#' * Calls represent the action of calling a function to
#'   compute a new value. Evaluating a call causes that value to be
#'   computed. Calls typically involve symbols to reference R objects.
#'
#' * Symbols represent the name that is given to an object in a
#'   particular context (an [environment][env]).
#'
#' We call objects containing calls and symbols [expressions][is_expr].
#' There are two ways to create R expressions. First you can **build**
#' calls and symbols from parts and pieces (see [sym()], [syms()] and
#' [call2()]). The other way is by *quotation* or *quasiquotation*,
#' i.e. by intercepting an expression instead of evaluating it.
#'
#'
#' @section User expressions versus your expressions:
#'
#' There are two points of view when it comes to capturing an
#' expression:
#'
#' * You can capture the expressions supplied by _the user_ of your
#'   function. This is the purpose of `ensym()`, `enexpr()` and
#'   `enquo()` and their plural variants. These functions take an
#'   argument name and capture the expression that was supplied to
#'   that argument.
#'
#' * You can capture the expressions that _you_ supply. To this end
#'   use `expr()` and `quo()` and their plural variants `exprs()` and
#'   `quos()`.
#'
#'
#' @section Capture raw expressions:
#'
#' * `enexpr()` and `expr()` capture a single raw expression.
#'
#' * `enexprs()` and `exprs()` capture a list of raw expressions
#'   including expressions contained in `...`.
#'
#' * `ensym()` and `ensyms()` are variants of `enexpr()` and
#'   `enexprs()` that check the captured expression is either a string
#'   (which they convert to symbol) or a symbol. If anything else
#'   is supplied they throw an error.
#'
#' In terms of base functions, `enexpr(arg)` corresponds to
#' `base::substitute(arg)` (though that function also features complex
#' substitution semantics) and `expr()` is like [quote()] (and
#' [bquote()] if we consider unquotation syntax). The plural variant
#' `exprs()` is equivalent to [base::alist()]. Finally there is no
#' function in base R that is equivalent to `enexprs()` but you can
#' reproduce its behaviour with `eval(substitute(alist(...)))`.
#'
#'
#' @section Capture expressions in quosures:
#'
#' `quo()` and `enquo()` are similar to their `expr` counterparts but
#' capture both the expression and its environment in an object called
#' a quosure. This wrapper contains a reference to the original
#' environment in which that expression was captured. Keeping track of
#' the environments of expressions is important because this is where
#' functions and objects mentioned in the expression are defined.
#'
#' Quosures are objects that can be evaluated with [eval_tidy()] just
#' like symbols or function calls. Since they always evaluate in their
#' original environment, quosures can be seen as vehicles that allow
#' expressions to travel from function to function but that beam back
#' instantly to their original environment upon evaluation.
#'
#' See the [quosure] help topic about tools to work with quosures.
#'
#'
#' @section Quasiquotation:
#'
#' All quotation functions in rlang have support for [unquoting
#' operators][quasiquotation]. The combination of quotation and
#' unquotation is called *quasiquotation*.
#'
#' Unquotation provides a way to refer to variables during quotation.
#' Variables are problematic when quoting because a captured
#' expression is essentially a constant, just like a string is a
#' constant. For instance in all the following cases `apple` is a
#' constant: `~apple`, `"apple"` and `expr(apple)`. Unquoting allows
#' you to introduce a part of variability within a captured
#' expression.
#'
#' * In the case of `enexpr()` and `enquo()`, unquoting provides an
#'   escape hatch to the users of your function that allows them to
#'   manipulate the expression that you capture.
#'
#' * In the case of `expr()` and `quo()`, quasiquotation lets you
#'   build a complex expressions where some parts are constant (the
#'   parts that are captured) and some parts are variable (the parts
#'   that are unquoted).
#'
#' See the [quasiquotation] help topic for more about this as well as
#' [the chapter in Advanced R](https://adv-r.hadley.nz/quasiquotation.html).
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
