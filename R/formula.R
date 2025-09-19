#' Create a formula
#'
#' @param lhs,rhs A call, name, or atomic vector.
#' @param env An environment.
#' @return A formula object.
#' @seealso [new_quosure()]
#' @export
#' @examples
#' new_formula(quote(a), quote(b))
#' new_formula(NULL, quote(b))
new_formula <- function(lhs, rhs, env = caller_env()) {
  .Call(ffi_new_formula, lhs, rhs, env)
}

#' Is object a formula?
#'
#' @description
#' `is_formula()` tests whether `x` is a call to `~`. `is_bare_formula()`
#' tests in addition that `x` does not inherit from anything else than
#' `"formula"`.
#'
#' __Note__: When we first implemented `is_formula()`, we thought it
#' best to treat unevaluated formulas as formulas by default (see
#' section below). Now we think this default introduces too many edge
#' cases in normal code. We recommend always supplying `scoped =
#' TRUE`. Unevaluated formulas can be handled via a `is_call(x, "~")`
#' branch.
#'
#' @param x An object to test.
#' @param scoped A boolean indicating whether the quosure is scoped,
#'   that is, has a valid environment attribute and inherits from
#'   `"formula"`. If `NULL`, the scope is not inspected.
#' @param lhs A boolean indicating whether the formula has a left-hand
#'   side. If `NULL`, the LHS is not inspected and `is_formula()`
#'   returns `TRUE` for both one- and two-sided formulas.
#'
#' @section Dealing with unevaluated formulas:
#' At parse time, a formula is a simple call to `~` and it does not
#' have a class or an environment. Once evaluated, the `~` call
#' becomes a properly structured formula. Unevaluated formulas arise
#' by quotation, e.g. `~~foo`, `quote(~foo)`, or `substitute(arg)`
#' with `arg` being supplied a formula. Use the `scoped` argument to
#' check whether the formula carries an environment.
#'
#' @examples
#' is_formula(~10)
#' is_formula(10)
#'
#' # If you don't supply `lhs`, both one-sided and two-sided formulas
#' # will return `TRUE`
#' is_formula(disp ~ am)
#' is_formula(~am)
#'
#' # You can also specify whether you expect a LHS:
#' is_formula(disp ~ am, lhs = TRUE)
#' is_formula(disp ~ am, lhs = FALSE)
#' is_formula(~am, lhs = TRUE)
#' is_formula(~am, lhs = FALSE)
#'
#' # Handling of unevaluated formulas is a bit tricky. These formulas
#' # are special because they don't inherit from `"formula"` and they
#' # don't carry an environment (they are not scoped):
#' f <- quote(~foo)
#' f_env(f)
#'
#' # By default unevaluated formulas are treated as formulas
#' is_formula(f)
#'
#' # Supply `scoped = TRUE` to ensure you have an evaluated formula
#' is_formula(f, scoped = TRUE)
#'
#' # By default unevaluated formulas not treated as bare formulas
#' is_bare_formula(f)
#'
#' # If you supply `scoped = TRUE`, they will be considered bare
#' # formulas even though they don't inherit from `"formula"`
#' is_bare_formula(f, scoped = TRUE)
#' @export
is_formula <- function(x, scoped = NULL, lhs = NULL) {
  .Call(ffi_is_formula, x, scoped, lhs)
}
#' @rdname is_formula
#' @export
is_bare_formula <- function(x, scoped = TRUE, lhs = NULL) {
  if (!is_formula(x, scoped = scoped, lhs = lhs)) {
    return(FALSE)
  }

  if (is_null(scoped)) {
    exp_class <- c("call", "formula")
  } else if (is_true(scoped)) {
    exp_class <- "formula"
  } else if (is_false(scoped)) {
    exp_class <- "call"
  } else {
    stop_input_type(scoped, "`NULL` or a logical value.")
  }
  is_string(class(x), exp_class)
}

#' Get or set formula components
#'
#' `f_rhs` extracts the right-hand side, `f_lhs` extracts the left-hand
#' side, and `f_env` extracts the environment in which the formula was defined.
#' All functions throw an error if `f` is not a formula.
#'
#' @param f,x A formula
#' @param value The value to replace with.
#' @export
#' @return `f_rhs` and `f_lhs` return language objects (i.e.  atomic
#'   vectors of length 1, a name, or a call). `f_env` returns an
#'   environment.
#' @examples
#' f_rhs(~ 1 + 2 + 3)
#' f_rhs(~ x)
#' f_rhs(~ "A")
#' f_rhs(1 ~ 2)
#'
#' f_lhs(~ y)
#' f_lhs(x ~ y)
#'
#' f_env(~ x)
#' f <- as.formula("y ~ x", env = new.env())
#' f_env(f)
f_rhs <- function(f) {
  if (is_quosure(f)) {
    signal_formula_access()
    return(quo_get_expr(f))
  }
  .Call(ffi_f_rhs, f)
}

#' @export
#' @rdname f_rhs
`f_rhs<-` <- function(x, value) {
  if (is_quosure(x)) {
    signal_formula_access()
    return(quo_set_expr(x, value))
  }
  check_formula(x, arg = "LHS")
  x[[length(x)]] <- value
  x
}

#' @export
#' @rdname f_rhs
f_lhs <- function(f) {
  if (is_quosure(f)) {
    signal_formula_access()
    abort("Can't retrieve the LHS of a quosure.")
  }
  .Call(ffi_f_lhs, f)
}

#' @export
#' @rdname f_rhs
`f_lhs<-` <- function(x, value) {
  if (is_quosure(x)) {
    signal_formula_access()
    abort("Can't set the LHS of a quosure.")
  }
  check_formula(x, arg = "LHS")

  if (length(x) < 3) {
    x <- duplicate(x)
    node_poke_cdr(x, pairlist(value, x[[2]]))
  } else {
    x[[2]] <- value
  }

  x
}

#' @export
#' @rdname f_rhs
f_env <- function(f) {
  if (is_quosure(f)) {
    signal_formula_access()
    return(quo_get_env(f))
  }
  check_formula(f)
  attr(f, ".Environment")
}

#' @export
#' @rdname f_rhs
`f_env<-` <- function(x, value) {
  if (is_quosure(x)) {
    signal_formula_access()
    return(quo_set_env(x, value))
  }
  if (!is_formula(x)) {
    abort("`f` must be a formula")
  }
  structure(x, .Environment = value)
}

#' Turn RHS of formula into a string or label
#'
#' Equivalent of [expr_text()] and [expr_label()] for formulas.
#'
#' @param x A formula.
#' @inheritParams expr_text
#' @export
#' @examples
#' f <- ~ a + b + bc
#' f_text(f)
#' f_label(f)
#'
#' # Names a quoted with ``
#' f_label(~ x)
#' # Strings are encoded
#' f_label(~ "a\nb")
#' # Long expressions are collapsed
#' f_label(~ foo({
#'   1 + 2
#'   print(x)
#' }))
f_text <- function(x, width = 60L, nlines = Inf) {
  expr_text(f_rhs(x), width = width, nlines = nlines)
}
#' @rdname f_text
#' @export
f_name <- function(x) {
  expr_name(f_rhs(x))
}
#' @rdname f_text
#' @export
f_label <- function(x) {
  expr_label(f_rhs(x))
}


signal_formula_access <- function() {
  if (is_true(peek_option("rlang:::warn_quosure_access"))) {
    warn(
      "Using formula accessors with quosures is soft-deprecated"
    )
  }
}
