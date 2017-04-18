#' Create a formula object by "hand".
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
  if (!is_env(env) && !is_null(env)) {
    abort("`env` must be an environment")
  }

  if (is_null(lhs)) {
    f <- lang("~", rhs)
  } else {
    f <- lang("~", lhs, rhs)
  }

  structure(f, class = "formula", .Environment = env)
}

#' Is object a formula?
#'
#' `is_formula()` tests if `x` is a call to `~`. `is_bare_formula()`
#' tests in addition that `x` does not inherit from anything else than
#' `"formula"`. `is_formulaish()` returns `TRUE` for both formulas and
#' [definitions][is_definition] of the type `a := b`.
#'
#' @inheritParams is_quosure
#' @param lhs A boolean indicating whether the [formula][is_formula]
#'   or [definition][is_definition] has a left-hand side. If `NULL`,
#'   the LHS is not inspected.
#' @seealso [is_quosure()] and [is_quosureish()]
#' @export
#' @examples
#' x <- disp ~ am
#' is_formula(x)
#'
#' is_formula(~10)
#' is_formula(10)
#'
#' is_formula(quo(foo))
#' is_bare_formula(quo(foo))
#'
#' # Note that unevaluated formulas are treated as bare formulas even
#' # though they don't inherit from "formula":
#' f <- quote(~foo)
#' is_bare_formula(f)
#'
#' # However you can specify `scoped` if you need the predicate to
#' # return FALSE for these unevaluated formulas:
#' is_bare_formula(f, scoped = TRUE)
#' is_bare_formula(eval(f), scoped = TRUE)
#'
#'
#' # There is also a variant that returns TRUE for definitions in
#' # addition to formulas:
#' is_formulaish(a ~ b)
#' is_formulaish(a := b)
is_formula <- function(x, scoped = NULL, lhs = NULL) {
  if (!is_formulaish(x, scoped = scoped, lhs = lhs)) {
    return(FALSE)
  }
  identical(node_car(x), sym_tilde)
}
#' @rdname is_formula
#' @export
is_bare_formula <- function(x, scoped = NULL, lhs = NULL) {
  if (!is_formula(x, scoped = scoped, lhs = lhs)) {
    return(FALSE)
  }
  class <- class(x)
  is_null(class) || identical(class, "formula")
}
#' @rdname is_formula
#' @export
is_formulaish <- function(x, scoped = NULL, lhs = NULL) {
  if(typeof(x) != "language") {
    return(FALSE)
  }

  head <- node_car(x)
  if (!identical(head, sym_tilde) && !identical(head, sym_def)) {
    return(FALSE)
  }

  if (!is_null(scoped) && scoped != is_env(attr(x, ".Environment"))) {
    return(FALSE)
  }
  if (!is_null(lhs) && !identical(lhs, length(x) > 2)) {
    return(FALSE)
  }

  TRUE
}

#' Get/set formula components.
#'
#' `f_rhs` extracts the righthand side, `f_lhs` extracts the lefthand
#' side, and `f_env` extracts the environment. All functions throw an
#' error if `f` is not a formula.
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
#' @useDynLib rlang f_rhs_
f_rhs <- function(f) {
  .Call(f_rhs_, f)
}

#' @export
#' @rdname f_rhs
`f_rhs<-` <- function(x, value) {
  stopifnot(is_formula(x))
  x[[length(x)]] <- value
  x
}

#' @export
#' @rdname f_rhs
#' @useDynLib rlang f_lhs_
f_lhs <- function(f) {
  .Call(f_lhs_, f)
}

#' @export
#' @rdname f_rhs
`f_lhs<-` <- function(x, value) {
  stopifnot(is_formula(x))
  if (length(x) < 3) {
    x <- duplicate(x)
    mut_node_cdr(x, pairlist(value, x[[2]]))
  } else {
    x[[2]] <- value
  }
  x
}

copy_lang_name <- function(f, x) {
  f[[1]] <- x[[1]]
  f
}

#' @export
#' @rdname f_rhs
f_env <- function(f) {
  if(!is_formula(f)) {
    abort("`f` is not a formula")
  }
  attr(f, ".Environment")
}

#' @export
#' @rdname f_rhs
`f_env<-` <- function(x, value) {
  stopifnot(is_formula(x))
  set_attrs(x, .Environment = value)
}

#' Turn RHS of formula into a string/label.
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

#' Unwrap a formula
#'
#' This interpolates values in the formula that are defined in its
#' environment, replacing the environment with its parent.
#'
#' @export
#' @param f A formula to unwrap.
#' @examples
#' n <- 100
#' f <- ~ x + n
#' f_unwrap(f)
f_unwrap <- function(f) {
  stopifnot(is_formula(f))

  e <- environment(f)
  if (identical(e, empty_env())) {
    f
  } else {
    new_formula(f_lhs(f), substitute_(f_rhs(f), e), env_parent(e))
  }
}
