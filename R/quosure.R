#' Is an object a quosure or quosure-like?
#'
#' @description
#'
#' These predicates test for [quosure][new_quosure] objects.
#'
#' - `is_quosure()` tests for the canonical R quosure: the one-sided
#'   "formula".
#'
#' - `is_quosureish()` tests for general R quosure objects: quosures,
#'   two-sided formulas, and [definitions][op-definition].
#'
#'
#' @details
#'
#' The `scoped` argument patterns-match on whether the scoped bundled
#' with the quosure is valid or not. Invalid scopes may happen in
#' nested quotations like `~~expr`, where the outer quosure is validly
#' scoped but not the inner one. This is because `~` saves the
#' environment when it is evaluated, and quoted quosures are by
#' definition not evaluated. Note that in the [tidy evaluation
#' framework][eval_tidy], unscoped quosures are automatically given
#' the environment of the outer quosure during the evaluation process.
#'
#' @param x An object to test.
#' @param scoped A boolean indicating whether the quosure or formula
#'   is scoped, that is, has a valid environment attribute. If `NULL`,
#'   the scope is not inspected.
#' @seealso [as_quosure()][new_quosure] and [new_quosure()] for creating
#'   quosures, and [quosure()] or [eval_tidy()] for information
#'   about the role of quosures in the tidy evaluation framework.
#' @export
#' @examples
#' # Degenerate quosures are often created by quoting, since `~`
#' # records the environment when it is evaluated the first time:
#' f <- ~~expr
#'
#' # The outer quosure has been evaluated and is scoped:
#' is_quosure(f, scoped = TRUE)
#'
#' # But the inner formula is not:
#' inner_f <- f_rhs(f)
#' is_quosure(inner_f, scoped = TRUE)
#'
#'
#' # Formulas and definitions are not quosures:
#' is_quosure(a := b)
#' is_quosure(a ~ b)
#'
#' # But they are quosureish objects:
#' is_quosureish(a := b)
#' is_quosureish(a ~ b)
is_quosure <- function(x, scoped = NULL) {
  if (!is_one_sided(x)) {
    return(FALSE)
  }
  if (!is_null(scoped) && scoped != is_env(f_env(x))) {
    return(FALSE)
  }
  TRUE
}
#' @rdname is_quosure
#' @export
is_quosureish <- function(x, scoped = NULL) {
  if (!is_formula(x)) {
    return(FALSE)
  }
  if (!is_null(scoped) && scoped != is_env(f_env(x))) {
    return(FALSE)
  }
  TRUE
}
is_one_sided <- function(x, lang_sym = sym_tilde) {
  typeof(x) == "language" &&
    identical(node_car(x), lang_sym) &&
    is_null(node_cadr(node_cdr(x)))
}


#' Create quosures.
#'
#' @description
#'
#' Quosure objects wrap an [expression][is_expr] with a [lexical
#' enclosure][env]. This is a powerful quoting (see [base::quote()]
#' and [quosure()]) mechanism that makes it possible to carry and
#' manipulate expressions while making sure that its symbolic content
#' (symbols and named calls, see [is_symbolic()]) is correctly looked
#' up during evaluation.
#'
#' - `new_quosure()` creates a quosure from a raw expression and an
#'   environment.
#'
#' - `as_quosure()` is useful for functions that expect quosures but
#'   allow specifying a raw expression as well. It has two possible
#'   effects: if `x` is not a quosure, it wraps it into a quosure
#'   bundling `env` as scope. If `x` is an unscoped quosure (see
#'   [is_quosure()]), `env` is used as a default scope. On the other
#'   hand if `x` has a valid enclosure, it is returned as is (even if
#'   `env` is not the same as the formula environment).
#'
#' - While `as_quosure()` always returns a quosure (a one-sided
#'   formula), even when its input is a [formula][new_formula] or a
#'   [definition][op-definition], `as_quosureish()` returns quosureish
#'   inputs as is.
#'
#' @inheritParams new_formula
#' @param x An object to convert.
#' @param env An environment specifying the lexical enclosure of the
#'   quosure.
#' @seealso [is_quosure()]
#' @export
#' @examples
#' f <- new_quosure(quote(mtcars), get_env("datasets"))
#' f
#' eval_tidy(f)
#'
#'
#' # Sometimes you get unscoped quosures because of quotation:
#' f <- ~~expr
#' inner_f <- f_rhs(f)
#' inner_f
#' is_quosure(inner_f, scoped = TRUE)
#'
#' # You can use as_quosure() to provide a default environment:
#' as_quosure(inner_f, base_env())
#'
#' # Or convert expressions or any R object to a validly scoped quosure:
#' as_quosure(quote(expr), base_env())
#' as_quosure(10L, base_env())
#'
#'
#' # While as_quosure() always returns a quosure (one-sided formula),
#' # as_quosureish() returns quosureish objects:
#' as_quosure(a := b)
#' as_quosureish(a := b)
#' as_quosureish(10L)
new_quosure <- function(rhs, env = caller_env()) {
  quo <- new_formula(NULL, rhs, env)
  struct(quo, class = c("quosure", "formula"))
}
#' @rdname new_quosure
#' @export
as_quosure <- function(x, env = caller_env()) {
  if (is_quosure(x)) {
    if (!is_env(f_env(x))) {
      f_env(x) <- env
    }
    struct(x, class = c("quosure", "formula"))
  } else if (is_quosureish(x)) {
    if (!is_env(f_env(x))) {
      f_env(x) <- env
    }
    new_quosure(f_rhs(x), env)
  } else if (is_frame(x)) {
    new_quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    new_quosure(x, env)
  }
}
#' @export
print.quosure <- function(x, ...) {
  x <- struct(x, class = "formula")
  NextMethod()
}

#' @rdname new_quosure
#' @export
as_quosureish <- function(x, env = caller_env()) {
  if (is_quosure(x)) {
    if (!is_env(f_env(x))) {
      f_env(x) <- env
    }
    x
  } else if (is_quosureish(x)) {
    if (!is_env(f_env(x))) {
      f_env(x) <- env
    }
    x
  } else if (is_frame(x)) {
    new_quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    new_quosure(x, env)
  }
}

#' Splice a quosure and format it into string or label.
#'
#' `quo_expr()` flattens all quosures within an expression. I.e., it
#' turns `~foo(~bar(), ~baz)` to `foo(bar(), baz)`. `quo_text()` and
#' `quo_label()` are equivalent to [f_text()], [expr_label()], etc,
#' but they first splice their argument using `quo_expr()`.
#'
#' @inheritParams expr_label
#' @param quo A quosure or expression.
#' @export
#' @seealso [expr_label()], [f_label()]
#' @examples
#' quo_expr(~foo(~bar))
#' quo_text(~foo(~bar))
quo_expr <- function(quo) {
  quo_splice(duplicate(quo))
}
#' @rdname quo_expr
#' @export
quo_label <- function(quo) {
  expr_label(quo_expr(quo))
}
#' @rdname quo_expr
#' @export
quo_text <- function(quo, width = 60L, nlines = Inf) {
  expr_text(quo_expr(quo), width = width, nlines = nlines)
}

quo_splice <- function(x, parent = NULL) {
  switch_expr(x,
    language = {
      if (is_quosure(x)) {
        while (is_quosure(x)) {
          x <- f_rhs(x)
        }
        if (!is_null(parent)) {
          mut_node_car(parent, x)
        }
        quo_splice(x, parent)
      } else {
        quo_splice(node_cdr(x))
      }
    },
    pairlist = {
      while(!is_null(x)) {
        quo_splice(node_car(x), x)
        x <- node_cdr(x)
      }
    }
  )

  x
}
