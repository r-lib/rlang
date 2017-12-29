#' Create quosures
#'
#' @description
#'
#' Quosures are quoted [expressions][is_expr] that keep track of an
#' [environment][env] (just like
#' [closures](http://adv-r.had.co.nz/Functional-programming.html#closures)).
#' They are an essential piece of the tidy evaluation framework.
#'
#' - `quo()` quotes its input (i.e. captures R code without
#'   evaluation), captures the current environment, and bundles them
#'   in a quosure.
#'
#' - `enquo()` takes a symbol referring to a function argument, quotes
#'   the R code that was supplied to this argument, captures the
#'   environment where the function was called (and thus where the R
#'   code was typed), and bundles them in a quosure.
#'
#' - [quos()] is a bit different to other functions as it returns a
#'   list of quosures. You can supply several expressions directly,
#'   e.g. `quos(foo, bar)`, but more importantly you can also supply
#'   dots: `quos(...)`. In the latter case, expressions forwarded
#'   through dots are captured and transformed to quosures. The
#'   environments bundled in those quosures are the ones where the
#'   code was supplied as arguments, even if the dots were forwarded
#'   multiple times across several function calls.
#'
#' - `new_quosure()` is the only constructor that takes its arguments
#'   by value. It lets you create a quosure from an expression and an
#'   environment.
#'
#' @section Role of quosures for tidy evaluation:
#'
#' Quosures play an essential role thanks to these features:
#'
#' - They allow consistent scoping of quoted expressions by recording
#'   an expression along with its local environment.
#'
#' - `quo()`, `quos()` and `enquo()` all support [quasiquotation]. By
#'   unquoting other quosures, you can safely combine expressions even
#'   when they come from different contexts. You can also unquote
#'   values and raw expressions depending on your needs.
#'
#' - Unlike formulas, quosures self-evaluate (see [eval_tidy()])
#'   within their own environment, which is why you can unquote a
#'   quosure inside another quosure and evaluate it like you've
#'   unquoted a raw expression.
#'
#' See the
#' [programming with dplyr](http://dplyr.tidyverse.org/articles/programming.html)
#' vignette for practical examples. For developers, the
#' [tidy evaluation](http://rlang.tidyverse.org/articles/tidy-evaluation.html)
#' vignette provides an overview of this approach. The
#' [quasiquotation] page goes in detail over the unquoting and
#' splicing operators.
#'
#' @param expr An expression.
#' @param arg A symbol referring to an argument. The expression
#'   supplied to that argument will be captured unevaluated.
#' @return A formula whose right-hand side contains the quoted
#'   expression supplied as argument.
#' @seealso [expr()] for quoting a raw expression with quasiquotation.
#'   The [quasiquotation] page goes over unquoting and splicing.
#' @export
#' @examples
#' # quo() is a quotation function just like expr() and quote():
#' expr(mean(1:10 * 2))
#' quo(mean(1:10 * 2))
#'
#' # It supports quasiquotation and allows unquoting (evaluating
#' # immediately) part of the quoted expression:
#' quo(mean(!! 1:10 * 2))
#'
#' # What makes quo() often safer to use than quote() and expr() is
#' # that it keeps track of the contextual environment. This is
#' # especially important if you're referring to local variables in
#' # the expression:
#' var <- "foo"
#' quo <- quo(var)
#' quo
#'
#' # Here `quo` quotes `var`. Let's check that it also captures the
#' # environment where that symbol is defined:
#' identical(get_env(quo), get_env())
#' env_has(quo, "var")
#'
#'
#' # Keeping track of the environment is important when you quote an
#' # expression in a context (that is, a particular function frame)
#' # and pass it around to other functions (which will be run in their
#' # own evaluation frame):
#' fn <- function() {
#'   foobar <- 10
#'   quo(foobar * 2)
#' }
#' quo <- fn()
#' quo
#'
#' # `foobar` is not defined here but was defined in `fn()`'s
#' # evaluation frame. However, the quosure keeps track of that frame
#' # and is safe to evaluate:
#' eval_tidy(quo)
#'
#'
#' # With eval_tidy() quosures self-evaluate under evaluation:
#' var
#' eval_tidy(quo(var))
#'
#' # Note that it's perfectly fine to unquote quosures within
#' # quosures, as long as you evaluate with eval_tidy():
#' quo <- quo(letters)
#' quo <- quo(toupper(!! quo))
#' quo
#' eval_tidy(quo)
#'
#'
#' # The `!!` operator can be used in prefix form as well. This is
#' # useful for using it with `$` for instance:
#' var <- sym("cyl")
#' quo(mtcars$`!!`(var))
#'
#' # Or equivalently:
#' quo(`$`(mtcars, !!var))
#'
#' # Quoting as a quosure is necessary to preserve scope information
#' # and make sure objects are looked up in the right place. However,
#' # there are situations where it can get in the way. This is the
#' # case when you deal with base NSE functions that do not understand
#' # quosures. You can extract the expression from a quosure with
#' # `get_expr()`.
#' get_expr(quo(foo))
#'
#' # To use `get_expr()` safely it is important that the expression
#' # within the quosure does not depend on any objects local to its
#' # place of creation. For instance if the quosure contains a symbol
#' # that refers to a data frame column it is safe to use the raw
#' # symbol. Let's try to use `get_expr()` with `$`:
#' var <- quo(cyl)
#' quo(mtcars$`!!`(get_expr(var)))
#'
#'
#' # When a quosure is printed in the console, the brackets indicate
#' # if the enclosure is the global environment or a local one:
#' locally(quo(foo))
#'
#' # Literals are enquosed with the empty environment because they can
#' # be evaluated anywhere. The brackets indicate "empty":
#' quo(10L)
#'
#' # To differentiate local environments, use str(). It prints the
#' # machine address of the environment:
#' quo1 <- locally(quo(foo))
#' quo2 <- locally(quo(foo))
#' quo1; quo2
#' str(quo1); str(quo2)
#'
#' # You can also see this address by printing the environment at the
#' # console:
#' get_env(quo1)
#' get_env(quo2)
#'
#'
#' # new_quosure() takes by value an expression that is already quoted:
#' expr <- quote(mtcars)
#' env <- as_env("datasets")
#' quo <- new_quosure(expr, env)
#' quo
#' eval_tidy(quo)
#' @name quosure
quo <- function(expr) {
  enquo(expr)
}
#' @rdname quosure
#' @inheritParams as_quosure
#' @export
new_quosure <- function(expr, env = caller_env()) {
  .Call(rlang_new_quosure, expr, env)
}
#' @rdname quosure
#' @export
enquo <- function(arg) {
  .Call(rlang_enquo, substitute(arg), parent.frame())
}

#' @export
print.quosure <- function(x, ...) {
  quo_print(x)

  env <- get_env(x)
  if (!is_reference(env, global_env())) {
    print(env)
  }

  invisible(x)
}
#' @export
str.quosure <- function(object, ...) {
  env_type <- env_format(get_env(object))

  cat(paste0("<quosure: ", env_type, ">\n"))
  print(set_attrs(object, NULL))
  invisible(object)
}

#' Is an object a quosure or quosure-like?
#'
#' This predicate tests that an object is a [quosure].
#'
#'
#' @section Life cycle:
#'
#' - `is_quosureish()` is deprecated as of rlang 0.2.0. This function
#'   assumed that quosures are formulas which is currently true but
#'   might not be in the future.
#'
#' @param x An object to test.
#' @param scoped A boolean indicating whether the quosure is scoped,
#'   that is, has a valid environment attribute. If `NULL`, the scope
#'   is not inspected.
#' @export
#' @examples
#' is_quosure(quo(foo))
is_quosure <- function(x) {
  inherits(x, "quosure")
}

#' Coerce object to quosure
#'
#' @description
#'
#' While [new_quosure()] wraps any R object (including expressions,
#' formulas, or other quosures) into a quosure, `as_quosure()`
#' converts formulas and quosures and does not double-wrap.
#'
#'
#' @section Life cycle:
#'
#' - `as_quosureish()` is deprecated as of rlang 0.2.0. This function
#'   assumes that quosures are formulas which is currently true but
#'   might not be in the future.
#'
#' @param x An object to convert. Either an [expression][is_expr] or a
#'   formula.
#' @param env An environment specifying the lexical enclosure of the
#'   quosure.
#' @seealso [quo()], [is_quosure()]
#' @export
#' @examples
#' # as_quosure() converts expressions or any R object to a validly
#' # scoped quosure:
#' as_quosure(quote(expr), base_env())
#' as_quosure(10L, base_env())
#'
#'
#' # Sometimes you get unscoped formulas because of quotation:
#' f <- ~~expr
#' inner_f <- f_rhs(f)
#' str(inner_f)
#'
#' # In that case testing for a scoped formula returns FALSE:
#' is_formula(inner_f, scoped = TRUE)
#'
#' # With as_quosure() you ensure that this kind of unscoped formulas
#' # will be granted a default environment:
#' as_quosure(inner_f, base_env())
as_quosure <- function(x, env = caller_env()) {
  if (is_quosure(x)) {
    x
  } else if (is_bare_formula(x)) {
    new_quosure(f_rhs(x), f_env(x) %||% env)
  } else if (is_symbolic(x)) {
    new_quosure(x, env)
  } else {
    new_quosure(x, empty_env())
  }
}


#' Get or set the components of a quosure
#'
#' These functions are equivalent to [get_expr()], [get_env()],
#' [set_expr()], and [set_env()]. However they only work on quosures
#' and are a bit more efficient.
#'
#' @param quo A quosure.
#'
#' @export
#' @examples
#' quo <- quo(foo(bar))
#' quo
#'
#' quo_set_expr(quo, quote(baz))
#' quo_set_env(quo, empty_env())
quo_get_expr <- function(quo) {
  if (typeof(quo) != "language") {
    abort("Expected a quosure")
  }
  node_cadr(quo)
}
#' @rdname quo_get_expr
#' @export
quo_get_env <- function(quo) {
  if (typeof(quo) != "language") {
    abort("Expected a quosure")
  }
  attr(quo, ".Environment")
}

#' @rdname quo_get_expr
#' @param expr A new expression for the quosure.
#' @export
quo_set_expr <- function(quo, expr) {
  if (typeof(quo) != "language") {
    abort("Expected a quosure")
  }
  if (is_null(node_cdr(quo))) {
    abort("Corrupt quosure")
  }

  quo <- duplicate(quo, shallow = TRUE)
  node_poke_cadr(quo, expr)
  quo
}
#' @rdname quo_get_expr
#' @param env A new environment for the quosure.
#' @export
quo_set_env <- function(quo, env) {
  if (!is_environment(env)) {
    abort("`env` must be an environment")
  }
  if (typeof(quo) != "language") {
    abort("Expected a quosure")
  }

  attr(quo, ".Environment") <- env
  quo
}


#' Is a quosure quoting a symbolic, missing or NULL object?
#'
#' These functions examine the expression of a quosure with a
#' predicate.
#'
#' @section Empty quosures:
#'
#' When missing arguments are captured as quosures, either through
#' [enquo()] or [quos()], they are returned as an empty quosure. These
#' quosures contain the [missing argument][missing_arg] and typically
#' have the [empty environment][empty_env] as enclosure.
#'
#' @param quo A quosure.
#' @examples
#' quo_is_symbol(quo(sym))
#' quo_is_symbol(quo(foo(bar)))
#'
#' # You can create empty quosures by calling quo() without input:
#' quo <- quo()
#' quo_is_missing(quo)
#' is_missing(f_rhs(quo))
#' @name quo-predicates
NULL

#' @rdname quo-predicates
#' @export
quo_is_missing <- function(quo) {
  .Call(rlang_quo_is_missing, quo)
}
#' @rdname quo-predicates
#' @export
quo_is_symbol <- function(quo) {
  .Call(rlang_quo_is_symbol, quo)
}
#' @rdname quo-predicates
#' @export
quo_is_lang <- function(quo) {
  .Call(rlang_quo_is_call, quo)
}
#' @rdname quo-predicates
#' @export
quo_is_symbolic <- function(quo) {
  .Call(rlang_quo_is_symbolic, quo)
}
#' @rdname quo-predicates
#' @export
quo_is_null <- function(quo) {
  .Call(rlang_quo_is_null, quo)
}


#' Splice a quosure and format it into string or label
#'
#' `quo_expr()` flattens all quosures within an expression. I.e., it
#' turns `~foo(~bar(), ~baz)` to `foo(bar(), baz)`. `quo_text()` and
#' `quo_label()` are equivalent to [f_text()], [expr_label()], etc,
#' but they first splice their argument using `quo_expr()`.
#' `quo_name()` transforms a quoted symbol to a string. It adds a bit
#' more intent and type checking than simply calling `quo_text()` on
#' the quoted symbol (which will work but won't return an error if not
#' a symbol).
#'
#' @inheritParams expr_label
#' @param quo A quosure or expression.
#' @param warn Whether to warn if the quosure contains other quosures
#'   (those will be collapsed).
#' @export
#' @seealso [expr_label()], [f_label()]
#' @examples
#' quo <- quo(foo(!! quo(bar)))
#' quo
#'
#' # quo_expr() unwraps all quosures and returns a raw expression:
#' quo_expr(quo)
#'
#' # This is used by quo_text() and quo_label():
#' quo_text(quo)
#'
#' # Compare to the unwrapped expression:
#' expr_text(quo)
#'
#' # quo_name() is helpful when you need really short labels:
#' quo_name(quo(sym))
#' quo_name(quo(!! sym))
quo_expr <- function(quo, warn = FALSE) {
  # Never warn when unwrapping outer quosure
  if (is_quosure(quo)) {
    quo <- quo_get_expr(quo)
  }
  if (is_missing(quo)) {
    missing_arg()
  } else {
    quo_flatten(duplicate(quo), warn = warn)
  }
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
#' @rdname quo_expr
#' @export
quo_name <- function(quo) {
  expr_name(quo_expr(quo))
}

quo_flatten <- function(x, parent = NULL, warn = FALSE) {
  switch_expr(x,
    language = {
      if (is_quosure(x)) {
        if (!is_false(warn)) {
          if (is_string(warn)) {
            msg <- warn
          } else {
            msg <- "Collapsing inner quosure"
          }
          warn(msg)
          warn <- FALSE
        }

        while (is_quosure(x)) {
          x <- quo_get_expr(x)
        }
        if (!is_null(parent)) {
          mut_node_car(parent, x)
        }
        quo_flatten(x, parent, warn = warn)
      } else {
        quo_flatten(node_cdr(x), warn = warn)
      }
    },
    pairlist = {
      while (!is_null(x)) {
        quo_flatten(node_car(x), x, warn = warn)
        x <- node_cdr(x)
      }
    }
  )

  x
}

quo_print <- function(x, parent = FALSE) {
  switch_expr(x,
    language = {
      if (is_quosure(x)) {
        while (is_quosure(x)) {
          cat("^")
          x <- quo_get_expr(x)
        }
        quo_print(x, parent = parent)
      } else {
        quo_print(node_car(x), parent = TRUE)
        cat("(")
        quo_print_args(node_cdr(x))
        cat(")")
        if (!parent) {
          cat("\n")
        }
      }
    }, {
      cat(deparse(x))
      if (!parent) {
        cat("\n")
      }
    }
  )
  invisible(x)
}

quo_print_args <- function(x) {
  while (!is_null(x)) {
    quo_print(node_car(x), parent = TRUE)
    x <- node_cdr(x)
    if (!is_null(x)) {
      cat(", ")
    }
  }
}
