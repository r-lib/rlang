#' Create quosures
#'
#' @description
#'
#' Quosures are quoted [expressions][is_expr] that keep track of an
#' [environment][env] (just like [closure
#' functions](http://adv-r.had.co.nz/Functional-programming.html#closures)).
#' They are implemented as a subclass of one-sided formulas. They are
#' an essential piece of the tidy evaluation framework.
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
#' See the [programming with
#' dplyr](http://dplyr.tidyverse.org/articles/programming.html)
#' vignette for practical examples. For developers, the [tidy
#' evaluation](http://rlang.tidyverse.org/articles/tidy-evaluation.html)
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
#' # Like other formulas, quosures are normally self-quoting under
#' # evaluation:
#' eval(~var)
#' eval(quo(var))
#'
#' # But eval_tidy() evaluates expressions in a special environment
#' # (called the overscope) where they become promises. They
#' # self-evaluate under evaluation:
#' eval_tidy(~var)
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
#' # Quoting as a quosure is necessary to preserve scope information
#' # and make sure objects are looked up in the right place. However,
#' # there are situations where it can get in the way. This is the
#' # case when you deal with non-tidy NSE functions that do not
#' # understand formulas. You can inline the RHS of a formula in a
#' # call thanks to the UQE() operator:
#' nse_function <- function(arg) substitute(arg)
#' var <- locally(quo(foo(bar)))
#' quo(nse_function(UQ(var)))
#' quo(nse_function(UQE(var)))
#'
#' # This is equivalent to unquoting and taking the RHS:
#' quo(nse_function(!! get_expr(var)))
#'
#' # One of the most important old-style NSE function is the dollar
#' # operator. You need to use UQE() for subsetting with dollar:
#' var <- quo(cyl)
#' quo(mtcars$UQE(var))
#'
#' # `!!`() is also treated as a shortcut. It is meant for situations
#' # where the bang operator would not parse, such as subsetting with
#' # $. Since that's its main purpose, we've made it a shortcut for
#' # UQE() rather than UQ():
#' var <- quo(cyl)
#' quo(mtcars$`!!`(var))
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
  quo <- new_formula(NULL, expr, env)
  set_attrs(quo, class = c("quosure", "formula"))
}
#' @rdname quosure
#' @export
enquo <- function(arg) {
  if (missing(arg)) {
    return(new_quosure(missing_arg(), empty_env()))
  }

  capture <- lang(captureArg, substitute(arg))
  arg <- eval_bare(capture, caller_env())
  expr <- .Call(rlang_interp, arg$expr, arg$env, TRUE)
  forward_quosure(expr, arg$env)
}
forward_quosure <- function(expr, env) {
  if (is_quosure(expr)) {
    expr
  } else if (is_definition(expr)) {
    as_quosureish(expr, env)
  } else if (is_symbolic(expr)) {
    new_quosure(expr, env)
  } else {
    new_quosure(expr, empty_env())
  }
}

#' @export
print.quosure <- function(x, ...) {
  cat(paste0("<quosure: ", env_type(get_env(x)), ">\n"))
  print(set_attrs(x, NULL))
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
#' @description
#'
#' These predicates test for [quosure] objects.
#'
#' - `is_quosure()` tests for a tidyeval quosure. These are one-sided
#'   formulas with a `quosure` class.
#'
#' - `is_quosureish()` tests for general R quosure objects: quosures,
#'   or one-sided formulas.
#'
#' @param x An object to test.
#' @param scoped A boolean indicating whether the quosure or formula
#'   is scoped, that is, has a valid environment attribute. If `NULL`,
#'   the scope is not inspected.
#' @seealso [is_formula()] and [is_formulaish()]
#' @export
#' @examples
#' # Quosures are created with quo():
#' quo(foo)
#' is_quosure(quo(foo))
#'
#' # Formulas look similar to quosures but are not quosures:
#' is_quosure(~foo)
#'
#' # But they are quosureish:
#' is_quosureish(~foo)
#'
#' # Note that two-sided formulas are never quosureish:
#' is_quosureish(a ~ b)
is_quosure <- function(x) {
  inherits(x, "quosure")
}
#' @rdname is_quosure
#' @export
is_quosureish <- function(x, scoped = NULL) {
  is_formula(x, scoped = scoped, lhs = FALSE)
}
is_one_sided <- function(x, lang_sym = sym_tilde) {
  typeof(x) == "language" &&
    identical(node_car(x), lang_sym) &&
    is_null(node_cadr(node_cdr(x)))
}

#' Coerce object to quosure
#'
#' @description
#'
#' Quosure objects wrap an [expression][is_expr] with a [lexical
#' enclosure][env]. This is a powerful quoting (see [base::quote()]
#' and [quo()]) mechanism that makes it possible to carry and
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
#' @param x An object to convert.
#' @param env An environment specifying the lexical enclosure of the
#'   quosure.
#' @seealso [is_quosure()]
#' @export
#' @examples
#' # Sometimes you get unscoped formulas because of quotation:
#' f <- ~~expr
#' inner_f <- f_rhs(f)
#' str(inner_f)
#' is_quosureish(inner_f, scoped = TRUE)
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
#' @rdname as_quosure
#' @export
as_quosureish <- function(x, env = caller_env()) {
  if (is_quosureish(x)) {
    if (!is_env(f_env(x))) {
      f_env(x) <- env
    }
    x
  } else if (is_frame(x)) {
    new_quosure(x$expr, sys_frame(x$caller_pos))
  } else {
    new_quosure(get_expr(x), get_env(x, env))
  }
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
  is_missing(f_rhs(quo))
}
#' @rdname quo-predicates
#' @export
quo_is_symbol <- function(quo) {
  is_symbol(f_rhs(quo))
}
#' @rdname quo-predicates
#' @export
quo_is_lang <- function(quo) {
  is_lang(f_rhs(quo))
}
#' @rdname quo-predicates
#' @export
quo_is_symbolic <- function(quo) {
  is_symbolic(f_rhs(quo))
}
#' @rdname quo-predicates
#' @export
quo_is_null <- function(quo) {
  is_null(f_rhs(quo))
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
    quo <- f_rhs(quo)
  }
  quo_splice(duplicate(quo), warn = warn)
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

quo_splice <- function(x, parent = NULL, warn = FALSE) {
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
          x <- f_rhs(x)
        }
        if (!is_null(parent)) {
          mut_node_car(parent, x)
        }
        quo_splice(x, parent, warn = warn)
      } else {
        quo_splice(node_cdr(x), warn = warn)
      }
    },
    pairlist = {
      while(!is_null(x)) {
        quo_splice(node_car(x), x, warn = warn)
        x <- node_cdr(x)
      }
    }
  )

  x
}
