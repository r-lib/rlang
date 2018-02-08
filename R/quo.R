#' Quosure getters, setters and testers
#'
#' @description
#'
#' You can access the quosure components (its expression and its
#' environment) with:
#'
#' * [get_expr()] and [get_env()]. These getters also support other
#'   kinds of objects such as formulas
#'
#' * `quo_get_expr()` and `quo_get_env()`. These getters only work
#'   with quosures and throw an error with other types of input.
#'
#' Test if an object is a quosure with `is_quosure()`. If you know an
#' object is a quosure, use the `quo_` prefixed predicates to check
#' its contents, `quo_is_missing()`, `quo_is_symbol()`, etc.
#'
#'
#' @section Empty quosures:
#'
#' When missing arguments are captured as quosures, either through
#' [enquo()] or [quos()], they are returned as an empty quosure. These
#' quosures contain the [missing argument][missing_arg] and typically
#' have the [empty environment][empty_env] as enclosure.
#'
#'
#' @section Life cycle:
#'
#' - `is_quosure()` is stable.
#'
#' - `quo_get_expr()` and `quo_get_env()` are stable.
#'
#' - `is_quosureish()` is deprecated as of rlang 0.2.0. This function
#'   assumed that quosures are formulas which is currently true but
#'   might not be in the future.
#'
#'
#' @name quosure
#' @seealso [quo()] for creating quosures by quotation; [as_quosure()]
#'   and [new_quosure()] for constructing quosures manually.
#' @examples
#' quo <- quo(my_quosure)
#' quo
#'
#'
#' # Access and set the components of a quosure:
#' quo_get_expr(quo)
#' quo_get_env(quo)
#'
#' quo <- quo_set_expr(quo, quote(baz))
#' quo <- quo_set_env(quo, empty_env())
#' quo
#'
#' # Test wether an object is a quosure:
#' is_quosure(quo)
#'
#' # If it is a quosure, you can use the specialised type predicates
#' # to check what is inside it:
#' quo_is_symbol(quo)
#' quo_is_call(quo)
#' quo_is_null(quo)
#'
#' # quo_is_missing() checks for a special kind of quosure, the one
#' # that contains the missing argument:
#' quo()
#' quo_is_missing(quo())
#'
#' fn <- function(arg) enquo(arg)
#' fn()
#' quo_is_missing(fn())
NULL

#' @rdname quosure
#' @param x An object to test.
#' @export
is_quosure <- function(x) {
  inherits(x, "quosure")
}

#' @rdname quosure
#' @param quo A quosure to test.
#' @export
quo_is_missing <- function(quo) {
  .Call(rlang_quo_is_missing, quo)
}
#' @rdname quosure
#' @param name The name of the symbol or function call. If `NULL` the
#'   name is not tested.
#' @export
quo_is_symbol <- function(quo, name = NULL) {
  is_symbol(quo_get_expr(quo), name = name)
}
#' @rdname quosure
#' @inheritParams is_call
#' @export
quo_is_call <- function(quo, name = NULL, n = NULL, ns = NULL) {
  is_call(quo_get_expr(quo), name = name, n = n, ns = ns)
}
#' @rdname quosure
#' @export
quo_is_symbolic <- function(quo) {
  .Call(rlang_quo_is_symbolic, quo)
}
#' @rdname quosure
#' @export
quo_is_null <- function(quo) {
  .Call(rlang_quo_is_null, quo)
}


#' @rdname quosure
#' @export
quo_get_expr <- function(quo) {
  .Call(rlang_quo_get_expr, quo)
}
#' @rdname quosure
#' @export
quo_get_env <- function(quo) {
  .Call(rlang_quo_get_env, quo)
}

#' @rdname quosure
#' @param expr A new expression for the quosure.
#' @export
quo_set_expr <- function(quo, expr) {
  .Call(rlang_quo_set_expr, quo, expr)
}
#' @rdname quosure
#' @param env A new environment for the quosure.
#' @export
quo_set_env <- function(quo, env) {
  .Call(rlang_quo_set_env, quo, env)
}


#' @rdname quosure
#' @export
is_quosures <- function(x) {
  inherits(x, "quosures")
}

#' @export
`[.quosures` <- function(x, i) {
  set_attrs(NextMethod(), class = "quosures")
}
#' @export
c.quosures <- function(..., recursive = FALSE) {
  structure(NextMethod(), class = "quosures")
}
#' @export
print.quosures <- function(x, ...) {
  print(unclass(x), ...)
}


#' Coerce object to quosure
#'
#' @description
#'
#' While `new_quosure()` wraps any R object (including expressions,
#' formulas, or other quosures) into a quosure, `as_quosure()`
#' converts formulas and quosures and does not double-wrap.
#'
#'
#' @section Life cycle:
#'
#' - Like the rest of the rlang package, `new_quosure()` and
#'   `as_quosure()` are maturing.
#'
#' - `as_quosureish()` is deprecated as of rlang 0.2.0. This function
#'   assumes that quosures are formulas which is currently true but
#'   might not be in the future.
#'
#' @param x An object to convert. Either an [expression][is_expression] or a
#'   formula.
#' @param env The original context of the context expression.
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
#' @rdname as_quosure
#' @param expr The expression wrapped by the quosure.
#' @export
new_quosure <- function(expr, env = caller_env()) {
  .Call(rlang_new_quosure, expr, env)
}


#' Squash a quosure
#'
#' @description
#'
#' `quo_squash()` flattens all nested quosures within an expression.
#' For example it transforms `^foo(^bar(), ^baz)` to the bare
#' expression `foo(bar(), baz)`.
#'
#' This operation is safe if the squashed quosure is used for
#' labelling or printing (see [quo_label()] or [quo_name()]). However
#' if the squashed quosure is evaluated, all expressions of the
#' flattened quosures are resolved in a single environment. This is a
#' source of bugs so it is good practice to set `warn` to `TRUE` to
#' let the user know about the lossy squashing.
#'
#'
#' @section Life cycle:
#'
#' This function replaces `quo_expr()` which was soft-deprecated in
#' rlang 0.2.0. `quo_expr()` was a misnomer because it implied that it
#' was a mere expression acccessor for quosures whereas it was really
#' a lossy operation that squashed all nested quosures.
#'
#'
#' @param quo A quosure or expression.
#' @param warn Whether to warn if the quosure contains other quosures
#'   (those will be collapsed). This is useful when you use
#'   `quo_squash()` in order to make a non-tidyeval API compatible
#'   with quosures. In that case, getting rid of the nested quosures
#'   is likely to cause subtle bugs and it is good practice to warn
#'   the user about it.
#'
#' @export
#' @examples
#' # Quosures can contain nested quosures:
#' quo <- quo(wrapper(!!quo(wrappee)))
#' quo
#'
#' # quo_squash() flattens all the quosures and returns a simple expression:
#' quo_squash(quo)
quo_squash <- function(quo, warn = FALSE) {
  # Never warn when unwrapping outer quosure
  if (is_quosure(quo)) {
    quo <- quo_get_expr(quo)
  }
  if (is_missing(quo)) {
    missing_arg()
  } else {
    quo_squash_impl(duplicate(quo), warn = warn)
  }
}


#' Format quosures for printing or labelling
#'
#' @description
#'
#' * `quo_text()` and `quo_label()` are equivalent to [expr_text()],
#'   [expr_label()], etc, but they first squash all quosures with
#'   [quo_squash()] so they print more nicely.
#'
#' * `quo_name()` squashes a quosure and transforms it into a simple
#'   string. It is suitable to give an unnamed quosure a default name,
#'   for instance a column name in a data frame.
#'
#' @inheritParams quo_squash
#' @inheritParams expr_label
#' @export
#' @seealso [expr_label()], [f_label()]
#' @examples
#' # Quosures can contain nested quosures:
#' quo <- quo(foo(!! quo(bar)))
#' quo
#'
#' # quo_squash() unwraps all quosures and returns a raw expression:
#' quo_squash(quo)
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
quo_label <- function(quo) {
  expr_label(quo_squash(quo))
}
#' @rdname quo_label
#' @export
quo_text <- function(quo, width = 60L, nlines = Inf) {
  expr_text(quo_squash(quo), width = width, nlines = nlines)
}
#' @rdname quo_label
#' @export
quo_name <- function(quo) {
  expr_name(quo_squash(quo))
}

quo_squash_impl <- function(x, parent = NULL, warn = FALSE) {
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
          node_poke_car(parent, x)
        }
        quo_squash_impl(x, parent, warn = warn)
      } else {
        quo_squash_impl(node_cdr(x), warn = warn)
      }
    },
    pairlist = {
      while (!is_null(x)) {
        quo_squash_impl(node_car(x), x, warn = warn)
        x <- node_cdr(x)
      }
    }
  )

  x
}


#' @export
print.quosure <- function(x, ...) {
  meow(.trailing = FALSE,
    "<quosure>",
    "  expr: "
  )
  quo_print(x)
  meow(.trailing = FALSE,
    "  env:  "
  )

  env <- get_env(x)
  quo_env_print(env)

  invisible(x)
}
#' @export
str.quosure <- function(object, ...) {
  str(unclass(object), ...)
}

# Create a circular list of colours. This infloops if printed in the REPL!
new_quo_palette <- function() {
  last_node <- new_node(open_cyan, NULL)
  palette <- new_node(open_blue, new_node(open_green, new_node(open_magenta, last_node)))
  node_poke_cdr(last_node, palette)

  # First node has no colour
  new_node(close_colour, palette)
}

# Reproduces output of printed calls
base_deparse <- function(x) {
  deparse(x, control = "keepInteger")
}

quo_deparse <- function(x, lines = new_quo_deparser()) {
  if (!is_quosure(x)) {
    return(sexp_deparse(x, lines = lines))
  }

  env <- quo_get_env(x)
  lines$quo_open_colour(env)

  lines$push("^")
  lines$make_next_sticky()
  sexp_deparse(quo_get_expr(x), lines)

  lines$quo_reset_colour()

  lines$get_lines()
}

new_quo_deparser <- function(width = peek_option("width"),
                             crayon = has_crayon()) {
  lines <- new_lines(width = width, deparser = quo_deparse)

  child_r6lite(lines,
    has_colour = crayon,

    quo_envs = list(),
    quo_history = pairlist(),
    quo_colours = list(
      open_blue,
      open_green,
      open_magenta,
      open_cyan,
      open_yellow
    ),
    quo_was_too_many = FALSE,

    quo_push_opener = function(self, opener) {
      self$quo_history <- new_node(opener, self$quo_history)
      self$push_sticky(opener())
      self
    },

    quo_open_colour = function(self, env) {
      if (self$has_colour) {
        if (is_reference(env, global_env()) || is_reference(env, empty_env())) {
          self$quo_push_opener(close_colour)
          return(NULL)
        }

        n_known_envs <- length(self$quo_envs)

        idx <- detect_index(self$quo_envs, identical, env)
        if (idx) {
          opener <- self$quo_colours[[idx]]
        } else if (n_known_envs < length(self$quo_colours)) {
          self$quo_envs <- c(self$quo_envs, list(env))
          idx <- n_known_envs + 1L
          opener <- self$quo_colours[[idx]]
        } else {
          opener <- function() paste0(close_colour(), open_blurred_italic())
          self$quo_was_too_many <- TRUE
        }

        self$quo_push_opener(opener)
      }
    },

    quo_reset_colour = function(self) {
      if (self$has_colour) {
        if (self$quo_was_too_many) {
          self$push_sticky(close_blurred_italic())
        }
        self$quo_history <- node_cdr(self$quo_history)
        reset <- node_car(self$quo_history) %||% close_colour
        self$push_sticky(reset())
      }
    }
  )
}

quo_print <- function(quo) {
  # Take into account the first 8-character wide columns
  width <- peek_option("width") - 10L
  deparser <- new_quo_deparser(width = width)

  lines <- quo_deparse(quo, deparser)

  n <- length(lines)
  lines[seq2(2, n)] <- paste0("       ", lines[seq2(2, n)])

  cat(paste0(lines, "\n"))
}
quo_env_print <- function(env) {
  if (is_reference(env, global_env())) {
    nm <- "global"
  } else if (is_reference(env, empty_env())) {
    nm <- "empty"
  } else {
    nm <- blue(sxp_address(env))
  }
  meow(nm)
}
