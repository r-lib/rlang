#' Quosure getters, setters and predicates
#'
#' @description
#' These tools inspect and modify [quosures][topic-quosure], a type of
#' [defused expression][topic-defuse] that includes a reference to the
#' context where it was created. A quosure is guaranteed to evaluate
#' in its original environment and can refer to local objects safely.
#'
#' -  You can access the quosure components with `quo_get_expr()` and
#'   `quo_get_env()`.
#'
#' - The `quo_` prefixed predicates test the expression of a quosure,
#'   `quo_is_missing()`, `quo_is_symbol()`, etc.
#'
#' All `quo_` prefixed functions expect a quosure and will fail if
#' supplied another type of object. Make sure the input is a quosure
#' with [is_quosure()].
#'
#' @section Empty quosures and missing arguments:
#' When missing arguments are captured as quosures, either through
#' [enquo()] or [quos()], they are returned as an empty quosure. These
#' quosures contain the [missing argument][missing_arg] and typically
#' have the [empty environment][empty_env] as enclosure.
#'
#' Use `quo_is_missing()` to test for a missing argument defused with
#' [enquo()].
#'
#' @seealso
#' * [quo()] for creating quosures by [argument defusal][topic-defuse].
#' * [new_quosure()] and [as_quosure()] for assembling quosures from
#'   components.
#' * `r link("topic_quosure")` for an overview.
#'
#' @name quosure-tools
#' @aliases quosure
#'
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

#' @rdname quosure-tools
#' @param quo A quosure to test.
#' @export
quo_is_missing <- function(quo) {
  .Call(ffi_quo_is_missing, quo)
}
#' @rdname quosure-tools
#' @param name The name of the symbol or function call. If `NULL` the
#'   name is not tested.
#' @export
quo_is_symbol <- function(quo, name = NULL) {
  is_symbol(quo_get_expr(quo), name = name)
}
#' @rdname quosure-tools
#' @inheritParams is_call
#' @export
quo_is_call <- function(quo, name = NULL, n = NULL, ns = NULL) {
  is_call(quo_get_expr(quo), name = name, n = n, ns = ns)
}
#' @rdname quosure-tools
#' @export
quo_is_symbolic <- function(quo) {
  .Call(ffi_quo_is_symbolic, quo)
}
#' @rdname quosure-tools
#' @export
quo_is_null <- function(quo) {
  .Call(ffi_quo_is_null, quo)
}

#' @rdname quosure-tools
#' @export
quo_get_expr <- function(quo) {
  .Call(ffi_quo_get_expr, quo)
}
#' @rdname quosure-tools
#' @export
quo_get_env <- function(quo) {
  .Call(ffi_quo_get_env, quo)
}

#' @rdname quosure-tools
#' @param expr A new expression for the quosure.
#' @export
quo_set_expr <- function(quo, expr) {
  .Call(ffi_quo_set_expr, quo, expr)
}
#' @rdname quosure-tools
#' @param env A new environment for the quosure.
#' @export
quo_set_env <- function(quo, env) {
  .Call(ffi_quo_set_env, quo, env)
}


#' Create a quosure from components
#'
#' @description
#' * `new_quosure()` wraps any R object (including expressions,
#'   formulas, or other quosures) into a [quosure][topic-quosure].
#'
#' * `as_quosure()` is similar but it does not rewrap formulas and
#'   quosures.
#'
#' @param x For `is_quosure()`, an object to test. For `as_quosure()`,
#'   an object to convert.
#' @param expr An expression to wrap in a quosure.
#' @param env The environment in which the expression should be
#'   evaluated. Only used for symbols and calls. This should normally
#'   be the environment in which the expression was created.
#'
#' @seealso
#' * [enquo()] and [quo()] for creating a quosure by [argument
#'   defusal][topic-defuse].
#'
#' * `r link("topic_quosure")`
#' @examples
#' # `new_quosure()` creates a quosure from its components. These are
#' # equivalent:
#' new_quosure(quote(foo), current_env())
#'
#' quo(foo)
#'
#' # `new_quosure()` always rewraps its input into a new quosure, even
#' # if the input is itself a quosure:
#' new_quosure(quo(foo))
#'
#' # This is unlike `as_quosure()` which preserves its input if it's
#' # already a quosure:
#' as_quosure(quo(foo))
#'
#'
#' # `as_quosure()` uses the supplied environment with naked expressions:
#' env <- env(var = "thing")
#' as_quosure(quote(var), env)
#'
#' # If the expression already carries an environment, this
#' # environment is preserved. This is the case for formulas and
#' # quosures:
#' as_quosure(~foo, env)
#'
#' as_quosure(~foo)
#'
#' # An environment must be supplied when the input is a naked
#' # expression:
#' try(
#'   as_quosure(quote(var))
#' )
#' @export
new_quosure <- function(expr, env = caller_env()) {
  .Call(ffi_new_quosure, expr, env)
}
#' @rdname new_quosure
#' @export
as_quosure <- function(x, env = NULL) {
  if (is_quosure(x)) {
    return(x)
  }

  if (is_bare_formula(x)) {
    env <- f_env(x)
    if (is_null(env)) {
      abort(paste_line(
        "The formula does not have an environment.",
        "This is a quoted formula that was never evaluated."
      ))
    }

    return(new_quosure(f_rhs(x), env))
  }

  if (is_symbolic(x)) {
    if (is_null(env)) {
      deprecate_warn(paste_line(
        "`as_quosure()` requires an explicit environment as of rlang 0.3.0.",
        "Please supply `env`."
      ))
      env <- caller_env()
    }

    return(new_quosure(x, env))
  }

  new_quosure(x, empty_env())
}
#' @rdname new_quosure
#' @param x An object to test.
#' @export
is_quosure <- function(x) {
  inherits(x, "quosure")
}


#' Create a list of quosures
#'
#' @description
#'
#' This small S3 class provides methods for `[` and `c()` and ensures
#' the following invariants:
#'
#' * The list only contains quosures.
#' * It is always named, possibly with a vector of empty strings.
#'
#' `new_quosures()` takes a list of quosures and adds the `quosures`
#' class and a vector of empty names if needed. `as_quosures()` calls
#' [as_quosure()] on all elements before creating the `quosures`
#' object.
#'
#' @param x A list of quosures or objects to coerce to quosures.
#' @param env The default environment for the new quosures.
#' @param named Whether to name the list with [quos_auto_name()].
#' @export
new_quosures <- function(x) {
  if (!is_list(x) || !every(x, is_quosure)) {
    abort("Expected a list of quosures")
  }
  structure(x,
    class = c("quosures", "list"),
    names = names2(x)
  )
}
#' @rdname new_quosures
#' @export
as_quosures <- function(x, env, named = FALSE) {
  x <- map(x, as_quosure, env = env)
  if (named) {
    x <- quos_auto_name(x)
  }
  new_quosures(x)
}
#' @rdname new_quosures
#' @export
is_quosures <- function(x) {
  inherits(x, "quosures")
}

#' @export
`[.quosures` <- function(x, i) {
  structure(NextMethod(), class = c("quosures", "list"))
}
#' @export
c.quosures <- function(..., recursive = FALSE) {
  out <- NextMethod()
  if (every(out, is_quosure)) {
    new_quosures(out)
  } else {
    deprecate_warn(paste_line(
      "Quosure lists can't be concatenated with objects other than quosures as of rlang 0.3.0.",
      "Please call `as.list()` on the quosure list first."
    ))
    out
  }
}
#' @export
print.quosures <- function(x, ...) {
  cat_line("<list_of<quosure>>\n")
  print(unclass(x), ...)
}
#' @export
as.list.quosures <- function(x, ...) {
  unclass(x)
}

#' @export
`[<-.quosures` <- function(x, i, value) {
  if (idx <- detect_index(value, negate(is_quosure))) {
    signal_quosure_assign(value[[idx]])
  }
  NextMethod()
}
#' @export
`[[<-.quosures` <- function(x, i, value) {
  if (!is_quosure(value) && !is_null(value)) {
    signal_quosure_assign(value)
  }
  NextMethod()
}
#' @export
`$<-.quosures` <- function(x, name, value) {
  x[[name]] <- value
  x
}
signal_quosure_assign <- function(x) {
  deprecate_warn(paste_line(
    "Assigning non-quosure objects to quosure lists is deprecated as of rlang 0.3.0.",
    "Please coerce to a bare list beforehand with `as.list()`"
  ))
}

pillar_shaft.quosures <- function(x, ...) {
  labels <- map_chr(unname(x), as_label)
  structure(labels, width = 10L)
}
type_sum.quosures <- function(x, ...) {
  "quos"
}
on_load({
  s3_register("pillar::pillar_shaft", "quosures", pillar_shaft.quosures)
  s3_register("pillar::type_sum", "quosures", type_sum.quosures)
})


#' Squash a quosure
#'
#' @description
#'
#' `quo_squash()` flattens all nested quosures within an expression.
#' For example it transforms `^foo(^bar(), ^baz)` to the bare
#' expression `foo(bar(), baz)`.
#'
#' This operation is safe if the squashed quosure is used for
#' labelling or printing (see [as_label()], but note that `as_label()`
#' squashes quosures automatically). However if the squashed quosure
#' is evaluated, all expressions of the flattened quosures are
#' resolved in a single environment. This is a source of bugs so it is
#' good practice to set `warn` to `TRUE` to let the user know about
#' the lossy squashing.
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
#' @keywords internal
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' **Note:** You should now use [as_label()] or [as_name()] instead
#' of `quo_name()`. See life cycle section below.
#'
#' These functions take an arbitrary R object, typically an
#' [expression][is_expression], and represent it as a string.
#'
#' * `quo_name()` returns an abbreviated representation of the object
#'   as a single line string. It is suitable for default names.
#'
#' * `quo_text()` returns a multiline string. For instance block
#'   expressions like `{ foo; bar }` are represented on 4 lines (one
#'   for each symbol, and the curly braces on their own lines).
#'
#' These deparsers are only suitable for creating default names or
#' printing output at the console. The behaviour of your functions
#' should not depend on deparsed objects. If you are looking for a way
#' of transforming symbols to strings, use [as_string()] instead of
#' `quo_name()`. Unlike deparsing, the transformation between symbols
#' and strings is non-lossy and well defined.
#'
#' @inheritParams quo_squash
#' @inheritParams expr_label
#'
#' @section Life cycle:
#'
#' These functions are superseded.
#'
#' * [as_label()] and [as_name()] should be used instead of
#'   `quo_name()`. `as_label()` transforms any R object to a string
#'   but should only be used to create a default name. Labelisation is
#'   not a well defined operation and no assumption should be made
#'   about the label. On the other hand, `as_name()` only works with
#'   (possibly quosured) symbols, but is a well defined and
#'   deterministic operation.
#'
#' * We don't have a good replacement for `quo_text()` yet. See
#'   <https://github.com/r-lib/rlang/issues/636> to follow discussions
#'   about a new deparsing API.
#'
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
#' @export
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
  quo_squash_do <- function(x) {
    if (!is_false(warn)) {
      if (is_string(warn)) {
        msg <- warn
      } else {
        msg <- "Collapsing inner quosure"
      }
      warn(msg)
      warn <- FALSE
    }

    while (is_quosure(maybe_missing(x))) {
      x <- quo_get_expr(x)
    }
    if (!is_null(parent)) {
      node_poke_car(parent, maybe_missing(x))
    }
    quo_squash_impl(x, parent, warn = warn)
  }

  node_squash <- function(x) {
    while (!is_null(x)) {
      quo_squash_impl(node_car(x), parent = x, warn = warn)
      x <- node_cdr(x)
    }
  }

  switch_expr(
    x,
    language = {
      if (is_quosure(x)) {
        x <- quo_squash_do(x)
      } else {
        node_squash(x)
      }
    },
    pairlist = node_squash(x)
  )

  maybe_missing(x)
}


#' @export
print.quosure <- function(x, ...) {
  cat_line(.trailing = FALSE,
    style_bold("<quosure>"),
    "expr: "
  )
  quo_print(x)
  cat_line(.trailing = FALSE,
    "env:  "
  )

  env <- quo_get_env(x)
  quo_env_print(env)

  invisible(x)
}
#' @export
str.quosure <- function(object, ...) {
  str(unclass(object), ...)
}

#' @export
as.character.quosure <- function(x, ...) {
  deprecate_warn(paste_line(
    "Using `as.character()` on a quosure is deprecated as of rlang 0.3.0.",
    "Please use `as_label()` or `as_name()` instead."
  ))
  NextMethod()
}

#' @export
`[.quosure` <- function(x, i, ...) {
  deprecate_soft(c(
    "Subsetting quosures with `[` is deprecated as of rlang 0.4.0",
    "Please use `quo_get_expr()` instead."
  ))
  NextMethod()
}
#' @export
`[[.quosure` <- function(x, i, ...) {
  deprecate_soft(c(
    "Subsetting quosures with `[[` is deprecated as of rlang 0.4.0",
    "Please use `quo_get_expr()` instead."
  ))
  NextMethod()
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
  sexp_deparse(quo_get_expr(x), lines = lines)

  lines$quo_reset_colour()

  lines$get_lines()
}

new_quo_deparser <- function(width = peek_option("width"),
                             max_elements = 5L,
                             crayon = has_ansi()) {
  lines <- new_lines(
    width = width,
    max_elements = max_elements,
    deparser = quo_deparse
  )

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
  nm <- env_label(env)

  if (!is_reference(env, global_env()) && !is_reference(env, empty_env())) {
    nm <- col_blue(nm)
  }

  cat_line(nm)
}

#' @export
Ops.quosure <- function(e1, e2) {
  if (identical(.Generic, "!")) {
    abort(paste_line(
      "Quosures can only be unquoted within a quasiquotation context.",
      "",
      "  # Bad:",
      "  list(!!myquosure)",
      "",
      "  # Good:",
      "  dplyr::mutate(data, !!myquosure)"
    ))
  }

  if (missing(e2)) {
    bad <- sprintf("  %s%s", .Generic, "myquosure")
    good <- sprintf("  %s!!%s", .Generic, "myquosure")
  } else if (is_quosure(e1) && is_quosure(e2)) {
    bad <- sprintf("  myquosure1 %s myquosure2", .Generic)
    good <- sprintf("  !!myquosure1 %s !!myquosure2", .Generic)
  } else if (is_quosure(e1)) {
    bad <- sprintf("  myquosure %s rhs", .Generic)
    good <- sprintf("  !!myquosure %s rhs", .Generic)
  } else {
    bad <- sprintf("  lhs %s myquosure", .Generic)
    good <- sprintf("  lhs %s !!myquosure", .Generic)
  }

  abort(paste_line(
    "Base operators are not defined for quosures.",
    "Do you need to unquote the quosure?",
    "",
    "  # Bad:",
    bad,
    "",
    "  # Good:",
    good,
  ))
}

abort_quosure_op <- function(group, op) {
  abort(paste_line(
    sprintf("%s operations are not defined for quosures.", group),
    "Do you need to unquote the quosure?",
    "",
    "  # Bad:",
    sprintf("  %s(myquosure)", op),
    "",
    "  # Good:",
    sprintf("  %s(!!myquosure)", op),
  ))
}
#' @export
Math.quosure <- function(x, ...) {
  abort_quosure_op("Math", .Generic)
}
#' @export
Summary.quosure <- function(x, ...) {
  abort_quosure_op("Summary", .Generic)
}
#' @export
mean.quosure <- function(x, na.rm = TRUE, ...) {
  abort_quosure_op("Summary", "mean")
}
#' @importFrom stats median
#' @export
median.quosure <- function(x, na.rm = TRUE, ...) {
  abort_quosure_op("Summary", "median")
}
#' @importFrom stats quantile
#' @export
quantile.quosure <- function(x, na.rm = TRUE, ...) {
  abort_quosure_op("Summary", "quantile")
}

#' @export
c.quosure <- function(..., recursive = FALSE) {
  inputs <- list(...)
  if (some(inputs, function(x) !is_quosure(x) && !is.list(x))) {
    abort("Can't concatenate quosures with incompatible objects.")
  }

  out <- NextMethod()
  if (!every(out, is_quosure)) {
    abort("Can't concatenate quosures with incompatible objects.")
  }

  new_quosures(out)
}

