#' Create a function
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' This constructs a new function given its three components:
#' list of arguments, body code and parent environment.
#'
#' @param args A named list or pairlist of default arguments. Note
#'   that if you want arguments that don't have defaults, you'll need
#'   to use the special function [pairlist2()]. If you need quoted
#'   defaults, use [exprs()].
#' @param body A language object representing the code inside the
#'   function. Usually this will be most easily generated with
#'   [base::quote()]
#' @param env The parent environment of the function, defaults to the
#'   calling environment of `new_function()`
#' @export
#' @examples
#' f <- function() letters
#' g <- new_function(NULL, quote(letters))
#' identical(f, g)
#'
#' # Pass a list or pairlist of named arguments to create a function
#' # with parameters. The name becomes the parameter name and the
#' # argument the default value for this parameter:
#' new_function(list(x = 10), quote(x))
#' new_function(pairlist2(x = 10), quote(x))
#'
#' # Use `exprs()` to create quoted defaults. Compare:
#' new_function(pairlist2(x = 5 + 5), quote(x))
#' new_function(exprs(x = 5 + 5), quote(x))
#'
#' # Pass empty arguments to omit defaults. `list()` doesn't allow
#' # empty arguments but `pairlist2()` does:
#' new_function(pairlist2(x = , y = 5 + 5), quote(x + y))
#' new_function(exprs(x = , y = 5 + 5), quote(x + y))
new_function <- function(args, body, env = caller_env()) {
  .Call(rlang_new_function, args, body, env)
}

prim_eval <- eval(quote(sys.function(0)))
is_prim_eval <- function(x) identical(x, prim_eval)

#' Name of a primitive function
#' @param prim A primitive function such as [base::c()].
#' @keywords internal
#' @export
prim_name <- function(prim) {
  stopifnot(is_primitive(prim))

  # Workaround because R_FunTab is not public
  name <- format(prim)
  name <- sub("^.Primitive\\(\"", "", name)
  name <- sub("\"\\)$", "", name)
  name
}

#' Extract arguments from a function
#'
#' `fn_fmls()` returns a named list of formal arguments.
#' `fn_fmls_names()` returns the names of the arguments.
#' `fn_fmls_syms()` returns formals as a named list of symbols. This
#' is especially useful for forwarding arguments in [constructed
#' calls][lang].
#'
#' Unlike `formals()`, these helpers throw an error with primitive
#' functions instead of returning `NULL`.
#'
#' @param fn A function. It is lookep up in the calling frame if not
#'   supplied.
#' @seealso [call_args()] and [call_args_names()]
#' @export
#' @examples
#' # Extract from current call:
#' fn <- function(a = 1, b = 2) fn_fmls()
#' fn()
#'
#' # fn_fmls_syms() makes it easy to forward arguments:
#' call2("apply", !!! fn_fmls_syms(lapply))
#'
#' # You can also change the formals:
#' fn_fmls(fn) <- list(A = 10, B = 20)
#' fn()
#'
#' fn_fmls_names(fn) <- c("foo", "bar")
#' fn()
fn_fmls <- function(fn = caller_fn()) {
  check_closure(fn)
  formals(fn)
}
#' @rdname fn_fmls
#' @export
fn_fmls_names <- function(fn = caller_fn()) {
  args <- fn_fmls(fn)
  names(args)
}
#' @rdname fn_fmls
#' @export
fn_fmls_syms <- function(fn = caller_fn()) {
  fmls_nms <- fn_fmls_names(fn)
  if (is_null(fmls_nms)) {
    return(list())
  }

  nms <- set_names(fmls_nms)
  names(nms)[match("...", nms)] <- ""
  syms(nms)
}

#' @rdname fn_fmls
#' @param value New formals or formals names for `fn`.
#' @export
`fn_fmls<-` <- function(fn, value) {
  check_closure(fn)
  attrs <- attributes(fn)

  formals(fn) <- value

  # Work around bug in base R
  attributes(fn) <- attrs

  fn
}
#' @rdname fn_fmls
#' @export
`fn_fmls_names<-` <- function(fn, value) {
  check_closure(fn)
  attrs <- attributes(fn)

  fmls <- formals(fn)
  names(fmls) <- value
  formals(fn) <- fmls

  # Work around bug in base R
  attributes(fn) <- attrs

  fn
}

check_closure <- function(x) {
  if (!is_closure(x)) {
    abort(sprintf("`fn` must be an R function, not %s", friendly_type_of(x)))
  }
}

#' Get or set function body
#'
#' `fn_body()` is a simple wrapper around [base::body()]. It always
#' returns a `\{` expression and throws an error when the input is a
#' primitive function (whereas `body()` returns `NULL`). The setter
#' version preserves attributes, unlike `body<-`.
#'
#' @inheritParams fn_fmls
#'
#' @export
#' @examples
#' # fn_body() is like body() but always returns a block:
#' fn <- function() do()
#' body(fn)
#' fn_body(fn)
#'
#' # It also throws an error when used on a primitive function:
#' try(fn_body(base::list))
fn_body <- function(fn = caller_fn()) {
  if(!is_closure(fn)) {
    abort("`fn` is not a closure")
  }

  body <- body(fn)

  if (is_call(body, "{")) {
    body
  } else {
    call("{", body)
  }
}
#' @rdname fn_body
#' @export
`fn_body<-` <- function(fn, value) {
  attrs <- attributes(fn)

  body(fn) <- value

  # Work around bug in base R. First remove source references since
  # the body has changed
  attrs$srcref <- NULL
  attributes(fn) <- attrs

  fn
}

fn_body_node <- function(fn) {
  body <- body(fn)
  if (is_call(body, "{")) {
    node_cdr(fn)
  } else {
    pairlist(body)
  }
}

#' Is object a function?
#'
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#'
#' Closures are functions written in R, named after the way their
#' arguments are scoped within nested environments (see
#' https://en.wikipedia.org/wiki/Closure_(computer_programming)). The
#' root environment of the closure is called the closure
#' environment. When closures are evaluated, a new environment called
#' the evaluation frame is created with the closure environment as
#' parent. This is where the body of the closure is evaluated. These
#' closure frames appear on the evaluation stack (see [ctxt_stack()]),
#' as opposed to primitive functions which do not necessarily have
#' their own evaluation frame and never appear on the stack.
#'
#' Primitive functions are more efficient than closures for two
#' reasons. First, they are written entirely in fast low-level
#' code. Second, the mechanism by which they are passed arguments is
#' more efficient because they often do not need the full procedure of
#' argument matching (dealing with positional versus named arguments,
#' partial matching, etc). One practical consequence of the special
#' way in which primitives are passed arguments is that they
#' technically do not have formal arguments, and [formals()] will
#' return `NULL` if called on a primitive function. Finally, primitive 
#' functions can either take arguments lazily, like R closures do, 
#' or evaluate them eagerly before being passed on to the C code. 
#' The former kind of primitives are called "special" in R terminology, 
#' while the latter is referred to as "builtin". `is_primitive_eager()` 
#' and `is_primitive_lazy()` allow you to check whether a primitive 
#' function evaluates arguments eagerly or lazily.
#'
#' You will also encounter the distinction between primitive and
#' internal functions in technical documentation. Like primitive
#' functions, internal functions are defined at a low level and
#' written in C. However, internal functions have no representation in
#' the R language. Instead, they are called via a call to
#' [base::.Internal()] within a regular closure. This ensures that
#' they appear as normal R function objects: they obey all the usual
#' rules of argument passing, and they appear on the evaluation stack
#' as any other closures. As a result, [fn_fmls()] does not need to
#' look in the `.ArgsEnv` environment to obtain a representation of
#' their arguments, and there is no way of querying from R whether
#' they are lazy ('special' in R terminology) or eager ('builtin').
#'
#' You can call primitive functions with [.Primitive()] and internal
#' functions with [.Internal()]. However, calling internal functions
#' in a package is forbidden by CRAN's policy because they are
#' considered part of the private API. They often assume that they
#' have been called with correctly formed arguments, and may cause R
#' to crash if you call them with unexpected objects.
#'
#' @inheritParams type-predicates
#' @export
#' @examples
#' # Primitive functions are not closures:
#' is_closure(base::c)
#' is_primitive(base::c)
#'
#' # On the other hand, internal functions are wrapped in a closure
#' # and appear as such from the R side:
#' is_closure(base::eval)
#'
#' # Both closures and primitives are functions:
#' is_function(base::c)
#' is_function(base::eval)
#'
#' # Primitive functions never appear in evaluation stacks:
#' is_primitive(base::`[[`)
#' is_primitive(base::list)
#' list(ctxt_stack())[[1]]
#'
#' # While closures do:
#' identity(identity(ctxt_stack()))
is_function <- function(x) {
  is_closure(x) || is_primitive(x)
}

#' @export
#' @rdname is_function
is_closure <- function(x) {
  typeof(x) == "closure"
}

#' @export
#' @rdname is_function
is_primitive <- function(x) {
  typeof(x) %in% c("builtin", "special")
}
#' @export
#' @rdname is_function
#' @examples
#'
#' # Many primitive functions evaluate arguments eagerly:
#' is_primitive_eager(base::c)
#' is_primitive_eager(base::list)
#' is_primitive_eager(base::`+`)
is_primitive_eager <- function(x) {
  typeof(x) == "builtin"
}
#' @export
#' @rdname is_function
#' @examples
#'
#' # However, primitives that operate on expressions, like quote() or
#' # substitute(), are lazy:
#' is_primitive_lazy(base::quote)
#' is_primitive_lazy(base::substitute)
is_primitive_lazy <- function(x) {
  typeof(x) == "special"
}


#' Return the closure environment of a function
#'
#' Closure environments define the scope of functions (see [env()]).
#' When a function call is evaluated, R creates an evaluation frame
#' (see [ctxt_stack()]) that inherits from the closure environment.
#' This makes all objects defined in the closure environment and all
#' its parents available to code executed within the function.
#'
#' `fn_env()` returns the closure environment of `fn`. There is also
#' an assignment method to set a new closure environment.
#'
#' @param fn,x A function.
#' @param value A new closure environment for the function.
#' @export
#' @examples
#' env <- child_env("base")
#' fn <- with_env(env, function() NULL)
#' identical(fn_env(fn), env)
#'
#' other_env <- child_env("base")
#' fn_env(fn) <- other_env
#' identical(fn_env(fn), other_env)
fn_env <- function(fn) {
  if (is_primitive(fn)) {
    return(ns_env("base"))
  }

  if(is_closure(fn)) {
    return(environment(fn))
  }

  abort("`fn` is not a function")
}

#' @export
#' @rdname fn_env
`fn_env<-` <- function(x, value) {
  if(!is_function(x)) {
    abort("`fn` is not a function")
  }
  environment(x) <- value
  x
}


#' Convert to function or closure
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' * `as_function()` transforms a one-sided formula into a function.
#'   This powers the lambda syntax in packages like purrr.
#'
#' * `as_closure()` first passes its argument to `as_function()`. If
#'   the result is a primitive function, it regularises it to a proper
#'   [closure] (see [is_function()] about primitive functions). Some
#'   special control flow primitives like `if`, `for`, or `break`
#'   can't be coerced to a closure.
#'
#' @param x A function or formula.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function
#'   with up to two arguments: `.x` (single argument) or `.x` and `.y`
#'   (two arguments). The `.` placeholder can be used instead of `.x`.
#'   This allows you to create very compact anonymous functions (lambdas) with up
#'   to two inputs. Functions created from formulas have a special
#'   class. Use `is_lambda()` to test for it.
#'
#'   Lambdas currently do not support [quasiquotation],
#'   due to the way the arguments are handled internally.
#'
#' @param env Environment in which to fetch the function in case `x`
#'   is a string.
#' @export
#' @examples
#' f <- as_function(~ .x + 1)
#' f(10)
#'
#' g <- as_function(~ -1 * .)
#' g(4)
#'
#' h <- as_function(~ .x - .y)
#' h(6, 3)
#'
#' # Functions created from a formula have a special class:
#' is_lambda(f)
#' is_lambda(as_function(function() "foo"))
#'
#' # Primitive functions are regularised as closures
#' as_closure(list)
#' as_closure("list")
#'
#' # Operators have `.x` and `.y` as arguments, just like lambda
#' # functions created with the formula syntax:
#' as_closure(`+`)
#' as_closure(`~`)
#'
#' # Use a regular function for tidy evaluation, also when calling functions
#' # that use tidy evaluation:
#' ## Bad:
#' e <- as_function(~ as_label(ensym(.x)))
#' ## Good:
#' e <- as_function(function(x) as_label(ensym(x)))
#'
#' e(y)
as_function <- function(x, env = caller_env()) {
  if (is_function(x)) {
    return(x)
  }

  if (is_quosure(x)) {
    return(eval(expr(function(...) eval_tidy(!!x))))
  }

  if (is_formula(x)) {
    if (length(x) > 2) {
      abort("Can't convert a two-sided formula to a function")
    }

    args <- list(... = missing_arg(), .x = quote(..1), .y = quote(..2), . = quote(..1))
    fn <- new_function(args, f_rhs(x), f_env(x))
    fn <- structure(fn, class = c("rlang_lambda_function", "function"))
    return(fn)
  }

  if (is_string(x)) {
    return(get(x, envir = env, mode = "function"))
  }

  abort_coercion(x, friendly_type("function"))
}
#' @export
print.rlang_lambda_function <- function(x, ...) {
  cat_line("<lambda>")
  NextMethod()
}
#' @rdname as_function
#' @export
is_lambda <- function(x) {
  inherits(x, "rlang_lambda_function")
}

#' @rdname as_function
#' @export
as_closure <- function(x, env = caller_env()) {
  x <- as_function(x, env = env)

  if (is_closure(x)) {
    return(x)
  }
  if (!is_primitive(x)) {
    abort_coercion(x, "a closure")
  }

  fn_name <- prim_name(x)
  fn <- op_as_closure(fn_name)

  if (!is_null(fn)) {
    return(fn)
  }

  fmls <- formals(args(fn_name))
  prim_call <- call2(x, !!!prim_args(fmls))

  # The closure wrapper should inherit from the global environment
  # to ensure proper lexical dispatch with methods defined there
  new_function(fmls, prim_call, global_env())
}

prim_args <- function(fmls) {
  args <- names(fmls)

  # Set argument names but only after `...`. Arguments before dots
  # should be positionally matched.
  dots_i <- match("...", args)
  if (!is_na(dots_i)) {
    idx <- seq2(dots_i + 1L, length(args))
    names2(args)[idx] <- args[idx]
  }

  syms(args)
}

utils::globalVariables(c("!<-", "(<-", "enexpr<-"))

op_as_closure <- function(prim_nm) {
  switch(prim_nm,
    `<-` = ,
    `<<-` = ,
    `=` = function(.x, .y) {
      op <- sym(prim_nm)
      expr <- expr((!!op)(!!enexpr(.x), !!enexpr(.y)))
      eval_bare(expr, caller_env())
    },
    `@` = ,
    `$` = function(.x, .i) {
      op <- sym(prim_nm)
      expr <- expr((!!op)(.x, !!quo_squash(enexpr(.i), warn = TRUE)))
      eval_bare(expr)
    },
    `[[<-` = function(.x, .i, .value) {
      expr <- expr((!!enexpr(.x))[[!!enexpr(.i)]] <- !!enexpr(.value))
      eval_bare(expr, caller_env())
    },
    `[<-` = function(.x, ...) {
      args <- exprs(...)
      n <- length(args)
      if (n < 2L) {
        abort("Must supply operands to `[<-`")
      }
      expr <- expr((!!enexpr(.x))[!!!args[-n]] <- !!args[[n]])
      eval_bare(expr, caller_env())
    },
    `@<-` = function(.x, .i, .value) {
      expr <- expr(`@`(!!enexpr(.x), !!enexpr(.i)) <- !!enexpr(.value))
      eval_bare(expr, caller_env())
    },
    `$<-` = function(.x, .i, .value) {
      expr <- expr(`$`(!!enexpr(.x), !!enexpr(.i)) <- !!enexpr(.value))
      eval_bare(expr, caller_env())
    },
    `(` = function(.x) .x,
    `[` = function(.x, ...) .x[...],
    `[[` = function(.x, ...) .x[[...]],
    `{` = function(...) {
      values <- list(...)
      values[[length(values)]]
    },
    `&`  = new_binary_closure(function(.x, .y) .x & .y),
    `|`  = new_binary_closure(function(.x, .y) .x | .y),
    `&&` = new_binary_closure(function(.x, .y) .x && .y),
    `||` = new_binary_closure(function(.x, .y) .x || .y, shortcircuiting = TRUE),
    `!`  = function(.x) !.x,
    `+`  = new_binary_closure(function(.x, .y) if (missing(.y)) .x else .x + .y, versatile = TRUE),
    `-`  = new_binary_closure(function(.x, .y) if (missing(.y)) -.x else .x - .y, versatile = TRUE),
    `*`  = new_binary_closure(function(.x, .y) .x * .y),
    `/`  = new_binary_closure(function(.x, .y) .x / .y),
    `^`  = new_binary_closure(function(.x, .y) .x ^ .y),
    `%%` = new_binary_closure(function(.x, .y) .x %% .y),
    `<`  = new_binary_closure(function(.x, .y) .x < .y),
    `<=` = new_binary_closure(function(.x, .y) .x <= .y),
    `>`  = new_binary_closure(function(.x, .y) .x > .y),
    `>=` = new_binary_closure(function(.x, .y) .x >= .y),
    `==` = new_binary_closure(function(.x, .y) .x == .y),
    `!=` = new_binary_closure(function(.x, .y) .x != .y),
    `:`  = new_binary_closure(function(.x, .y) .x : .y),
    `~`  = function(.x, .y) {
      if (is_missing(substitute(.y))) {
        new_formula(NULL, substitute(.x), caller_env())
      } else {
        new_formula(substitute(.x), substitute(.y), caller_env())
      }
    },

    `c` = function(...) c(...),

    # Unsupported primitives
    `break` = ,
    `for` = ,
    `function` = ,
    `if` = ,
    `next` = ,
    `repeat` = ,
    `return` = ,
    `while` = {
      nm <- chr_quoted(prim_nm)
      abort(paste0("Can't coerce the primitive function ", nm, " to a closure"))
    }
  )
}

new_binary_closure <- function(fn,
                               versatile = FALSE,
                               shortcircuiting = FALSE) {
  if (versatile) {
    nodes <- versatile_check_nodes
  } else if (shortcircuiting) {
    nodes <- shortcircuiting_check_nodes
  } else {
    nodes <- binary_check_nodes
  }

  nodes <- duplicate(nodes, shallow = TRUE)
  nodes <- node_append(nodes, fn_body_node(fn))
  body <- new_call(brace_sym, nodes)

  formals(fn) <- binary_fmls
  body(fn) <- body

  fn
}

binary_fmls <- as.pairlist(alist(
  e1 = ,
  e2 = ,
  .x = e1,
  .y = e2
))
binary_check_nodes <- pairlist(
  quote(
    if (missing(.x)) {
      if (missing(e1)) {
        abort("Must supply `e1` or `.x` to binary operator")
      }
      .x <- e1
    } else if (!missing(e1)) {
      abort("Can't supply both `e1` and `.x` to binary operator")
    }
  ),
  quote(
    if (missing(.y)) {
      if (missing(e2)) {
        abort("Must supply `e2` or `.y` to binary operator")
      }
      .y <- e2
    } else if (!missing(e2)) {
      abort("Can't supply both `e2` and `.y` to binary operator")
    }
  )
)
versatile_check_nodes <- as.pairlist(c(
  binary_check_nodes[[1]],
  quote(
    if (missing(.y) && !missing(e2)) {
      .y <- e2
    } else if (!missing(e2)) {
      abort("Can't supply both `e2` and `.y` to binary operator")
    }
  )
))
shortcircuiting_check_nodes <- as.pairlist(c(
  binary_check_nodes[[1]],
  quote(if (.x) return(TRUE)),
  binary_check_nodes[[2]]
))

#' Make an `fn` object
#'
#' @noRd
#' @description
#'
#' `new_fn()` takes a function and sets the class to `c("fn",
#' function)`.
#'
#' * Inheriting from `"fn"` enables a print method that strips all
#'   attributes (except `srcref`) before printing. This is currently
#'   the only purpose of the `fn` class.
#'
#' * Inheriting from `"function"` makes sure your function still
#'   dispatches on type methods.
#'
#' @param fn A closure.
#' @return An object of class `c("fn", "function")`.
#' @examples
#' fn <- structure(function() "foo", attribute = "foobar")
#' print(fn)
#'
#' # The `fn` object doesn't print with attributes:
#' fn <- new_fn(fn)
#' print(fn)
new_fn <- function(fn) {
  stopifnot(is_closure(fn))
  structure(fn, class = c("fn", "function"))
}
print.fn <- function(x, ...) {
  srcref <- attr(x, "srcref")
  attributes(x) <- NULL
  x <- structure(x, srcref = srcref)
  print(x)
}

as_predicate <- function(.fn, ...) {
  .fn <- as_function(.fn)

  function(...) {
    out <- .fn(...)

    if (!is_bool(out)) {
      abort(sprintf(
        "Predicate functions must return a single `TRUE` or `FALSE`, not %s",
        as_predicate_friendly_type_of(out)
      ))
    }

    out
  }
}
as_predicate_friendly_type_of <- function(x) {
  if (is_na(x)) {
    "a missing value"
  } else {
    friendly_type_of(x, length = TRUE)
  }
}
