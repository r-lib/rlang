#' Create a function
#'
#' This constructs a new function given it's three components:
#' list of arguments, body code and parent environment.
#'
#' @param args A named list of default arguments.  Note that if you
#'   want arguments that don't have defaults, you'll need to use the
#'   special function [alist], e.g. `alist(a = , b = 1)`
#' @param body A language object representing the code inside the
#'   function. Usually this will be most easily generated with
#'   [base::quote()]
#' @param env The parent environment of the function, defaults to the
#'   calling environment of `make_function`
#' @export
#' @examples
#' f <- function(x) x + 3
#' g <- new_function(alist(x = ), quote(x + 3))
#'
#' # The components of the functions are identical
#' identical(formals(f), formals(g))
#' identical(body(f), body(g))
#' identical(environment(f), environment(g))
#'
#' # But the functions are not identical because f has src code reference
#' identical(f, g)
#'
#' attr(f, "srcref") <- NULL
#' # Now they are:
#' stopifnot(identical(f, g))
new_function <- function(args, body, env = caller_env()) {
  stopifnot(all(have_name(args)), is_expr(body), is_env(env))

  args <- as.pairlist(args)
  eval_bare(call("function", args, body), env)
}

prim_eval <- eval(quote(sys.function(0)))
is_prim_eval <- function(x) identical(x, prim_eval)

#' Name of a primitive function
#' @param prim A primitive function such as [base::c()].
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
#' Unlike `formals()`, these helpers also work with primitive
#' functions. See [is_function()] for a discussion of primitive and
#' closure functions.
#'
#' @param fn A function. It is lookep up in the calling frame if not
#'   supplied.
#' @seealso [lang_args()] and [lang_args_names()]
#' @export
#' @examples
#' # Extract from current call:
#' fn <- function(a = 1, b = 2) fn_fmls()
#' fn()
#'
#' # Works with primitive functions:
#' fn_fmls(base::switch)
#'
#' # fn_fmls_syms() makes it easy to forward arguments:
#' lang("apply", !!! fn_fmls_syms(lapply))
fn_fmls <- function(fn = caller_fn()) {
  fn <- as_closure(fn)
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
  nms <- set_names(fn_fmls_names(fn))
  names(nms)[match("...", nms)] <- ""
  syms(nms)
}


#' Is object a function?
#'
#' The R language defines two different types of functions: primitive
#' functions, which are low-level, and closures, which are the regular
#' kind of functions.
#'
#' Closures are functions written in R, named after the way their
#' arguments are scoped within nested environments (see
#' https://en.wikipedia.org/wiki/Closure_(computer_programming)).  The
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
#' code. Secondly, the mechanism by which they are passed arguments is
#' more efficient because they often do not need the full procedure of
#' argument matching (dealing with positional versus named arguments,
#' partial matching, etc). One practical consequence of the special
#' way in which primitives are passed arguments this is that they
#' technically do not have formal arguments, and [formals()] will
#' return `NULL` if called on a primitive function. See [fn_fmls()]
#' for a function that returns a representation of formal arguments
#' for primitive functions. Finally, primitive functions can either
#' take arguments lazily, like R closures do, or evaluate them eagerly
#' before being passed on to the C code. The former kind of primitives
#' are called "special" in R terminology, while the latter is referred
#' to as "builtin". `is_primitive_eager()` and `is_primitive_lazy()`
#' allow you to check whether a primitive function evaluates arguments
#' eagerly or lazily.
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
  if(!is_function(fn)) {
    abort("`fn` is not a function", "type")
  }
  environment(fn)
}

#' @export
#' @rdname fn_env
`fn_env<-` <- function(x, value) {
  if(!is_function(x)) {
    abort("`fn` is not a function", "type")
  }
  environment(x) <- value
  x
}


#' Convert to function or closure
#'
#' @description
#'
#' * `as_function()` transform objects to functions. It fetches
#'   functions by name if supplied a string or transforms
#'   [quosures][quosure] to a proper function.
#'
#' * `as_closure()` first passes its argument to `as_function()`. If
#'   the result is a primitive function, it regularises it to a proper
#'   [closure] (see [is_function()] about primitive functions).
#'
#' @param x A function or formula.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function
#'   with two arguments, `.x` or `.` and `.y`. This allows you to
#'   create very compact anonymous functions with up to two inputs.
#' @param env Environment in which to fetch the function in case `x`
#'   is a string.
#' @export
#' @examples
#' f <- as_function(~ . + 1)
#' f(10)
#'
#' # Primitive functions are regularised as closures
#' as_closure(list)
#' as_closure("list")
#'
#' # Operators have `.x` and `.y` as arguments, just like lambda
#' # functions created with the formula syntax:
#' as_closure(`+`)
#' as_closure(`~`)
as_function <- function(x, env = caller_env()) {
  coerce_type(x, friendly_type("function"),
    primitive = ,
    closure = {
      x
    },
    formula = {
      if (length(x) > 2) {
        abort("Can't convert a two-sided formula to a function")
      }
      args <- list(... = missing_arg(), .x = quote(..1), .y = quote(..2), . = quote(..1))
      new_function(args, f_rhs(x), f_env(x))
    },
    string = {
      get(x, envir = env, mode = "function")
    }
  )
}
#' @rdname as_function
#' @export
as_closure <- function(x, env = caller_env()) {
  x <- as_function(x, env = env)
  coerce_type(x, "a closure",
    closure =
      x,
    primitive = {
      fn_name <- prim_name(x)

      fn <- op_as_closure(fn_name)
      if (!is_null(fn)) {
        return(fn)
      }

      if (fn_name == "eval") {
        # do_eval() starts a context with a fake primitive function as
        # function definition. We replace it here with the .Internal()
        # wrapper of eval() so we can match the arguments.
        fmls <- formals(base::eval)
      } else {
        fmls <- formals(.ArgsEnv[[fn_name]] %||% .GenericArgsEnv[[fn_name]])
      }

      args <- syms(names(fmls))
      args <- set_names(args)
      names(args)[(names(args) == "...")] <- ""

      prim_call <- lang(fn_name, splice(args))
      new_function(fmls, prim_call, base_env())
    }
  )
}

op_as_closure <- function(prim_nm) {
  switch(prim_nm,
    `<-` = ,
    `<<-` = ,
    `=` = function(.x, .y) {
      op <- sym(prim_nm)
      expr <- expr(UQ(op)(UQ(enexpr(.x)), UQ(enexpr(.y))))
      eval_bare(expr, caller_env())
    },
    `@` = ,
    `$` = function(.x, .i) {
      op <- sym(prim_nm)
      expr <- expr(UQ(op)(.x, `!!`(enexpr(.i))))
      eval_bare(expr)
    },
    `[[<-` = ,
    `@<-` = ,
    `$<-` = function(.x, .i, .value) {
      op <- sym(prim_nm)
      expr <- expr(UQ(op)(UQ(enexpr(.x)), UQ(enexpr(.i)), UQ(enexpr(.value))))
      eval_bare(expr, caller_env())
    },
    `[<-` = function(.x, ...) {
      expr <- expr(`[<-`(!! enexpr(.x), !!! exprs(...)))
      eval_bare(expr, caller_env())
    },
    `(` = function(.x) .x,
    `[` = function(.x, ...) .x[...],
    `[[` = function(.x, ...) .x[[...]],
    `{` = function(...) {
      values <- list(...)
      values[[length(values)]]
    },
    `&` = function(.x, .y) .x & .y,
    `|` = function(.x, .y) .x | .y,
    `&&` = function(.x, .y) .x && .y,
    `||` = function(.x, .y) .x || .y,
    `!` = function(.x) !.x,
    `+` = function(.x, .y) if (missing(.y)) .x else .x + .y,
    `-` = function(.x, .y) if (missing(.y)) -.x else .x - .y,
    `*` = function(.x, .y) .x * .y,
    `/` = function(.x, .y) .x / .y,
    `^` = function(.x, .y) .x ^ .y,
    `%%` = function(.x, .y) .x %% .y,
    `<` = function(.x, .y) .x < .y,
    `<=` = function(.x, .y) .x <= .y,
    `>` = function(.x, .y) .x > .y,
    `>=` = function(.x, .y) .x >= .y,
    `==` = function(.x, .y) .x == .y,
    `!=` = function(.x, .y) .x != .y,
    `:` = function(.x, .y) .x : .y,
    `~` = function(.x, .y) {
      if (is_missing(substitute(.y))) {
        new_formula(NULL, substitute(.x), caller_env())
      } else {
        new_formula(substitute(.x), substitute(.y), caller_env())
      }
    },

    # Unsupported primitives
    `break` = ,
    `for` = ,
    `function` = ,
    `if` = ,
    `next` = ,
    `repeat` = ,
    `return` = ,
    `while` = {
      nm <- chr_quoted(prim_name)
      abort(paste0("Can't coerce the primitive function ", nm, " to a closure"))
    }
  )
}
