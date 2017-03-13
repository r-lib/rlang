#' Create a function by "hand"
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
#' g <- new_fn(alist(x = ), quote(x + 3))
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
new_fn <- function(args, body, env = caller_env()) {
  stopifnot(all(have_names(args)), is_expr(body), is_env(env))

  args <- as.pairlist(args)
  expr_eval(call("function", args, body), env)
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
#' `fn_defaults()` returns a named list of default arguments.
#' `fn_fmls_names()` returns the names of the arguments.
#'
#' Contrarily to `formals()`, these helpers also work with primitive
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
#' closure frames appear on the evaluation stack (see [eval_stack()]),
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
#' list(eval_stack())[[1]]
#'
#' # While closures do:
#' identity(identity(eval_stack()))
is_function <- function(x) {
  is_closure(x) || is_primitive(x)
}

#' @export
#' @rdname is_function
is_closure <- function(x) {
  typeof(x) == "closure"
}
#' Convert to closure.
#'
#' Convert an object to a closure. This is especially useful to
#' normalise primitive functions to a proper closure (see
#' [is_function()] about primitive functions).
#'
#' @param x A function or a string. In the latter case, the function
#'   is looked up in the calling environment.
#' @export
#' @examples
#' as_closure(list)
as_closure <- function(x) {
  coerce_type(x, "closure",
    closure =
      x,
    primitive = {
      fn_name <- prim_name(x)
      if (fn_name == "eval") {
        # do_eval() starts a context with a fake primitive function as
        # function definition. We replace it here with the .Internal()
        # wrapper of eval() so we can match the arguments.
        base::eval
      } else {
        .ArgsEnv[[fn_name]] %||% .GenericArgsEnv[[fn_name]]
      }
    },
    string =
      as_closure(get(x, envir = caller_env(), x, mode = "function"))
  )
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


#' Return the closure environment of a function.
#'
#' Closure environments define the scope of functions (see [env()]).
#' When a function call is evaluated, R creates an evaluation frame
#' (see [eval_stack()]) that inherits from the closure environment.
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


#' Coerce to function.
#'
#' This generic transforms objects to functions. It is especially
#' useful with formulas to create lambdas on the fly.
#'
#' @param .f A function or formula.
#'
#'   If a **function**, it is used as is.
#'
#'   If a **formula**, e.g. `~ .x + 2`, it is converted to a function
#'   with two arguments, `.x` or `.` and `.y`. This allows you to
#'   create very compact anonymous functions with up to two inputs.
#' @param ... Additional arguments passed on to methods. Currently
#'   unused in rlang.
#' @export
#' @examples
#' f <- as_function(~ . + 1)
#' f(10)
as_function <- function(.f, ...) {
  UseMethod("as_function")
}

#' @export
as_function.function <- function(.f, ...) {
  .f
}

#' @export
as_function.formula <- function(.f, ...) {
  .x <- NULL # hush R CMD check NOTE

  if (length(.f) != 2) {
    stop("Formula must be one sided", call. = FALSE)
  }
  new_fn(alist(.x = , .y = , . = .x), .f[[2]], environment(.f))
}
