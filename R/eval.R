#' Evaluate an expression in an environment
#'
#' `eval_bare()` is a lightweight version of the base function
#' [base::eval()]. It does not accept supplementary data, but it is
#' more efficient and does not clutter the evaluation stack.
#' Technically, `eval_bare()` is a simple wrapper around the C
#' function `Rf_eval()`.
#'
#' `base::eval()` inserts two call frames in the stack, the second of
#' which features the `envir` parameter as frame environment. This may
#' unnecessarily clutter the evaluation stack and it can change
#' evaluation semantics with stack sensitive functions in the case
#' where `env` is an evaluation environment of a stack frame (see
#' [ctxt_stack()]). Since the base function `eval()` creates a new
#' evaluation context with `env` as frame environment there are
#' actually two contexts with the same evaluation environment on the
#' stack when `expr` is evaluated. Thus, any command that looks up
#' frames on the stack (stack sensitive functions) may find the
#' parasite frame set up by `eval()` rather than the original frame
#' targetted by `env`. As a result, code evaluated with `base::eval()`
#' does not have the property of stack consistency, and stack
#' sensitive functions like [base::return()], [base::parent.frame()]
#' may return misleading results.
#'
#' @param expr An expression to evaluate.
#' @param env The environment in which to evaluate the expression.
#' @seealso with_env
#' @export
#' @examples
#' # eval_bare() works just like base::eval():
#' env <- child_env(NULL, foo = "bar")
#' expr <- quote(foo)
#' eval_bare(expr, env)
#'
#' # To explore the consequences of stack inconsistent semantics, let's
#' # create a function that evaluates `parent.frame()` deep in the call
#' # stack, in an environment corresponding to a frame in the middle of
#' # the stack. For consistency we R's lazy evaluation semantics, we'd
#' # expect to get the caller of that frame as result:
#' fn <- function(eval_fn) {
#'   list(
#'     returned_env = middle(eval_fn),
#'     actual_env = get_env()
#'   )
#' }
#' middle <- function(eval_fn) {
#'   deep(eval_fn, get_env())
#' }
#' deep <- function(eval_fn, eval_env) {
#'   expr <- quote(parent.frame())
#'   eval_fn(expr, eval_env)
#' }
#'
#' # With eval_bare(), we do get the expected environment:
#' fn(rlang::eval_bare)
#'
#' # But that's not the case with base::eval():
#' fn(base::eval)
#'
#' # Another difference of eval_bare() compared to base::eval() is
#' # that it does not insert parasite frames in the evaluation stack:
#' get_stack <- quote(identity(ctxt_stack()))
#' eval_bare(get_stack)
#' eval(get_stack)
eval_bare <- function(expr, env = parent.frame()) {
  .Call(rlang_eval, expr, env)
}

#' Evaluate an expression within a given environment
#'
#' These functions evaluate `expr` within a given environment (`env`
#' for `with_env()`, or the child of the current environment for
#' `locally`). They rely on [eval_bare()] which features a lighter
#' evaluation mechanism than base R [base::eval()], and which also has
#' some subtle implications when evaluting stack sensitive functions
#' (see help for [eval_bare()]).
#'
#' `locally()` is equivalent to the base function
#' [base::local()] but it produces a much cleaner
#' evaluation stack, and has stack-consistent semantics. It is thus
#' more suited for experimenting with the R language.
#'
#' @inheritParams eval_bare
#' @param env An environment within which to evaluate `expr`. Can be
#'   an object with an [get_env()] method.
#' @export
#' @examples
#' # with_env() is handy to create formulas with a given environment:
#' env <- child_env("rlang")
#' f <- with_env(env, ~new_formula())
#' identical(f_env(f), env)
#'
#' # Or functions with a given enclosure:
#' fn <- with_env(env, function() NULL)
#' identical(get_env(fn), env)
#'
#'
#' # Unlike eval() it doesn't create duplicates on the evaluation
#' # stack. You can thus use it e.g. to create non-local returns:
#' fn <- function() {
#'   g(get_env())
#'   "normal return"
#' }
#' g <- function(env) {
#'   with_env(env, return("early return"))
#' }
#' fn()
#'
#'
#' # Since env is passed to as_env(), it can be any object with an
#' # as_env() method. For strings, the pkg_env() is returned:
#' with_env("base", ~mtcars)
#'
#' # This can be handy to put dictionaries in scope:
#' with_env(mtcars, cyl)
with_env <- function(env, expr) {
  .Call(rlang_eval, substitute(expr), as_env(env, caller_env()))
}
#' @rdname with_env
#' @export
locally <- function(expr) {
  .Call(rlang_eval, substitute(expr), child_env(caller_env()))
}

#' Invoke a function with a list of arguments
#'
#' Normally, you invoke a R function by typing arguments manually. A
#' powerful alternative is to call a function with a list of arguments
#' assembled programmatically. This is the purpose of `invoke()`.
#'
#' Technically, `invoke()` is basically a version of [base::do.call()]
#' that creates cleaner call traces because it does not inline the
#' function and the arguments in the call (see examples). To achieve
#' this, `invoke()` creates a child environment of `.env` with `.fn`
#' and all arguments bound to new symbols (see [env_bury()]). It then
#' uses the same strategy as [eval_bare()] to evaluate with minimal
#' noise.
#'
#' @param .fn A function to invoke. Can be a function object or the
#'   name of a function in scope of `.env`.
#' @param .args,... List of arguments (possibly named) to be passed to
#'   `.fn`.
#' @param .env The environment in which to call `.fn`.
#' @param .bury A character vector of length 2. The first string
#'   specifies which name should the function have in the call
#'   recorded in the evaluation stack. The second string specifies a
#'   prefix for the argument names. Set `.bury` to `NULL` if you
#'   prefer to inline the function and its arguments in the call.
#' @export
#' @examples
#' # invoke() has the same purpose as do.call():
#' invoke(paste, letters)
#'
#' # But it creates much cleaner calls:
#' invoke(call_inspect, mtcars)
#'
#' # and stacktraces:
#' fn <- function(...) sys.calls()
#' invoke(fn, list(mtcars))
#'
#' # Compare to do.call():
#' do.call(call_inspect, mtcars)
#' do.call(fn, list(mtcars))
#'
#'
#' # Specify the function name either by supplying a string
#' # identifying the function (it should be visible in .env):
#' invoke("call_inspect", letters)
#'
#' # Or by changing the .bury argument, with which you can also change
#' # the argument prefix:
#' invoke(call_inspect, mtcars, .bury = c("inspect!", "col"))
invoke <- function(.fn, .args = list(), ...,
                   .env = caller_env(), .bury = c(".fn", "")) {
  args <- c(.args, list(...))

  if (is_null(.bury) || !length(args)) {
    if (is_scalar_character(.fn)) {
      .fn <- env_get(.env, .fn, inherit = TRUE)
    }
    call <- lang(.fn, !!! args)
    return(.Call(rlang_eval, call, .env))
  }


  if (!is_character(.bury, 2L)) {
    abort("`.bury` must be a character vector of length 2")
  }
  arg_prefix <- .bury[[2]]
  fn_nm <- .bury[[1]]

  buried_nms <- paste0(arg_prefix, seq_along(args))
  buried_args <- set_names(args, buried_nms)
  .env <- env_bury(.env, !!! buried_args)
  args <- set_names(buried_nms, names(args))
  args <- syms(args)

  if (is_function(.fn)) {
    env_bind(.env, !! fn_nm := .fn)
    .fn <- fn_nm
  }

  call <- lang(.fn, !!! args)
  .Call(rlang_eval, call, .env)
}
