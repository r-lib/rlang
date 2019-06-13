#' Evaluate an expression in an environment
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("stable")}
#'
#' `eval_bare()` is a lower-level version of function [base::eval()].
#' Technically, it is a simple wrapper around the C function
#' `Rf_eval()`. You generally don't need to use `eval_bare()` instead
#' of `eval()`. Its main advantage is that it handles stack-sensitive
#' (calls such as `return()`, `on.exit()` or `parent.frame()`) more
#' consistently when you pass an enviroment of a frame on the call
#' stack.
#'
#' @details
#'
#' These semantics are possible because `eval_bare()` creates only one
#' frame on the call stack whereas `eval()` creates two frames, the
#' second of which has the user-supplied environment as frame
#' environment. When you supply an existing frame environment to
#' `base::eval()` there will be two frames on the stack with the same
#' frame environment. Stack-sensitive functions only detect the
#' topmost of these frames. We call these evaluation semantics
#' "stack inconsistent".
#'
#' Evaluating expressions in the actual frame environment has useful
#' practical implications for `eval_bare()`:
#'
#' * `return()` calls are evaluated in frame environments that might
#'   be burried deep in the call stack. This causes a long return that
#'   unwinds multiple frames (triggering the `on.exit()` event for
#'   each frame). By contrast `eval()` only returns from the `eval()`
#'   call, one level up.
#'
#' * `on.exit()`, `parent.frame()`, `sys.call()`, and generally all
#'   the stack inspection functions `sys.xxx()` are evaluated in the
#'   correct frame environment. This is similar to how this type of
#'   calls can be evaluated deep in the call stack because of lazy
#'   evaluation, when you force an argument that has been passed
#'   around several times.
#'
#' The flip side of the semantics of `eval_bare()` is that it can't
#' evaluate `break` or `next` expressions even if called within a
#' loop.
#'
#'
#' @param expr An expression to evaluate.
#' @param env The environment in which to evaluate the expression.
#'
#' @seealso [eval_tidy()] for evaluation with data mask and quosure
#'   support.
#' @export
#' @examples
#' # eval_bare() works just like base::eval() but you have to create
#' # the evaluation environment yourself:
#' eval_bare(quote(foo), env(foo = "bar"))
#'
#' # eval() has different evaluation semantics than eval_bare(). It
#' # can return from the supplied environment even if its an
#' # environment that is not on the call stack (i.e. because you've
#' # created it yourself). The following would trigger an error with
#' # eval_bare():
#' ret <- quote(return("foo"))
#' eval(ret, env())
#' # eval_bare(ret, env())  # "no function to return from" error
#'
#' # Another feature of eval() is that you can control surround loops:
#' bail <- quote(break)
#' while (TRUE) {
#'   eval(bail)
#'   # eval_bare(bail)  # "no loop for break/next" error
#' }
#'
#' # To explore the consequences of stack inconsistent semantics, let's
#' # create a function that evaluates `parent.frame()` deep in the call
#' # stack, in an environment corresponding to a frame in the middle of
#' # the stack. For consistency with R's lazy evaluation semantics, we'd
#' # expect to get the caller of that frame as result:
#' fn <- function(eval_fn) {
#'   list(
#'     returned_env = middle(eval_fn),
#'     actual_env = current_env()
#'   )
#' }
#' middle <- function(eval_fn) {
#'   deep(eval_fn, current_env())
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
#'
#' @section Life cycle:
#'
#' These functions are experimental. Expect API changes.
#'
#'
#' @inheritParams eval_bare
#' @param env An environment within which to evaluate `expr`. Can be
#'   an object with a [get_env()] method.
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
#'   g(current_env())
#'   "normal return"
#' }
#' g <- function(env) {
#'   with_env(env, return("early return"))
#' }
#' fn()
#'
#'
#' # Since env is passed to as_environment(), it can be any object with an
#' # as_environment() method. For strings, the pkg_env() is returned:
#' with_env("base", ~mtcars)
#'
#' # This can be handy to put dictionaries in scope:
#' with_env(mtcars, cyl)
with_env <- function(env, expr) {
  .Call(rlang_eval, substitute(expr), as_environment(env, caller_env()))
}
#' @rdname with_env
#' @export
locally <- function(expr) {
  .Call(rlang_eval, substitute(expr), child_env(caller_env()))
}

#' Invoke a function with a list of arguments
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("soft-deprecated")}
#'
#' Normally, you invoke a R function by typing arguments manually. A
#' powerful alternative is to call a function with a list of arguments
#' assembled programmatically. This is the purpose of `invoke()`.
#'
#' @details
#'
#' Technically, `invoke()` is basically a version of [base::do.call()]
#' that creates cleaner call traces because it does not inline the
#' function and the arguments in the call (see examples). To achieve
#' this, `invoke()` creates a child environment of `.env` with `.fn`
#' and all arguments bound to new symbols (see [env_bury()]). It then
#' uses the same strategy as [eval_bare()] to evaluate with minimal
#' noise.
#'
#'
#' @section Life cycle:
#'
#' `invoke()` is soft-deprecated in favour of [exec()]. Now that we
#' understand better the interaction between unquoting and dots
#' capture, we can take a simpler approach in `exec()`.
#'
#' If you need finer control over the generated call, you should construct
#' an environment and call yourself, manually burying large objects
#' or complex expressions.
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
#' @keywords internal
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
  signal_soft_deprecated(c(
    "`invoke()` is deprecated as of rlang 0.4.0.",
    "Please use `exec()` or `eval(expr())`instead."
  ))

  args <- c(.args, list(...))

  if (is_null(.bury) || !length(args)) {
    if (is_scalar_character(.fn)) {
      .fn <- env_get(.env, .fn, inherit = TRUE)
    }
    call <- call2(.fn, !!! args)
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

  call <- call2(.fn, !!! args)
  .Call(rlang_eval, call, .env)
}

value <- function(expr) {
  eval_bare(enexpr(expr), caller_env())
}

eval_top <- function(expr, env = caller_env()) {
  .Call(rlang_eval_top, expr, env)
}


#' Execute a function
#'
#' @description
#'
#' This function constructs and evaluates a call to `.fn`.
#' It has two primary uses:
#'
#' * To call a function with arguments stored in a list (if the function
#'   doesn't support [tidy-dots])
#'
#' * To call every function stored in a list (in conjunction with `map()`/
#'   [lapply()])
#'
#' @param .fn A function, or function name as a string.
#' @param ... Arguments to function.
#'
#'   These dots support [tidy-dots] features.
#' @param  .env Environment in which to evaluate the call. This will be
#'   most useful if `f` is a string, or the function has side-effects.
#' @export
#' @examples
#' args <- list(x = c(1:10, 100, NA), na.rm = TRUE)
#' exec("mean", !!!args)
#' exec("mean", !!!args, trim = 0.2)
#'
#' fs <- list(a = function() "a", b = function() "b")
#' lapply(fs, exec)
#'
#' # Compare to do.call it will not automatically inline expressions
#' # into the evaluated call.
#' x <- 10
#' args <- exprs(x1 = x + 1, x2 = x * 2)
#' exec(list, !!!args)
#' do.call(list, args)
#'
#' # exec() is not designed to generate pretty function calls. This is
#' # most easily seen if you call a function that captures the call:
#' f <- disp ~ cyl
#' exec("lm", f, data = mtcars)
#'
#' # If you need finer control over the generated call, you'll need to
#' # construct it yourself. This may require creating a new environment
#' # with carefully constructed bindings
#' data_env <- env(data = mtcars)
#' eval(expr(lm(!!f, data)), data_env)
exec <- function(.fn, ..., .env = caller_env()) {
  .External2(rlang_exec, .fn, .env)
}
