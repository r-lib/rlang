#' Evaluate an expression in an environment
#'
#' @description
#' `eval_bare()` is a lower-level version of function [base::eval()].
#' Technically, it is a simple wrapper around the C function
#' `Rf_eval()`. You generally don't need to use `eval_bare()` instead
#' of `eval()`. Its main advantage is that it handles stack-sensitive
#' calls (such as `return()`, `on.exit()` or `parent.frame()`) more
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
  .External2(ffi_eval, expr, env)
}

eval_top <- function(expr, env = caller_env()) {
  .Call(ffi_eval_top, expr, env)
}


#' Execute a function
#'
#' @description
#'
#' This function constructs and evaluates a call to `.fn`.
#' It has two primary uses:
#'
#' * To call a function with arguments stored in a list (if the
#'   function doesn't support [dynamic dots][dyn-dots]). Splice the
#'   list of arguments with `!!!`.
#'
#' * To call every function stored in a list (in conjunction with `map()`/
#'   [lapply()])
#'
#' @param .fn A function, or function name as a string.
#' @param ... <[dynamic][dyn-dots]> Arguments for `.fn`.
#' @param  .env Environment in which to evaluate the call. This will be
#'   most useful if `.fn` is a string, or the function has side-effects.
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
  .External2(ffi_exec, .fn, .env)
}

#' Inject objects in an R expression
#'
#' @description
#'
#' `inject()` evaluates an expression with [injection][quasiquotation]
#' support. There are three main usages:
#'
#' - [Splicing][!!!] lists of arguments in a function call.
#'
#' - Inline objects or other expressions in an expression with `!!`
#'   and `!!!`. For instance to create functions or formulas
#'   programmatically.
#'
#' - Pass arguments to NSE functions that [defuse][nse-defuse] their
#'   arguments without injection support (see for instance
#'   [enquo0()]). You can use `{{ arg }}` with functions documented
#'   to support quosures. Otherwise, use `!!enexpr(arg)`.
#'
#' @param expr An argument to evaluate. This argument is immediately
#'   evaluated in `env` (the current environment by default) with
#'   injected objects and expressions.
#' @param env The environment in which to evaluate `expr`. Defaults to
#'   the current environment. For expert use only.
#'
#' @export
#' @examples
#' # inject() simply evaluates its argument with injection
#' # support. These expressions are equivalent:
#' 2 * 3
#' inject(2 * 3)
#' inject(!!2 * !!3)
#'
#' # Injection with `!!` can be useful to insert objects or
#' # expressions within other expressions, like formulas:
#' lhs <- sym("foo")
#' rhs <- sym("bar")
#' inject(!!lhs ~ !!rhs + 10)
#'
#' # Injection with `!!!` splices lists of arguments in function
#' # calls:
#' args <- list(na.rm = TRUE, finite = 0.2)
#' inject(mean(1:10, !!!args))
inject <- function(expr, env = caller_env()) {
  .External2(ffi_eval, enexpr(expr), env)
}

eval_parse <- function(code, env = caller_env()) {
  file <- tempfile("rlang_eval_parsed_", fileext = ".R")
  on.exit(if (file.exists(file)) file.remove(file))

  writeLines(code, file)
  exprs <- parse(file, keep.source = TRUE)

  out <- NULL
  for (expr in exprs) {
    out <- eval_bare(expr, env)
  }

  out
}
