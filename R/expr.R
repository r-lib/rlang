#' Is an object an expression?
#'
#' @description
#'
#' These helpers are consistent wrappers around their base R
#' equivalents. `is_expr()` tests for expressions, the set of objects
#' that can be obtained from parsing R code. An expression can be one
#' of two things: either a symbolic object (for which `is_symbolic()`
#' returns `TRUE`), or a parsable literal (testable with
#' `is_parsable_literal()`). Note that we are using the term
#' expression in its colloquial sense and not to refer to
#' [expression()] vectors, a data type that wraps expressions in a
#' vector and which has not much use in R.
#'
#' Technically, a call can contain any R object, not necessarily
#' language objects. However, this only happens in artificial
#' situations. Expressions as we define them only contain numbers,
#' strings, `NULL`, symbols, and calls: this is the complete set of R
#' objects that are created when R parses source code (e.g. from using
#' [parse_expr()]). These objects can be classified as literals and
#' symbolic objects. Symbolic objects like symbols and calls are
#' treated specially when R evaluates an expression. When a symbol is
#' evaluated, it is looked up and replaced by its value. When a call
#' is evaluated, its arguments are recursively evaluated, and the
#' corresponding function is called, and the call is replaced by the
#' returned value. On the other hand, literal objects, such as numbers
#' and strings, just return their own value. To sum up, an expression
#' can either be symbolic or a parsable literal.
#'
#' @details
#'
#' `is_symbolic()` returns `TRUE` for symbols and calls (objects with
#' type `language`). Literals are the complement of symbolic
#' objects. `is_parsable_literal()` is a predicate that returns `TRUE`
#' for the subset of literals that are created by R when parsing text
#' (see [parse_expr()]): numbers, strings and `NULL`. Along with
#' symbols, these literals are the terminating nodes in a parse tree.
#'
#' Note that in the most general sense, a literal is any R object that
#' evaluates to itself and that can be evaluated in the empty
#' environment. For instance, `quote(c(1, 2))` is not a literal, it is
#' a call. However, the result of evaluating it in [base_env()] is a
#' literal(in this case an atomic vector).
#'
#' Pairlists are also a kind of language objects. However, since they
#' are mostly an internal data structure, `is_expr()` returns `FALSE`
#' for pairlists. You can use `is_pairlist()` to explicitly check for
#' them. Pairlists are the data structure for function arguments. They
#' usually do not arise from R code because subsetting a call is a
#' type-preserving operation. However, you can obtain the pairlist of
#' arguments by taking the CDR of the call object from C code. The
#' rlang function [lang_tail()] will do it from R. Another way in
#' which pairlist of arguments arise is by extracting the argument
#' list of a closure with [base::formals()] or [fn_fmls()].
#'
#' @param x An object to test. When you supply a tidy quote (see
#'   [quo()]) to any of the expression predicates, they will
#'   perform their test on the RHS of the formula.
#' @seealso [is_lang()] for a call predicate; [as_symbol()] and
#'   [as_lang()] for coercion functions.
#' @export
#' @examples
#' q1 <- quote(1)
#' is_expr(q1)
#' is_parsable_literal(q1)
#'
#' q2 <- quote(x)
#' is_expr(q2)
#' is_symbol(q2)
#'
#' q3 <- quote(x + 1)
#' is_expr(q3)
#' is_lang(q3)
#'
#'
#' # Since tidy quotes are an important way of representing
#' # expressions in R, all expression predicates will test the RHS of
#' # the formula if you supply one:
#' is_symbol(~foo)
#' is_lang(~foo)
#' is_symbol(~foo(bar))
#' is_lang(~foo(bar))
#'
#'
#' # Atomic expressions are the terminating nodes of a call tree:
#' # NULL or a scalar atomic vector:
#' is_parsable_literal("string")
#' is_parsable_literal(NULL)
#'
#' is_parsable_literal(letters)
#' is_parsable_literal(quote(call()))
#'
#' # Parsable literals have the property of being self-quoting:
#' identical("foo", quote("foo"))
#' identical(1L, quote(1L))
#' identical(NULL, quote(NULL))
#'
#' # Like any literals, they can be evaluated within the empty
#' # environment:
#' eval(quote(1L), empty_env())
#'
#' # Whereas it would fail for symbolic expressions:
#' # eval(quote(c(1L, 2L)), empty_env())
#'
#'
#' # Pairlists are also language objects representing argument lists.
#' # You will usually encounter them with extracted formals:
#' fmls <- formals(is_expr)
#' typeof(fmls)
#'
#' # Since they are mostly an internal data structure, is_expr()
#' # returns FALSE for pairlists, so you will have to check explicitly
#' # for them:
#' is_expr(fmls)
#' is_pairlist(fmls)
#'
#' # Note that you can also extract call arguments as a pairlist:
#' lang_tail(quote(fn(arg1, arg2 = "foo")))
is_expr <- function(x) {
  x <- get_expr(x)
  is_symbolic(x) || is_parsable_literal(x)
}
#' @export
#' @rdname is_expr
is_parsable_literal <- function(x) {
  x <- get_expr(x)
  typeof(x) == "NULL" || (length(x) == 1 && typeof(x) %in% parsable_atomic_types)
}
#' @export
#' @rdname is_expr
is_symbolic <- function(x) {
  x <- get_expr(x)
  typeof(x) %in% c("language", "symbol")
}


#' Evaluate an expression in an environment.
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
#' @useDynLib rlang rlang_eval
#' @seealso with_env
#' @export
#' @examples
#' # eval_bare() works just like base::eval():
#' env <- child_env(data = list(foo = "bar"))
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


#' Turn an expression to a label.
#'
#' `expr_text()` turns the expression into a single string, which
#' might be multi-line. `expr_name()` is suitable for formatting
#' names. It works best with symbols and scalar types, but also
#' accepts calls. `expr_label()` formats the expression nicely for use
#' in messages.
#'
#' @param expr An expression to labellise.
#' @export
#' @examples
#' # To labellise a function argument, first capture it with
#' # substitute():
#' fn <- function(x) expr_label(substitute(x))
#' fn(x:y)
#'
#' # Strings are encoded
#' expr_label("a\nb")
#'
#' # Names and expressions are quoted with ``
#' expr_label(quote(x))
#' expr_label(quote(a + b + c))
#'
#' # Long expressions are collapsed
#' expr_label(quote(foo({
#'   1 + 2
#'   print(x)
#' })))
expr_label <- function(expr) {
  if (is.character(expr)) {
    encodeString(expr, quote = '"')
  } else if (is.atomic(expr)) {
    format(expr)
  } else if (is.name(expr)) {
    paste0("`", as.character(expr), "`")
  } else {
    chr <- deparse(expr)
    if (length(chr) > 1) {
      dot_call <- new_language(expr[[1]], quote(...))
      chr <- paste(deparse(dot_call), collapse = "\n")
    }
    paste0("`", chr, "`")
  }
}
#' @rdname expr_label
#' @export
expr_name <- function(expr) {
  name <- switch_type(expr,
    symbol = as_name(expr),
    quosure = ,
    language = {
      expr_text <- deparse(expr)
      paste0("(", expr_text, ")")
    },
    logical = ,
    integer = ,
    double = ,
    complex = ,
    string = ,
    character = {
      if (length(expr) == 1) {
        as.character(expr)
      }
    }
  )

  if (is_null(name)) {
    abort("`expr` must quote a symbol, scalar, or call")
  } else {
    name
  }
}
#' @rdname expr_label
#' @export
#' @param width Width of each line.
#' @param nlines Maximum number of lines to extract.
expr_text <- function(expr, width = 60L, nlines = Inf) {
  str <- deparse(expr, width.cutoff = width)

  if (length(str) > nlines) {
    str <- c(str[seq_len(nlines - 1)], "...")
  }

  paste0(str, collapse = "\n")
}

#' Set and get an expression.
#'
#' These helpers are useful to make your function work generically
#' with tidy quotes and raw expressions. First call `get_expr()` to
#' extract an expression. Once you're done processing the expression,
#' call `set_expr()` on the original object to update the expression.
#' You can return the result of `set_expr()`, either a formula or an
#' expression depending on the input type. Note that `set_expr()` does
#' not change its input, it creates a new object.
#'
#' `as_generic_expr()` is helpful when your function accepts frames as
#' input but should be able to call `set_expr()` at the
#' end. `set_expr()` does not work on frames because it does not make
#' sense to modify this kind of object. In this case, first call
#' `as_generic_expr()` to transform the input to an object that
#' supports `set_expr()`. It transforms frame objects to a raw
#' expression, and return formula quotes and raw expressions without
#' changes.
#'
#' @param x An expression or one-sided formula. In addition,
#'   `set_expr()` and `as_generic_expr()` accept frames.
#' @param value An updated expression.
#' @return The updated original input for `set_expr()`. A raw
#'   expression for `get_expr()`. `as_generic_expr()` returns an
#'   expression or formula quote.
#' @export
#' @examples
#' f <- ~foo(bar)
#' e <- quote(foo(bar))
#' frame <- identity(identity(ctxt_frame()))
#'
#' get_expr(f)
#' get_expr(e)
#' get_expr(frame)
#'
#' as_generic_expr(f)
#' as_generic_expr(e)
#' as_generic_expr(frame)
#'
#' set_expr(f, quote(baz))
#' set_expr(e, quote(baz))
set_expr <- function(x, value) {
  if (is_quosureish(x)) {
    f_rhs(x) <- value
    x
  } else {
    value
  }
}
#' @rdname set_expr
#' @export
get_expr <- function(x) {
  if (is_quosureish(x)) {
    f_rhs(x)
  } else if (inherits(x, "frame")) {
    x$expr
  } else {
    x
  }
}
#' @rdname set_expr
#' @export
as_generic_expr <- function(x) {
  if (is_frame(x)) {
    x$expr
  } else {
    x
  }
}


expr_type_of <- function(x) {
  type <- typeof(x)
  if (type %in% c("symbol", "language", "pairlist", "NULL")) {
    type
  } else {
    "literal"
  }
}
switch_expr <- function(.x, ...) {
  switch(expr_type_of(.x), ...)
}
