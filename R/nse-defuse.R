#' Defusing R expressions with `expr()` and `enquo()`
#'
#' @description
#'
#' _Note regarding tidyverse programming:_ `expr()` and `enquo()` are
#' advanced tidy eval operators. Most cases can be solved with `{{`
#' and `...`. You can read about this in [embracing and
#' forwarding][embracing].
#'
#' The defusing operators `expr()` and `enquo()` disable evaluation of
#' R code. When a piece of R code is defused, R doesn't return its
#' value like it normally would. Instead it returns the expression in
#' a special tree-like object that describes how to compute a value.
#' These defused expressions can be thought of as blueprints or
#' recipes for computing values.
#'
#' ```{r}
#' # Return the result of `1 + 1`
#' 1 + 1
#'
#' # Return the expression `1 + 1`
#' expr(1 + 1)
#'
#' # Return the expression and evaluate it
#' e <- expr(1 + 1)
#' eval(e)
#' ```
#'
#' There are two main ways to defuse expressions, to which correspond
#' the two functions `expr()` and `enquo()`:
#'
#' * You can defuse your _own_ R expressions with `expr()`.
#'
#' * You can defuse the expressions supplied by _the user_ of your
#'   function with the `en`-prefixed operators, such as `enquo()` and
#'   `enquos()`. These operators defuse function arguments.
#'
#' One purpose for defusing evaluation of an expression is to
#' interface with data-masking functions with the
#' __defuse-and-inject__ pattern. Function arguments referring to
#' data-variables are defused and then injected with `!!` or `!!!` in
#' a data-masking function where the data-variables are defined.
#'
#' ```
#' my_summarise <- function(data, arg) {
#'   # Defuse the user expression in `arg`
#'   arg <- enquo(arg)
#'
#'   # Inject the expression contained in `arg`
#'   # inside a `summarise()` argument
#'   data |> dplyr::summarise(mean = mean(!!arg, na.rm = TRUE))
#' }
#' ```
#'
#' This pattern of defuse-and-inject can be done in one step with the
#' embracing operator `{{` described in [embracing and
#' forwarding][embracing].
#'
#' ```
#' my_summarise <- function(data, arg) {
#'   # Defuse and inject in a single step with the embracing operator
#'   data |> dplyr::summarise(mean = mean({{ arg }}, na.rm = TRUE))
#' }
#' ```
#'
#' Using `enquo()` and `!!` separately is useful for more complex
#' cases where you need access to the defused expression instead of
#' just passing it on.
#'
#'
#' @section Defused arguments and quosures:
#'
#' If you inspect the return values of `expr()` and `enquo()`, you'll
#' notice that the latter doesn't return a raw expression like the
#' former. Instead it returns a [quosure], a wrapper containing an
#' expression and an environment.
#'
#' ```
#' expr(1 + 1)
#' #> 1 + 1
#'
#' my_function <- function(arg) enquo(arg)
#' my_function(1 + 1)
#' #> <quosure>
#' #> expr: ^1 + 1
#' #> env:  global
#' ```
#'
#' R needs information about the environment to properly evaluate
#' argument expressions because they come from a different context
#' than the current function. For instance when a function in your
#' package calls `dplyr::mutate()`, the quosure environment indicates
#' where all the private functions of your package are defined.
#'
#'
#' @section Comparison with base R:
#'
#' Defusing is known as _quoting_ in other frameworks.
#'
#' - The equivalent of `expr()` is [base::bquote()].
#'
#' - The equivalent of `enquo()` is [base::substitute()]. The latter
#'   returns a naked expression instead of a quosure.
#'
#' - There is no equivalent for `enquos(...)` but you can defuse dots
#'   as a list of naked expressions with `eval(substitute(alist(...)))`.
#'
#' What makes tidy eval work consistently and safely is that defused
#' argument expressions are wrapped in a [quosure]. Unlike a naked
#' expression, a quosure carries information about the context from
#' which an expression comes from. A quosure is evaluated in the
#' original environment of the expression, which allows R to find
#' local data and local functions.
#'
#'
#' @section Types of defused expressions:
#'
#' * __Calls__, like `f(1, 2, 3)` or `1 + 1` represent the action of
#'   calling a function to compute a new value, such as a vector.
#'
#' * __Symbols__, like `x` or `df`, represent named objects. When the
#'   object pointed to by the symbol was defined in a function or in
#'   the global environment, we call it an environment-variable. When
#'   the object is a column in a data frame, we call it a
#'   data-variable.
#'
#' * __Constants__, like `1` or `NULL`.
#'
#' You can create new call or symbol objects by using the defusing
#' function `expr()`:
#'
#' ```
#' # Create a symbol representing objects called `foo`
#' expr(foo)
#' #> foo
#'
#' # Create a call representing the computation of the mean of `foo`
#' expr(mean(foo, na.rm = TRUE))
#' #> mean(foo, na.rm = TRUE)
#'
#' # Return a constant
#' expr(1)
#' #> [1] 1
#'
#' expr(NULL)
#' #> NULL
#' ```
#'
#' Defusing is not the only way to create defused expressions. You can
#' also assemble them from data:
#'
#' ```
#' # Assemble a symbol from a string
#' var <- "foo"
#' sym(var)
#'
#' # Assemble a call from strings, symbols, and constants
#' call("mean", sym(var), na.rm = TRUE)
#' ```
#'
#' @inheritParams dots_list
#' @param expr An expression.
#' @param arg A symbol representing an argument. The expression
#'   supplied to that argument will be captured instead of being
#'   evaluated.
#' @param ... For `enexprs()`, `ensyms()` and `enquos()`, names of
#'   arguments to capture without evaluation (including `...`). For
#'   `exprs()` and `quos()`, the expressions to capture unevaluated
#'   (including expressions contained in `...`).
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty. Named arguments are not
#'   considered empty.
#' @param .unquote_names Whether to treat `:=` as `=`. Unlike `=`, the
#'   `:=` syntax supports `!!` unquoting on the LHS.
#'
#' @seealso [Advanced tidyeval operators][defusing-advanced].
#'
#' @examples
#' # `expr()` defuses the expression that you supply
#' expr(1 + 1)
#'
#' # `enquo()` defuses the expression supplied by your user
#' my_function <- function(arg) {
#'   enquo(arg)
#' }
#' my_function(1 + 1)
#'
#' # `enquos() works with arguments and dots and returns a list of
#' # expressions`
#' my_function <- function(...) {
#'   enquos(...)
#' }
#' my_function(1 + 1, 2 * 10)
#'
#'
#' # `expr()` and `enquo()` support _injection_, a convenient way of
#' # modifying part of an expression by injecting other objects.
#' column <- sym("cyl")
#' expr(mean(!!column, na.rm = TRUE))
#'
#' columns <- syms(c("cyl", "am"))
#' expr(mean(!!columns[[1]] * !!columns[[2]], na.rm = TRUE))
#' expr(list(!!!columns))
#'
#' # Using `enquo()` enables `{{ arg }}` embracing in particular
#' other_function <- function(arg) {
#'   my_function({{ arg }} * 2)
#' }
#' other_function(100)
#' 
#' other_function(!!column)
#'
#' @name defusing
#' @aliases quotation nse-defuse
NULL

#' @rdname defusing
#' @export
expr <- function(expr) {
  enexpr(expr)
}
#' @rdname defusing
#' @export
enquo <- function(arg) {
  .Call(ffi_enquo, substitute(arg), parent.frame())
}
#' @rdname defusing
#' @export
enquos <- function(...,
                   .named = FALSE,
                   .ignore_empty = c("trailing", "none", "all"),
                   .unquote_names = TRUE,
                   .homonyms = c("keep", "first", "last", "error"),
                   .check_assign = FALSE) {
  quos <- endots(
    call = sys.call(),
    frame_env = parent.frame(),
    capture_arg = ffi_enquo,
    capture_dots = ffi_quos_interp,
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
  structure(quos, class = c("quosures", "list"))
}

#' Advanced defusal operators
#'
#' @description
#' [expr()], [enquo()], and [enquos()] are sufficient for most
#' purposes. rlang provides other operations for completeness which
#' are summarised here.
#'
#' *   `exprs()` is the plural variant of `expr()`. It returns a list of
#'     expressions. It is like [base::alist()] but with
#'     [injection][nse-inject] support.
#'
#' *   `quo()` and `quos()` are like `expr()` and `exprs()` but return
#'     quosures instead of naked expressions. When you are defusing
#'     your own local expressions (by opposition to function arguments
#'     where non-local expressions are supplied by your users), there
#'     is generally no need to attach the current environment in a
#'     quosure. See [When are quosures needed?][faq-quosure].
#'
#' *   `enexpr()` and `enexprs()` are like [enquo()] and [enquos()] but
#'     return naked expressions instead of quosures. These operators
#'     should very rarely be used because they lose track of the
#'     environment of defused arguments.
#'
#' *   `ensym()` and `ensyms()` are like `enexpr()` and `enexprs()` but
#'     they throw an error when the defused expressions are not simple
#'     symbols. They also support strings which are interpreted as
#'     symbols. These functions are modelled on the behaviour of the
#'     left-hand side of `=` and `<-` where you can supply symbols and
#'     strings interchangeably.
#'
#'     ```
#'     "foo" <- NULL
#'     list("foo" = NULL)
#'     ```
#'
#' *   `enquo0` and `enquos0()` are like `enquo()` and `enquos()` but
#'     without injection support. The injection operators `!!`, `!!!`,
#'     and `{{` are not processed, instead they are preserved in the
#'     defused expression. This makes it possible to defuse
#'     expressions that potentially contain injection operators meant
#'     for later use.
#'
#'     The flip side is that this makes it harder for users to inject
#'     expressions in your function. They have to do it explicitly
#'     with [inject()].
#'
#'     None of the features of [dynamic dots][dyn-dots] are available
#'     when defusing with `enquos0()`. For instance, trailing empty
#'     arguments are not automatically trimmed.
#'
#' @inheritParams defusing
#'
#' @examples
#' # `exprs()` is the plural variant of `expr()`
#' exprs(foo, bar, bar)
#'
#' # `quo()` and `quos()` are the quosure variants of `expr()` and `exprs()`
#' quo(foo)
#' quos(foo, bar)
#'
#' # `enexpr()` and `enexprs()` are the naked variants of `enquo()` and `enquos()`
#' my_function1 <- function(arg) enexpr(arg)
#' my_function2 <- function(arg, ...) enexprs(arg, ...)
#' my_function1(1 + 1)
#' my_function2(1 + 1, 10 * 2)
#'
#'
#' # `ensym()` and `ensyms()` are symbol variants of `enexpr()` and `enexprs()`
#' my_function3 <- function(arg) ensym(arg)
#' my_function4 <- function(arg, ...) ensyms(arg, ...)
#'
#' # The user must supply symbols
#' my_function3(foo)
#' my_function4(foo, bar)
#'
#' # Complex expressions are an error
#' try(my_function3(1 + 1))
#' try(my_function4(1 + 1, 10 * 2))
#'
#'
#' # `enquo0()` and `enquos0()` disable injection operators
#' automatic_injection <- function(x) enquo(x)
#' no_injection <- function(x) enquo0(x)
#'
#' automatic_injection(foo(!!!1:3))
#' no_injection(foo(!!!1:3))
#'
#' # Injection can still be done explicitly
#' inject(no_injection(foo(!!!1:3)))
#' @name defusing-advanced
NULL

#' @rdname defusing-advanced
#' @export
enexpr <- function(arg) {
  .Call(ffi_enexpr, substitute(arg), parent.frame())
}

#' @rdname defusing-advanced
#' @export
exprs <- function(...,
                  .named = FALSE,
                  .ignore_empty = c("trailing", "none", "all"),
                  .unquote_names = TRUE) {
  .Call(ffi_exprs_interp,
    frame_env = environment(),
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = "keep",
    check_assign = FALSE
  )
}
#' @rdname defusing-advanced
#' @export
enexprs <- function(...,
                   .named = FALSE,
                   .ignore_empty = c("trailing", "none", "all"),
                   .unquote_names = TRUE,
                   .homonyms = c("keep", "first", "last", "error"),
                   .check_assign = FALSE) {
  endots(
    call = sys.call(),
    frame_env = parent.frame(),
    capture_arg = ffi_enexpr,
    capture_dots = ffi_exprs_interp,
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
}

#' @rdname defusing-advanced
#' @export
ensym <- function(arg) {
  .Call(ffi_ensym, substitute(arg), parent.frame())
}
#' @rdname defusing-advanced
#' @export
ensyms <- function(...,
                   .named = FALSE,
                   .ignore_empty = c("trailing", "none", "all"),
                   .unquote_names = TRUE,
                   .homonyms = c("keep", "first", "last", "error"),
                   .check_assign = FALSE) {
  exprs <- endots(
    call = sys.call(),
    frame_env = parent.frame(),
    capture_arg = ffi_enexpr,
    capture_dots = ffi_exprs_interp,
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = .homonyms,
    check_assign = .check_assign
  )
  map(exprs, function(expr) {
    if (is_quosure(expr)) {
      expr <- quo_get_expr(expr)
    }
    sym(expr)
  })
}


#' @rdname defusing-advanced
#' @export
quo <- function(expr) {
  enquo(expr)
}
#' @rdname defusing-advanced
#' @export
quos <- function(...,
                 .named = FALSE,
                 .ignore_empty = c("trailing", "none", "all"),
                 .unquote_names = TRUE) {
  .Call(ffi_quos_interp,
    frame_env = environment(),
    named = .named,
    ignore_empty = .ignore_empty,
    unquote_names = .unquote_names,
    homonyms = "keep",
    check_assign = FALSE
  )
}

#' @rdname defusing-advanced
#' @export
enquo0 <- function(arg) {
  info <- .External(ffi_capturearginfo, environment(), parent.frame())
  as_quosure(info$expr, info$env)
}
#' @rdname defusing-advanced
#' @export
enquos0 <- function(...) {
  dots <- .External(ffi_capturedots, environment())
  lapply(dots, function(dot) as_quosure(dot$expr, dot$env))
}


capture_args <- c(
  ".named",
  ".ignore_empty",
  ".unquote_names",
  ".homonyms",
  ".check_assign"
)

endots <- function(call,
                   frame_env,
                   capture_arg,
                   capture_dots,
                   named,
                   ignore_empty,
                   unquote_names,
                   homonyms,
                   check_assign) {
  ignore_empty <- arg_match0(ignore_empty, c("trailing", "none", "all"))
  syms <- as.list(node_cdr(call))

  if (!is_null(names(syms))) {
    is_arg <- names(syms) %in% capture_args
    syms <- syms[!is_arg]
  }

  # Avoid note about registration problems
  dot_call <- .Call

  splice_dots <- FALSE
  dots <- map(syms, function(sym) {
    if (!is_symbol(sym)) {
      abort("Inputs to capture must be argument names")
    }
    if (identical(sym, dots_sym)) {
      splice_dots <<- TRUE
      splice(dot_call(capture_dots,
        frame_env = frame_env,
        named = named,
        ignore_empty = ignore_empty,
        unquote_names = unquote_names,
        homonyms = homonyms,
        check_assign = check_assign
      ))
    } else {
      dot_call(capture_arg, sym, frame_env)
    }
  })

  if (splice_dots) {
    dots <- flatten_if(dots, is_spliced)
  }

  if (ignore_empty == "all") {
    if (identical(capture_arg, ffi_enquo)) {
      dot_is_missing <- quo_is_missing
    } else {
      dot_is_missing <- is_missing
    }

    is_missing <- map_lgl(dots, dot_is_missing)
    is_named <- detect_named(dots)
    is_dev_supplied <- is_named & names2(dots) %in% names(syms)

    # Named missing arguments supplied by the developer are considered
    # empty. Named missing arguments supplied by the user through
    # `...` are not considered empty, consistently with `quos()`.
    is_empty <- is_missing & (is_dev_supplied | !is_named)

    dots <- discard(dots, is_empty)
  }

  if (is_true(named)) {
    dots <- quos_auto_name(dots)
  } else if (is_false(named)) {
    names(dots) <- names2(dots)
  } else if (!is_null(named)) {
    abort("`.named` must be a logical value.")
  }

  dots
}

#' Ensure that all elements of a list of expressions are named
#'
#' This gives default names to unnamed elements of a list of
#' expressions (or expression wrappers such as formulas or
#' quosures). `exprs_auto_name()` deparses the expressions with
#' [expr_name()] by default. `quos_auto_name()` deparses with
#' [quo_name()].
#'
#' @param exprs A list of expressions.
#' @inheritParams args_dots_empty
#' @param repair_auto Whether to repair the automatic names. By
#'   default, minimal names are returned. See `?vctrs::vec_as_names`
#'   for information about name repairing.
#' @param repair_quiet Whether to inform user about repaired names.
#' @param width `r lifecycle::badge("deprecated")` Maximum width of
#'   names.
#' @param printer `r lifecycle::badge("deprecated")` A function that
#'   takes an expression and converts it to a string. This function
#'   must take an expression as the first argument and `width` as the
#'   second argument.
#' @export
exprs_auto_name <- function(exprs,
                            ...,
                            repair_auto = c("minimal", "unique"),
                            repair_quiet = FALSE,
                            width = deprecated(),
                            printer = deprecated()) {
  check_dots_empty0(...)
  repair_auto <- arg_match0(repair_auto, c("minimal", "unique"))

  if (!is_missing(width) && !is_null(width)) {
    warn_deprecated(paste_line(
      "The `width` argument is deprecated as of rlang 0.3.0."
    ))
  }
  if (!is_missing(printer) && !is_null(width)) {
    warn_deprecated(paste_line(
      "The `printer` argument is deprecated as of rlang 0.3.0."
    ))
  }

  named <- detect_named(exprs)
  if (all(named)) {
    return(exprs)
  }

  names <- names(exprs)

  auto_names <- map_chr(exprs[!named], as_label)
  names[!named] <- auto_names

  if (repair_auto == "unique" && anyDuplicated(auto_names)) {
    orig <- names
    unique_names <- names_as_unique(names, quiet = TRUE)
    names[!named] <- unique_names[!named]

    if (!repair_quiet) {
      names_inform_repair(orig, names)
    }
  }

  names(exprs) <- names
  exprs
}
#' @rdname exprs_auto_name
#' @param quos A list of quosures.
#' @export
quos_auto_name <- function(quos, width = NULL) {
  exprs_auto_name(quos, width = width)
}


captureArgInfo <- function(arg) {
  .External(ffi_capturearginfo, environment(), parent.frame())
}
captureDots <- function() {
  .External(ffi_capturedots, parent.frame())
}


# Enable glue syntax in name-unquoting when glue is loaded
on_load(
  on_package_load("glue", .Call(ffi_glue_is_here))
)
