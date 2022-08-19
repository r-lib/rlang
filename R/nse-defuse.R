#' Embrace operator `{{`
#'
#' @description
#'
#' The embrace operator `{{` is used to create functions that call
#' other [data-masking][topic-data-mask] functions. It transports a
#' data-masked argument (an argument that can refer to columns of a
#' data frame) from one function to another.
#'
#' ```r
#' my_mean <- function(data, var) {
#'   dplyr::summarise(data, mean = mean({{ var }}))
#' }
#' ```
#'
#' @section Under the hood:
#'
#' `{{` combines [enquo()] and [`!!`][injection-operator] in one
#' step. The snippet above is equivalent to:
#'
#' ```r
#' my_mean <- function(data, var) {
#'   var <- enquo(var)
#'   dplyr::summarise(data, mean = mean(!!var))
#' }
#' ```
#'
#' @name embrace-operator
#' @aliases curly-curly
#'
#' @seealso
#' - `r link("topic_data_mask")`
#' - `r link("topic_data_mask_programming")`
#'
NULL


#' Defuse an R expression
#'
#' @description
#'
#' `expr()` [defuses][topic-defuse] an R expression with
#' [injection][injection-operator] support.
#'
#' It is equivalent to [base::bquote()].
#'
#' @usage NULL
#' @param expr An expression to defuse.
#'
#' @seealso
#' - `r link("topic_defuse")` for an overview.
#'
#' - [enquo()] to defuse non-local expressions from function
#'   arguments.
#'
#' - [Advanced defusal operators][defusing-advanced].
#'
#' - [sym()] and [call2()] for building expressions (symbols and calls
#'   respectively) programmatically.
#'
#' - [base::eval()] and [rlang::eval_bare()] for resuming evaluation
#'   of a defused expression.
#'
#' @examples
#' # R normally returns the result of an expression
#' 1 + 1
#'
#' # `expr()` defuses the expression that you have supplied and
#' # returns it instead of its value
#' expr(1 + 1)
#'
#' expr(toupper(letters))
#'
#' # It supports _injection_ with `!!` and `!!!`. This is a convenient
#' # way of modifying part of an expression by injecting other
#' # objects.
#' var <- "cyl"
#' expr(with(mtcars, mean(!!sym(var))))
#'
#' vars <- c("cyl", "am")
#' expr(with(mtcars, c(!!!syms(vars))))
#'
#' # Compare to the normal way of building expressions
#' call("with", call("mean", sym(var)))
#'
#' call("with", call2("c", !!!syms(vars)))
#'
#' @export
expr <- function(expr) {
  enexpr(expr)
}

#' Defuse function arguments
#'
#' @description
#'
#' `enquo()` and `enquos()` [defuse][topic-defuse] function arguments.
#' A defused expression can be examined, modified, and injected into
#' other expressions.
#'
#' Defusing function arguments is useful for:
#'
#' - Creating data-masking functions.
#' - Interfacing with another [data-masking][topic-data-mask] function
#'   using the [defuse-and-inject][topic-metaprogramming] pattern.
#'
#' These are advanced tools. Make sure to first learn about the embrace
#' operator `r link("{{")` in `r link("topic_data_mask_programming")`.
#' `{{` is easier to work with less theory, and it is sufficient
#' in most applications.
#'
#' @inheritParams dots_list
#' @param arg An unquoted argument name. The expression
#'   supplied to that argument is defused and returned.
#' @param ... Names of arguments to defuse.
#' @param .ignore_empty Whether to ignore empty arguments. Can be one
#'   of `"trailing"`, `"none"`, `"all"`. If `"trailing"`, only the
#'   last argument is ignored if it is empty. Named arguments are not
#'   considered empty.
#' @param .unquote_names Whether to treat `:=` as `=`. Unlike `=`, the
#'   `:=` syntax supports [names injection][glue-operators].
#' @return `enquo()` returns a [quosure][topic-quosure] and `enquos()`
#'   returns a list of quosures.
#'
#' @section Implicit injection:
#'
#' Arguments defused with `enquo()` and `enquos()` automatically gain
#' [injection][topic-inject] support.
#'
#' ```r
#' my_mean <- function(data, var) {
#'   var <- enquo(var)
#'   dplyr::summarise(data, mean(!!var))
#' }
#'
#' # Can now use `!!` and `{{`
#' my_mean(mtcars, !!sym("cyl"))
#' ```
#'
#' See [enquo0()] and [enquos0()] for variants that don't enable
#' injection.
#'
#' @seealso
#' - `r link("topic_defuse")` for an overview.
#'
#' - [expr()] to defuse your own local expressions.
#'
#' - [Advanced defusal operators][defusing-advanced].
#'
#' - [base::eval()] and [rlang::eval_bare()] for resuming evaluation
#'   of a defused expression.
#'
#' @examples
#' # `enquo()` defuses the expression supplied by your user
#' f <- function(arg) {
#'   enquo(arg)
#' }
#'
#' f(1 + 1)
#'
#' # `enquos()` works with arguments and dots. It returns a list of
#' # expressions
#' f <- function(...) {
#'   enquos(...)
#' }
#'
#' f(1 + 1, 2 * 10)
#'
#'
#' # `enquo()` and `enquos()` enable _injection_ and _embracing_ for
#' # your users
#' g <- function(arg) {
#'   f({{ arg }} * 2)
#' }
#' g(100)
#'
#' column <- sym("cyl")
#' g(!!column)
#'
#' @export
enquo <- function(arg) {
  .Call(ffi_enquo, substitute(arg), parent.frame())
}
#' @rdname enquo
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
#'
#' These advanced operators [defuse][topic-defuse] R expressions.
#' [expr()], [enquo()], and [enquos()] are sufficient for most
#' purposes but rlang provides these other operations, either for
#' completeness or because they are useful to experts.
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
#'     quosure. See `r link("topic_quosure")`.
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
#'     for later use. The trade off is that it makes it harder for
#'     users to inject expressions in your function. They have to
#'     enable injection explicitly with [inject()].
#'
#'     None of the features of [dynamic dots][dyn-dots] are available
#'     when defusing with `enquos0()`. For instance, trailing empty
#'     arguments are not automatically trimmed.
#'
#' @inheritParams expr
#' @inheritParams enquo
#' @param ... For `enexprs()`, `ensyms()` and `enquos()`, names of
#'   arguments to defuse. For `exprs()` and `quos()`, expressions
#'   to defuse.
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
#'
#' @name defusing-advanced
#' @keywords internal
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
                   check_assign,
                   error_call = caller_env()) {
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
      abort(
        "Inputs to defuse must be argument names.",
        call = error_call
      )
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
    check_bool(named, arg = ".named", call = error_call)
  }

  dots
}

#' Ensure that all elements of a list of expressions are named
#'
#' This gives default names to unnamed elements of a list of
#' expressions (or expression wrappers such as formulas or
#' quosures), deparsed with [as_label()].
#'
#' @param exprs A list of expressions.
#' @inheritParams args_dots_empty
#' @param repair_auto Whether to repair the automatic names. By
#'   default, minimal names are returned. See `?vctrs::vec_as_names`
#'   for information about name repairing.
#' @param repair_quiet Whether to inform user about repaired names.
#' @export
exprs_auto_name <- function(exprs,
                            ...,
                            repair_auto = c("minimal", "unique"),
                            repair_quiet = FALSE) {
  check_dots_empty0(...)
  repair_auto <- arg_match0(repair_auto, c("minimal", "unique"))

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
quos_auto_name <- function(quos) {
  exprs_auto_name(quos)
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
