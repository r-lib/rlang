#' Parse R code
#'
#' @description
#' These functions parse and transform text into R expressions. This
#' is the first step to interpret or evaluate a piece of R code
#' written by a programmer.
#'
#' * `parse_expr()` returns one expression. If the text contains more
#'   than one expression (separated by semicolons or new lines), an
#'   error is issued. On the other hand `parse_exprs()` can handle
#'   multiple expressions. It always returns a list of expressions
#'   (compare to [base::parse()] which returns a base::expression
#'   vector). All functions also support R connections.
#'
#' * `parse_expr()` concatenates `x` with `\\n` separators prior to
#'   parsing in order to support the roundtrip
#'   `parse_expr(expr_deparse(x))` (deparsed expressions might be
#'   multiline). On the other hand, `parse_exprs()` doesn't do any
#'   concatenation because it's designed to support named inputs. The
#'   names are matched to the expressions in the output, which is
#'   useful when a single named string creates multiple expressions.
#'
#'   In other words, `parse_expr()` supports vector of lines whereas
#'   `parse_exprs()` expects vectors of complete deparsed expressions.
#'
#' * `parse_quo()` and `parse_quos()` are variants that create a
#'   [quosure][quo]. Supply `env = current_env()` if you're parsing
#'   code to be evaluated in your current context. Supply `env =
#'   global_env()` when you're parsing external user input to be
#'   evaluated in user context.
#'
#'   Unlike quosures created with [enquo()], [enquos()], or `{{`, a
#'   parsed quosure never contains injected quosures. It is thus safe
#'   to evaluate them with `eval()` instead of [eval_tidy()], though
#'   the latter is more convenient as you don't need to extract `expr`
#'   and `env`.
#'
#' @details
#' Unlike [base::parse()], these functions never retain source reference
#' information, as doing so is slow and rarely necessary.
#'
#' @param x Text containing expressions to parse_expr for
#'   `parse_expr()` and `parse_exprs()`. Can also be an R connection,
#'   for instance to a file. If the supplied connection is not open,
#'   it will be automatically closed and destroyed.
#' @return `parse_expr()` returns an [expression][is_expression],
#'   `parse_exprs()` returns a list of expressions. Note that for the
#'   plural variants the length of the output may be greater than the
#'   length of the input. This would happen is one of the strings
#'   contain several expressions (such as `"foo; bar"`). The names of
#'   `x` are preserved (and recycled in case of multiple expressions).
#'   The `_quo` suffixed variants return quosures.
#' @seealso [base::parse()]
#' @export
#' @examples
#' # parse_expr() can parse any R expression:
#' parse_expr("mtcars %>% dplyr::mutate(cyl_prime = cyl / sd(cyl))")
#'
#' # A string can contain several expressions separated by ; or \n
#' parse_exprs("NULL; list()\n foo(bar)")
#'
#' # Use names to figure out which input produced an expression:
#' parse_exprs(c(foo = "1; 2", bar = "3"))
#'
#' # You can also parse source files by passing a R connection. Let's
#' # create a file containing R code:
#' path <- tempfile("my-file.R")
#' cat("1; 2; mtcars", file = path)
#'
#' # We can now parse it by supplying a connection:
#' parse_exprs(file(path))
parse_expr <- function(x) {
  if (is_character(x)) {
    exprs <- chr_parse(paste_line(x))
  } else {
    exprs <- parse_exprs(x)
  }

  n <- length(exprs)
  if (n != 1) {
    abort(sprintf(
      "%s must contain exactly 1 expression, not %d.",
      format_arg("x"),
      n
    ))
  }

  exprs[[1]]
}
#' @rdname parse_expr
#' @export
parse_exprs <- function(x) {
  if (inherits(x, "connection")) {
    if (!isOpen(x)) {
      open(x)
      on.exit(close(x))
    }
    exprs <- parse(file = x, keep.source = FALSE)
  } else if (is.character(x)) {
    exprs <- chr_parse_exprs(x)
  } else {
    stop_input_type(x, "a character vector or an R connection")
  }
  as.list(exprs)
}

chr_parse_exprs <- function(x) {
  parsed <- map(x, function(elt) as.list(chr_parse(elt)))

  nms <- names(parsed)
  parsed <- unname(parsed)

  if (!is_null(nms)) {
    nms <- list_c(map2(parsed, nms, rep_along))
  }
  if (length(parsed)) {
    parsed <- list_c(parsed)
  }

  set_names(parsed, nms)
}

chr_parse <- function(x) {
  # Never keep sources, because they get dropped anyways when combining
  # multiple expressions together, and keeping them here is very slow
  parse(text = x, keep.source = FALSE)
}

#' @rdname parse_expr
#' @param env The environment for the quosures. The [global
#'   environment][global_env] (the default) may be the right choice
#'   when you are parsing external user inputs. You might also want to
#'   evaluate the R code in an isolated context (perhaps a child of
#'   the global environment or of the [base environment][base_env]).
#' @export
parse_quo <- function(x, env) {
  check_required(env)
  new_quosure(parse_expr(x), as_environment(env))
}
#' @rdname parse_expr
#' @export
parse_quos <- function(x, env) {
  check_required(env)
  out <- map(parse_exprs(x), new_quosure, env = as_environment(env))
  new_quosures(out)
}
