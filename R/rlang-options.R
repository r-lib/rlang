#' Global options for rlang
#'
#' @description
#' rlang has several options which may be set globally to control
#' behavior. A brief description of each is given here. If any functions
#' are referenced, refer to their documentation for additional details.
#'
#' * `rlang_interactive`: A logical value used by [is_interactive()]. This
#'    can be set to `TRUE` to test interactive behavior in unit tests,
#'    for example.
#'
#' * `rlang_backtrace_on_error`: A character string which controls whether
#'    backtraces are displayed with error messages, and the level of
#'    detail they print. See [rlang_backtrace_on_error] for the possible option values.
#'
#' * `rlang_trace_format_srcrefs`: A logical value used to control whether
#'    srcrefs are printed as part of the backtrace.
#'
#' * `rlang_trace_top_env`: An environment which will be treated as the
#'    top-level environment when printing traces. See [trace_back()]
#'    for examples.
#' @name faq-options
NULL
