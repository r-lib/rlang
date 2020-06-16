
local_exit <- function(expr, frame = caller_env()) {
  # We are at top-level when only one frame refers to the global environment
  if (is_reference(frame, global_env())) {
    is_global_frame <- sys.parents() == 0
    if (sum(is_global_frame) == 1) {
      abort("Can't add an exit event at top-level.")
    }
  }

  expr <- enexpr(expr)
  blast(defer(!!expr, envir = frame))

  invisible(expr)
}
