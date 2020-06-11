
local_exit <- function(expr, frame = caller_env()) {
  expr <- enexpr(expr)
  out <- expr

  # We are at top-level when only one frame refers to the global environment
  if (is_reference(frame, global_env())) {
    is_global_frame <- sys.parents() == 0
    if (sum(is_global_frame) == 1) {
      abort("Can't add an exit event at top-level")
    }
  }

  exit_exprs <- eval_bare(call2(sys.on.exit), frame)
  rlang_exit <- detect_rlang_exit(exit_exprs)

  if (is_null(rlang_exit)) {
    first_expr <- new_node(expr)
    exit_marker_env <- new_environment(list(
      exprs = first_expr,
      frame = frame
    ))

    expr <- call2(eval_rlang_exit, exit_marker_env)

    # Add marker to exit expressions
    expr <- call2("{", "_rlang_exit", expr)

    # Inline everything so the call will succeed in any environment
    expr <- call2(on.exit, expr, add = TRUE)

    eval_bare(expr, frame)
  } else {
    # Insert new exit in place in LIFO order
    exit_marker_env <- node_cadr(rlang_exit)
    exit_marker_env$exprs <- new_node(expr, exit_marker_env$exprs)
  }

  invisible(out)
}

eval_rlang_exit <- function(marker_env) {
  exits <- new_call(`{`, marker_env$exprs)
  eval_bare(exits, marker_env$frame)
}

# This is set up to recurse across `{` calls for robustness
# because sys.on.exit() rewraps in `{`
detect_rlang_exit <- function(expr) {
  if (!is_call(expr, "{")) {
    return(NULL)
  }

  expr <- node_cdr(expr)

  while (!is_null(expr)) {
    exit <- node_car(expr)
    expr <- node_cdr(expr)

    if (is_call(exit, "{")) {
      exit <- detect_rlang_exit(exit)
      if (is_null(exit)) {
        next
      } else {
        return(exit)
      }
    }

    if (identical(exit, "_rlang_exit")) {
      return(node_car(expr))
    }

    next
  }

  NULL
}
