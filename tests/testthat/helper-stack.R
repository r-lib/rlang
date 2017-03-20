
fixup_calls <- function(x) {
  cur_pos <- sys.nframe() - 1
  x[seq(n+1, length(x))]
}

fixup_ctxt_depth <- function(x) {
  x - (sys.nframe() - 1)
}
fixup_call_depth <- function(x) {
  x - (call_depth() - 1)
}

fixup_call_trail <- function(trail) {
  eval_callers <- ctxt_stack_callers()
  cur_trail <- trail_make(eval_callers)
  cur_pos <- eval_callers[1]

  indices <- seq(1, length(trail) - length(cur_trail))
  trail <- trail[indices]
  trail - cur_pos
}
