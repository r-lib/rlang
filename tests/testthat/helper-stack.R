
fixup_ctxts <- function(x) {
  cur_pos <- sys.nframe() - 1
  x[seq(cur_pos + 1, length(x))]
}
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
  ctxt_callers <- ctxt_stack_callers()
  cur_trail <- make_trail(ctxt_callers)
  cur_pos <- cur_trail[1]

  indices <- seq(1, length(trail) - length(cur_trail))
  trail <- trail[indices]
  trail - cur_pos
}
