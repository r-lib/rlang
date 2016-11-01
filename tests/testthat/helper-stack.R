
fixup_ctxts <- function(x) {
  cur_pos <- ctxt_pos() - 1
  x[seq(cur_pos + 1, length(x))]
}
fixup_calls <- function(x) {
  cur_pos <- call_pos() - 1
  x[seq(n+1, length(x))]
}

fixup_ctxt_pos <- function(x) {
  x - (ctxt_pos() - 1)
}
fixup_call_pos <- function(x) {
  x - (call_pos() - 1)
}

fixup_call_trail <- function(trail) {
  ctxt_callers <- ctxt_stack_callers()
  cur_trail <- make_trail(ctxt_callers)
  cur_pos <- cur_trail[1]

  indices <- seq(1, length(trail) - length(cur_trail))
  trail <- trail[indices]
  trail - cur_pos
}
