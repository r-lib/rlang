
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
  cur_trail <- call_stack_trail()
  cur_trail <- drop_first(cur_trail)
  cur_pos <- cur_trail[1]

  indices <- seq(1, length(trail) - length(cur_trail))
  trail <- trail[indices]
  trail - cur_pos
}
