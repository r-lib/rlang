
nms_are_duplicated <- function(nms, from_last = FALSE) {
  .Call(rlang_nms_are_duplicated, nms, from_last)
}
