r_parse_eval <- function(x, env = caller_env()) {
  .Call(ffi_test_parse_eval, x, env)
}

nms_are_duplicated <- function(nms, from_last = FALSE) {
  .Call(ffi_nms_are_duplicated, nms, from_last)
}

r_lgl_sum <- function(x, na_true) {
  stopifnot(is_logical(x), is_bool(na_true))
  .Call(ffi_test_lgl_sum, x, na_true)
}

r_lgl_which <- function(x, na_propagate) {
  stopifnot(is_logical(x), is_bool(na_propagate))
  .Call(ffi_test_lgl_which, x, na_propagate)
}

r_obj_encode_utf8 <- function(x) {
  .Call(ffi_test_obj_encode_utf8, x)
}
test_encodings <- function() {
  string <- "\u00B0C"

  utf8 <- iconv(string, from = Encoding(string), to = "UTF-8")
  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)
  latin1 <- iconv(string, from = Encoding(string), to = "latin1")

  list(utf8 = utf8, unknown = unknown, latin1 = latin1)
}
expect_utf8_encoded <- function(object) {
  expect_identical(Encoding(object), rep("UTF-8", length(object)))
}
