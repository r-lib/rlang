
expect_condition <- function(expr,
                             class = NULL,
                             regex = NULL,
                             info = NULL,
                             label = NULL) {
  object <- tryCatch(expr, condition = identity)

  if (is_na(class)) {
    expect_false(inherits(object, "condition"), info = info, label = label)
    return(invisible(object))
  }

  expect_is(object, "condition", info = info, label = label)

  if (!is_null(class)) {
    expect_is(object, class, info = info, label = label)
  }
  if (!is_null(regex)) {
    expect_match(object$message, regex, class, info = info, label = label)
  }

  invisible(object)
}
