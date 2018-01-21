
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

expect_no_error <- function(...) {
  expect_error(regexp = NA, ...)
}
expect_no_error_ <- function(object, ...) {
  expect_error(object, regexp = NA, ...)
}

with_verbose_retirement <- function(expr) {
  with_options(lifecycle_force_verbose_retirement = TRUE, expr)
}
with_non_verbose_retirement <- function(expr) {
  with_options(lifecycle_force_verbose_retirement = NULL, expr)
}

# This is set automatically with newer testthat versions. However it
# is easier to develop rlang with the older testthat for now because
# of the dangling pointers after a load_all().
options(lifecycle_force_verbose_retirement = TRUE)
