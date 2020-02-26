context("operators")

test_that("%|% returns default value", {
  lgl <- c(TRUE, TRUE, NA, FALSE) %|% FALSE
  expect_identical(lgl, c(TRUE, TRUE, FALSE, FALSE))

  int <- c(1L, 2L, NA, 4L) %|% 3L
  expect_identical(int, 1:4)

  dbl <- c(1, 2, NA, 4) %|% 3
  expect_identical(dbl, as.double(1:4))

  chr <- c("1", "2", NA, "4") %|% "3"
  expect_identical(chr, as.character(1:4))

  cpx <- c(1i, 2i, NA, 4i) %|% 3i
  expect_equal(cpx, c(1i, 2i, 3i, 4i))
})

test_that("%|% also works when y is of same length as x", {
  lgl <- c(TRUE, TRUE, NA, FALSE) %|% c(TRUE, TRUE, FALSE, TRUE)
  expect_identical(lgl, c(TRUE, TRUE, FALSE, FALSE))

  int <- c(1L, 2L, NA, 4L) %|% c(10L, 11L, 12L, 13L)
  expect_identical(int, c(1L, 2L, 12L, 4L))

  dbl <- c(1, 2, NA, 4) %|% c(10, 11, 12, 13)
  expect_identical(dbl, c(1, 2, 12, 4))

  chr <- c("1", "2", NA, "4") %|% c("10", "11", "12", "13")
  expect_identical(chr, c("1", "2", "12", "4"))

  cpx <- c(1i, 2i, NA, 4i) %|% c(10i, 11i, 12i, 13i)
  expect_equal(cpx, c(1i, 2i, 12i, 4i))
})

test_that("%|% fails on non-atomic original values", {
  verify_errors({
    expect_error(call("fn") %|% 1)
  })
})

test_that("%|% fails with wrong types", {
  verify_errors({
    expect_error(c(1L, NA) %|% 2)
    expect_error(c(1, NA) %|% "")
    expect_error(c(1, NA) %|% call("fn"))
  })
})

test_that("%|% fails with wrong length", {
  verify_errors({
    expect_error(c(1L, NA) %|% 1:3)
    expect_error(1:10 %|% 1:4)
    expect_error(1L %|% 1:4)
  })
})

test_that("%|% fails with intelligent errors", {
  verify_output(test_path("test-operators-replace-na.txt"), {
    "# %|% fails on non-atomic original values"
    call("fn") %|% 1

    "# %|% fails with wrong types"
    c(1L, NA) %|% 2
    c(1, NA) %|% ""
    c(1, NA) %|% call("fn")

    "# %|% fails with wrong length"
    c(1L, NA) %|% 1:3
    1:10 %|% 1:4
    1L %|% 1:4
  })
})

test_that("%@% returns attribute", {
  expect_identical(mtcars %@% row.names, row.names(mtcars))
  expect_identical(mtcars %@% "row.names", row.names(mtcars))
  expect_null(mtcars %@% "row")
})

test_that("%@% has replacement version", {
  x <- structure(list(), foo = "bar")
  x %@% foo <- NULL
  x %@% baz <- "quux"
  expect_identical(x, structure(list(), baz = "quux"))
})

test_that("new_definition() returns new `:=` call", {
  def <- "foo" ~ "bar"
  node_poke_car(def, quote(`:=`))
  expect_identical(new_definition("foo", "bar"), def)
})

test_that("%@% works with S4 objects (#207)", {
  .Person <- setClass("Person", slots = c(name = "character", species = "character"))
  fievel <- .Person(name = "Fievel", species = "mouse")

  expect_identical(fievel %@% name, "Fievel")
  expect_identical(fievel %@% "species", "mouse")

  fievel %@% name <- "Bernard"
  fievel %@% "species" <- "MOUSE"
  expect_identical(fievel@name, "Bernard")
  expect_identical(fievel@species, "MOUSE")
})
