local_options(rlib_message_verbosity = "quiet")

test_that("unique_names() handles unnamed vectors", {
  expect_equal(names_as_unique(names2(1:3)), c("...1", "...2", "...3"))
})

test_that("names_as_unique() is a no-op when no repairs are needed", {
  x <- c("x", "y")
  out <- names_as_unique(x)
  expect_true(is_reference(out, x))
  expect_equal(out, c("x", "y"))
})

test_that("names_as_unique() eliminates emptiness and duplication", {
  expect_equal(
    names_as_unique(c("", "x", "y", "x")),
    c("...1", "x...2", "y", "x...4")
  )
  expect_equal(
    names_as_unique(c("1", "foo", "1")),
    c("1...1", "foo", "1...3")
  )
})

test_that("names_as_unique(): solo empty or NA gets suffix", {
  expect_equal(names_as_unique(""), "...1")
  expect_equal(names_as_unique(NA_character_), "...1")
})

test_that("names_as_unique() treats ellipsis like empty string", {
  expect_equal(names_as_unique("..."), names_as_unique(""))
})

test_that("two_three_dots() does its job and no more", {
  two_to_three_dots <- function(names) {
    sub("(^[.][.][1-9][0-9]*$)", ".\\1", names)
  }

  x <- c(".", ".1", "...1", "..1a")
  expect_equal(two_to_three_dots(x), x)

  expect_equal(two_to_three_dots(c("..1", "..22")), c("...1", "...22"))
})

test_that("two dots then number treated like three dots then number", {
  expect_equal(names_as_unique("..2"), names_as_unique("...5"))
})

test_that("names_as_unique() strips positional suffixes, re-applies as needed", {
  x <- c("...20", "a...1", "b", "", "a...2...34")
  expect_equal(names_as_unique(x), c("...1", "a...2", "b", "...4", "a...5"))

  expect_equal(names_as_unique("a...1"), "a")
  expect_equal(names_as_unique(c("a...2", "a")), c("a...1", "a...2"))
  expect_equal(
    names_as_unique(c("a...3", "a", "a")),
    c("a...1", "a...2", "a...3")
  )
  expect_equal(
    names_as_unique(c("a...2", "a", "a")),
    c("a...1", "a...2", "a...3")
  )
  expect_equal(
    names_as_unique(c("a...2", "a...2", "a...2")),
    c("a...1", "a...2", "a...3")
  )
})

test_that("names_as_unique() is idempotent", {
  x <- c("...20", "a...1", "b", "", "a...2")
  expect_equal(names_as_unique(!!x), names_as_unique(names_as_unique(!!x)))
})

test_that("unique-ification has an 'algebraic'-y property", {
  x <- c("...20", "a...1", "b", "", "a...2", "d")
  y <- c("", "a...3", "b", "...3", "e")

  ## Fix names on each, catenate, fix the whole
  z1 <- names_as_unique(c(names_as_unique(x), names_as_unique(y)))

  ## Fix names on x, catenate, fix the whole
  z2 <- names_as_unique(c(names_as_unique(x), y))

  ## Fix names on y, catenate, fix the whole
  z3 <- names_as_unique(c(x, names_as_unique(y)))

  ## Catenate, fix the whole
  z4 <- names_as_unique(c(x, y))

  expect_equal(z1, z2)
  expect_equal(z1, z3)
  expect_equal(z1, z4)
})

test_that("names_as_unique() are verbose or silent", {
  local_options(rlib_message_verbosity = "default")
  expect_message(names_as_unique(c("", "")), "-> `...1`", fixed = TRUE)
  expect_message(regexp = NA, names_as_unique(c("", ""), quiet = TRUE))
})

test_that("names with only duplicates are repaired", {
  expect_equal(names_as_unique(c("x", "x")), c("x...1", "x...2"))
})

test_that("names_as_unique() handles encodings", {
  x <- unname(unlist(encodings()[c("utf8", "latin1")]))
  out <- names_as_unique(x)
  expect_equal(out, paste0(rep(x[[1]], 2), "...", 1:2))
  expect_equal(Encoding(out), c("UTF-8", "UTF-8"))
})

test_that("names_inform_repair() signals classed messages", {
  local_options(rlib_message_verbosity = "default")
  expect_message(
    names_inform_repair("x", "y"),
    class = "rlib_message_name_repair"
  )
})

test_that("names_inform_repair() can be silenced by `rlib_name_repair_verbosity`", {
  local_options(
    rlib_message_verbosity = "default",
    rlib_name_repair_verbosity = "quiet"
  )
  expect_message(names_inform_repair("x", "y"), NA)
})

test_that("`rlib_name_repair_verbosity` is validated", {
  local_options(rlib_name_repair_verbosity = 1)
  expect_error(peek_name_repair_verbosity())

  local_options(rlib_name_repair_verbosity = "qu")
  expect_error(peek_name_repair_verbosity())
})
