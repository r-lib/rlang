local_options(rlib_message_verbosity = "quiet")

test_that("unique_names() handles unnamed vectors", {
  expect_equal(chr_as_unique_names(names2(1:3)), c("...1", "...2", "...3"))
})

test_that("chr_as_unique_names() is a no-op when no repairs are needed", {
  x <- c("x", "y")
  out <- chr_as_unique_names(x)
  expect_true(is_reference(out, x))
  expect_equal(out, c("x", "y"))
})

test_that("chr_as_unique_names() eliminates emptiness and duplication", {
  x <- c("", "x", "y", "x")
  expect_equal(chr_as_unique_names(x), c("...1", "x...2", "y", "x...4"))
})

test_that("chr_as_unique_names(): solo empty or NA gets suffix", {
  expect_equal(chr_as_unique_names(""), "...1")
  expect_equal(chr_as_unique_names(NA_character_), "...1")
})

test_that("chr_as_unique_names() treats ellipsis like empty string", {
  expect_equal(chr_as_unique_names("..."), chr_as_unique_names(""))
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
  expect_equal(chr_as_unique_names("..2"), chr_as_unique_names("...5"))
})

test_that("chr_as_unique_names() strips positional suffixes, re-applies as needed", {
  x <- c("...20", "a...1", "b", "", "a...2...34")
  expect_equal(chr_as_unique_names(x), c("...1", "a...2", "b", "...4", "a...5"))

  expect_equal(chr_as_unique_names("a...1"), "a")
  expect_equal(chr_as_unique_names(c("a...2", "a")), c("a...1", "a...2"))
  expect_equal(chr_as_unique_names(c("a...3", "a", "a")), c("a...1", "a...2", "a...3"))
  expect_equal(chr_as_unique_names(c("a...2", "a", "a")), c("a...1", "a...2", "a...3"))
  expect_equal(chr_as_unique_names(c("a...2", "a...2", "a...2")), c("a...1", "a...2", "a...3"))
})

test_that("chr_as_unique_names() is idempotent", {
  x <- c("...20", "a...1", "b", "", "a...2")
  expect_equal(chr_as_unique_names(!!x), chr_as_unique_names(chr_as_unique_names(!!x)))
})

test_that("unique-ification has an 'algebraic'-y property", {
  x <- c("...20", "a...1", "b", "", "a...2", "d")
  y <- c("", "a...3", "b", "...3", "e")

  ## Fix names on each, catenate, fix the whole
  z1 <- chr_as_unique_names(c(chr_as_unique_names(x), chr_as_unique_names(y)))

  ## Fix names on x, catenate, fix the whole
  z2 <- chr_as_unique_names(c(chr_as_unique_names(x), y))

  ## Fix names on y, catenate, fix the whole
  z3 <- chr_as_unique_names(c(x, chr_as_unique_names(y)))

  ## Catenate, fix the whole
  z4 <- chr_as_unique_names(c(x, y))

  expect_equal(z1, z2)
  expect_equal(z1, z3)
  expect_equal(z1, z4)
})

test_that("chr_as_unique_names() are verbose or silent", {
  local_options(rlib_message_verbosity = "default")
  expect_message(chr_as_unique_names(c("", "")), "-> `...1`", fixed = TRUE)
  expect_message(regexp = NA, chr_as_unique_names(c("", ""), quiet = TRUE))
})

test_that("names with only duplicates are repaired", {
  expect_equal(chr_as_unique_names(c("x", "x")), c("x...1", "x...2"))
})

test_that("chr_as_unique_names() handles encodings", {
  x <- unname(unlist(encodings()[c("utf8", "latin1")]))
  out <- chr_as_unique_names(x)
  expect_equal(out, paste0(rep(x[[1]], 2), "...", 1:2))
  expect_equal(Encoding(out), c("UTF-8", "UTF-8"))
})
