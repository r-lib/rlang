test_that("hashes are stable across R versions", {
  skip_if_big_endian()

  expect_snapshot({
    # Scalars
    hash(NULL)
    hash(1)
    hash(1L)
    hash(TRUE)
    hash("a")

    # NA variants
    hash(NA_real_)
    hash(NaN)
    hash(NA_integer_)
    hash(NA)
    hash(NA_character_)

    # Vectors
    hash(1:5 + 0L)

    # Empty vectors
    hash(raw(0))
    hash(list())

    # Symbols
    hash(quote(x))
    hash(quote(foo))
  })
})


test_that("different objects produce different hashes", {
  skip_if_big_endian()

  expect_snapshot({
    hash(1L)
    hash(2L)

    hash("a")
    hash("b")

    hash(1:3)
    hash(4:6)

    hash(TRUE)
    hash(FALSE)

    hash(list(1))
    hash(list(2))

    hash(quote(x))
    hash(quote(y))
  })
})

test_that("different types produce different hashes", {
  expect_false(hash(1L) == hash(1))
  expect_false(hash(TRUE) == hash(1L))
  expect_false(hash(1L) == hash("1"))
  expect_false(hash(list()) == hash(NULL))
  expect_false(hash(integer()) == hash(double()))
  expect_false(hash(integer()) == hash(logical()))
  expect_false(hash(integer()) == hash(raw()))
})

test_that("NA vs NaN produce different hashes", {
  expect_false(hash(NA_real_) == hash(NaN))
  expect_false(hash(NA_integer_) == hash(0L))
  expect_false(hash(NA) == hash(TRUE))
  expect_false(hash(NA_character_) == hash(""))
})

test_that("+0 and -0 produce the same hash", {
  expect_identical(hash(0), hash(-0))
})

test_that("complex NA/NaN/zero normalisation works", {
  expect_identical(
    hash(complex(real = 0, imaginary = 0)),
    hash(complex(real = -0, imaginary = -0))
  )
  expect_identical(
    hash(complex(real = 0, imaginary = -0)),
    hash(complex(real = -0, imaginary = 0))
  )
  expect_false(hash(complex(real = NA)) == hash(complex(real = NaN)))
  expect_false(
    hash(complex(real = NA, imaginary = 1)) ==
      hash(complex(real = NaN, imaginary = 1))
  )
  # Components are positional
  expect_false(
    hash(complex(real = 1, imaginary = 2)) ==
      hash(complex(real = 2, imaginary = 1))
  )
})

test_that("attribute names matter", {
  expect_false(hash(structure(1, foo = 1)) == hash(structure(1, bar = 1)))
})

test_that("attribute values matter", {
  expect_false(hash(structure(1, x = 1)) == hash(structure(1, x = 2)))
})

test_that("NULL hashes consistently", {
  h <- hash(NULL)
  expect_identical(h, hash(NULL))
  expect_false(h == hash(list()))
  expect_false(h == hash(list(NULL)))
})

test_that("nested lists produce distinct hashes", {
  expect_false(hash(list(1, 2)) == hash(list(list(1, 2))))
  expect_false(hash(list(1, 2)) == hash(list(1, list(2))))
  expect_false(hash(list(list(1), 2)) == hash(list(1, list(2))))
})

test_that("pairlist tags matter", {
  expect_false(hash(pairlist(a = 1)) == hash(pairlist(b = 1)))
})

test_that("pairlist with attributes differs from longer pairlist (#1681)", {
  # Regression test: without a length prefix on pairlists, the attribute
  # walk bytes are indistinguishable from additional pairlist node bytes
  x <- pairlist(a = 1)
  attr(x, "b") <- 2
  y <- pairlist(a = 1, b = 2)
  expect_false(hash(x) == hash(y))
})

test_that("call with attributes differs from call with extra arg (#1681)", {
  x <- quote(f(x))
  attr(x, "foo") <- 1L
  y <- quote(f(x, foo = 1L))
  expect_false(hash(x) == hash(y))
})

test_that("call structure is hashed correctly", {
  expect_identical(hash(quote(f(x))), hash(quote(f(x))))
  expect_false(hash(quote(f(x))) == hash(quote(g(x))))
  expect_false(hash(quote(f(x))) == hash(quote(f(y))))
})

test_that("closures with same body/formals/env hash the same", {
  e <- new.env(parent = baseenv())
  f1 <- local(function(x) x + 1, envir = e)
  f2 <- local(function(x) x + 1, envir = e)
  expect_identical(hash(f1), hash(f2))
})

test_that("closures in different environments hash differently", {
  e1 <- new.env(parent = baseenv())
  e2 <- new.env(parent = baseenv())
  f1 <- local(function(x) x + 1, envir = e1)
  f2 <- local(function(x) x + 1, envir = e2)
  expect_false(hash(f1) == hash(f2))
})

test_that("byte-compiled closures hash identically to uncompiled ones", {
  f <- function(x) x + 1
  g <- compiler::cmpfun(f)
  expect_identical(hash(f), hash(g))
})

test_that("srcrefs are ignored by default for closures", {
  e <- new.env(parent = baseenv())
  with_srcref("f1 <- function(x) x + 1", env = e)
  with_srcref("f2 <- function(x) x + 1", env = e)
  expect_identical(hash(e$f1), hash(e$f2))
  expect_false(hash(e$f1, zap_srcref = FALSE) == hash(e$f2, zap_srcref = FALSE))
})

test_that("srcrefs are ignored by default for quoted function calls", {
  e <- new.env(parent = baseenv())
  with_srcref("q1 <- quote(function(x) x + 1)", env = e)
  with_srcref("q2 <- quote(function(x) x + 1)", env = e)
  # Parser stores srcref as 4th element on `function` calls
  expect_length(e$q1, 4)
  expect_identical(hash(e$q1), hash(e$q2))
  expect_false(hash(e$q1, zap_srcref = FALSE) == hash(e$q2, zap_srcref = FALSE))
})

test_that("srcrefs are ignored by default for calls with srcref attributes", {
  e <- new.env(parent = baseenv())
  with_srcref("b1 <- quote({ 1; 2 })", env = e)
  with_srcref("b2 <- quote({ 1; 2 })", env = e)
  expect_true("srcref" %in% names(attributes(e$b1)))
  expect_identical(hash(e$b1), hash(e$b2))
  expect_false(hash(e$b1, zap_srcref = FALSE) == hash(e$b2, zap_srcref = FALSE))
})

test_that("srcrefs are ignored by default for expression vectors", {
  x1 <- parse(text = "1 + 2; 3 + 4", keep.source = TRUE)
  x2 <- parse(text = "1 + 2; 3 + 4", keep.source = TRUE)
  expect_true("srcref" %in% names(attributes(x1)))
  expect_identical(hash(x1), hash(x2))
  expect_false(hash(x1, zap_srcref = FALSE) == hash(x2, zap_srcref = FALSE))
})

test_that("NA_character_ hashes distinctly from the string \"NA\"", {
  expect_false(hash(NA_character_) == hash("NA"))
  expect_false(hash(NA_character_) == hash(""))
})

test_that("string encoding is part of the hash", {
  utf8 <- "caf\u00e9"
  latin1 <- iconv(utf8, from = "UTF-8", to = "latin1")
  Encoding(latin1) <- "latin1"

  # Same displayed text, different encoding → different hash
  expect_identical(as.character(utf8), as.character(latin1))
  expect_false(hash(utf8) == hash(latin1))
})

test_that("symbols hash by value", {
  expect_identical(hash(quote(x)), hash(as.symbol("x")))
  expect_false(hash(quote(x)) == hash(quote(y)))
})

test_that("environments hash by identity", {
  e <- new.env(parent = emptyenv())
  expect_identical(hash(e), hash(e))
  expect_false(
    hash(new.env(parent = emptyenv())) == hash(new.env(parent = emptyenv()))
  )
})

test_that("external pointers hash by identity", {
  p1 <- hasher_init()
  p2 <- hasher_init()
  expect_identical(hash(p1), hash(p1))
  expect_false(hash(p1) == hash(p2))
})

test_that("S4 objects hash structurally, not by identity", {
  on.exit(removeClass("HashTestS4"))
  setClass("HashTestS4", slots = list(x = "numeric", y = "character"))
  a <- new("HashTestS4", x = 1, y = "hello")
  b <- new("HashTestS4", x = 1, y = "hello")
  c <- new("HashTestS4", x = 2, y = "hello")
  expect_identical(hash(a), hash(b))
  expect_false(hash(a) == hash(c))
})

test_that("resizable vectors hash the same as regular vectors (#1681)", {
  skip_if_not_installed("vctrs")
  expect_identical(hash(vctrs::vec_slice(1:3, 1:3)), hash(1:3))
  expect_identical(hash(vctrs::vec_c(1L, 2L, 3L)), hash(1:3))

  x <- c("a", "b", "c")
  expect_identical(hash(vctrs::vec_slice(x, 1:3)), hash(x))
})

test_that("empty vectors of different types hash distinctly", {
  types <- list(
    logical(),
    integer(),
    double(),
    complex(),
    character(),
    raw(),
    list()
  )
  hashes <- vapply(types, hash, character(1))
  expect_identical(length(unique(hashes)), length(hashes))
})

test_that("named vectors with different names hash differently", {
  expect_false(hash(c(a = 1)) == hash(c(b = 1)))
})

test_that("matrices with different dim hash differently", {
  m1 <- matrix(1:6, nrow = 2)
  m2 <- matrix(1:6, nrow = 3)
  expect_false(hash(m1) == hash(m2))
  expect_identical(hash(m1), hash(matrix(1:6, nrow = 2)))
})

test_that("data frames hash based on content and attributes", {
  df1 <- data.frame(x = 1:3, y = letters[1:3])
  df2 <- data.frame(x = 1:3, y = letters[1:3])
  df3 <- data.frame(x = 1:3, y = letters[4:6])
  expect_identical(hash(df1), hash(df2))
  expect_false(hash(df1) == hash(df3))
})

test_that("hash_file() errors if the file doesn't exist", {
  expect_error(hash_file("foo.ext"))
})

test_that("hash_file() works for 0 length input", {
  expect_identical(hash_file(character()), character())
})

test_that("hash_file() has known fixed value for empty files", {
  skip_if_big_endian()

  path <- withr::local_tempfile()
  file.create(path)

  expect_identical(hash_file(path), "99aa06d3014798d86001c324468d497f")
})

test_that("hash_file() results change as more data is written to the file", {
  path <- withr::local_tempfile()
  file.create(path)

  initial <- hash_file(path)

  saveRDS(1, path)

  expect_true(hash_file(path) != initial)
})

test_that("hash_file()'s internal state is reset between files", {
  path1 <- withr::local_tempfile()
  file.create(path1)
  saveRDS(1, path1)

  path2 <- withr::local_tempfile()
  file.create(path2)
  saveRDS(2, path2)

  hashes <- hash_file(c(path1, path2))

  expect_identical(hashes[[1]], hash_file(path1))
  expect_identical(hashes[[2]], hash_file(path2))
})
