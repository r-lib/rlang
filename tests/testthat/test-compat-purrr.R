test_that("map functions work", {
  expect_equal(map(1:2, length), list(1, 1))
  expect_equal(walk(1:2, length), 1:2)

  expect_equal(map_lgl(0:1, as.logical), c(FALSE, TRUE))
  expect_identical(map_int(1:2, as.integer), 1:2)
  expect_identical(map_dbl(1:2, as.integer), c(1, 2))
  expect_equal(map_chr(1:2, as.character), c("1", "2"))
})

test_that("map2 functions work", {
  expect_equal(map2(1, 1:2, `+`), list(2, 3))

  expect_equal(map2_lgl(1, 1:2, `==`), c(TRUE, FALSE))
  expect_identical(map2_int(1, 1:2, `+`), c(2L, 3L))
  expect_identical(map2_dbl(1, 1:2, `+`), c(2, 3))
  expect_equal(map2_chr(1, 1:2, paste0), c("11", "12"))
})

test_that("imap works", {
  expect_equal(imap(c("a", "b"), list), list(list("a", 1L), list("b", 2L)))
  expect_equal(imap(c(x = "a", y = "b"), list), list(x = list("a", "x"), y = list("b", "y")))
  expect_equal(imap(c(x = "a", "b"), list), list(x = list("a", "x"), list("b", "")))
})

test_that("pmap works", {
  expect_equal(pmap(list(1, 1:2), paste0), list("11", "12"))
})

test_that("predicate based functions work", {
  x <- list(1, 2)
  expect_equal(keep(x, ~ sum(.x) > 1), list(2))
  expect_equal(discard(x, ~ sum(.x) > 1), list(1))
  expect_equal(map_if(x, ~ sum(.x) > 1, ~ 10), list(1, 10))

  expect_true(every(x, ~ .x > 0))
  expect_false(every(x, ~ .x < 0))
  expect_true(some(x, ~ .x > 0))
  expect_false(some(x, ~ .x < 0))

  expect_equal(detect(x, ~ .x > 1), 2)
  expect_identical(detect_index(x, ~ .x > 0), 1L)
})

test_that("reduce/accumulate work", {
  x <- 1:3
  expect_equal(reduce(x, `+`), 6)
  expect_equal(reduce_right(x, `+`), 6)

  expect_equal(accumulate(x, `+`), c(1, 3, 6))
  expect_equal(accumulate_right(x, `+`), c(6, 5, 3))
})

test_that("transpose() handles empty list", {
  expect_equal(transpose(list()), list())
})
