context("weakref")

test_that("weakref with key and no value allows key to be GC'd", {
  # This is the case when a weakref is used like a weak pointer (with no
  # value).
  k <- env()
  w_finalized <- FALSE
  w <- new_weakref(key = k, finalizer = function(e) { message("w finalized"); w_finalized <<- TRUE })

  expect_identical(wref_key(w), k)
  expect_identical(wref_value(w), NULL)
  expect_false(w_finalized)

  rm(k)
  gc()
  expect_identical(wref_key(w), NULL)
  expect_true(w_finalized)
})


test_that("key keeps value alive", {
  # Key and value: key keeps value alive
  k <- env()
  k_finalized <- FALSE
  reg.finalizer(k, function(e) { k_finalized <<- TRUE })

  v <- env()
  v$x <- "hello"
  v_finalized <- FALSE
  reg.finalizer(v, function(e) { v_finalized <<- TRUE })

  w_finalized <- FALSE
  w <- new_weakref(
    key = k,
    value = v,
    finalizer = function(e) { w_finalized <<- TRUE }
  )
  expect_identical(wref_key(w), k)
  expect_identical(wref_value(w), v)

  # As long as the key is reachable, the value stays alive.
  rm(v)
  gc()
  expect_false(v_finalized)
  expect_identical(wref_value(w)$x, "hello")

  # Even if the weakref object is unreachable, it still exists, so as long as the key is
  # reachable, it keeps the value alive.
  rm(w)
  gc()
  expect_false(v_finalized)
  expect_false(w_finalized)

  # When the key becomes unreachable, that allows the weakref and value to be
  # GC'd.
  rm(k)
  gc()
  expect_true(k_finalized)
  expect_true(v_finalized)
  expect_true(w_finalized)
})
