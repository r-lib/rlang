context("node")

test_that("node() creates a pairlist node", {
  x <- new_node("foo", "bar")
  expect_is(x, "pairlist")
  expect_identical(node_car(x), "foo")
  expect_identical(node_cdr(x), "bar")
})

test_that("node getters and pokers work", {
  A <- as.pairlist(c(a = "a", b = "b"))
  B <- as.pairlist(c(A = "A", B = "B"))
  x <- pairlist(foo = A, bar = B, baz = "baz")

  expect_identical(node_car(x), A)
  expect_identical(node_cdr(x), pairlist(bar = B, baz = "baz"))
  expect_identical(node_caar(x), "a")
  expect_identical(node_cadr(x), B)
  expect_identical(node_cdar(x), pairlist(b = "b"))
  expect_identical(node_cddr(x), pairlist(baz = "baz"))
  expect_identical(node_tag(x), sym("foo"))

  node_poke_car(x, B)
  expect_identical(node_car(x), B)

  node_poke_cdr(x, pairlist(foo = A))
  expect_identical(node_cdr(x), pairlist(foo = A))

  node_poke_cdar(x, "cdar")
  expect_identical(node_cdar(x), "cdar")

  node_poke_caar(x, "caar")
  expect_identical(node_caar(x), "caar")

  node_poke_cadr(x, "cadr")
  expect_identical(node_cadr(x), "cadr")

  node_poke_cddr(x, "cddr")
  expect_identical(node_cddr(x), "cddr")

  node_poke_tag(x, sym("tag"))
  expect_identical(node_tag(x), sym("tag"))
})

test_that("node_tree_clone() clones all nodes", {
  x <- pairlist(1, pairlist(2))
  clone <- node_tree_clone(x)

  # Outer vector
  expect_false(sexp_address(x) == sexp_address(clone))

  # Outer node list
  expect_true(sexp_address(node_car(x)) == sexp_address(node_car(clone)))

  cdr <- node_cdr(x)
  clone_cdr <- node_cdr(clone)
  expect_false(sexp_address(cdr) == sexp_address(clone_cdr))

  # Inner node list
  cadr <- node_car(cdr)
  clone_cadr <- node_car(clone_cdr)
  expect_false(sexp_address(cadr) == sexp_address(clone_cadr))

  # Inner vector
  caadr <- node_car(cadr)
  clone_caadr <- node_car(clone_cadr)
  expect_true(sexp_address(caadr) == sexp_address(clone_caadr))
})

test_that("as_pairlist() converts to pairlist", {
  expect_identical(as_pairlist(letters), as.pairlist(letters))
  expect_error(as_pairlist(quote(foo)), "Can't convert a symbol to a pairlist node")

  expect_identical(as_pairlist(NULL), NULL)

  x <- pairlist(1, 2)
  expect_identical(as_pairlist(x), x)
})

test_that("pairlist predicates detect pairlists", {
  node <- new_node(NULL)
  call <- quote(foo(bar))

  expect_true(is_pairlist(node))

  expect_true(is_node(node))
  expect_true(is_node(call))

  expect_true(is_node_list(node))
  expect_true(is_node_list(NULL))
})

test_that("pairlist2() converts to pairlist", {
  expect_identical_(pairlist2(1, !!!c(2, 3), 4), pairlist(1, 2, 3, 4))
  expect_identical_(pairlist2(1, !!!mtcars[1:2], 4), pairlist(1, mpg = mtcars$mpg, cyl = mtcars$cyl, 4))

  scoped_bindings(.env = global_env(),
    as.list.rlang_foobar = function(x) list("foo", "bar")
  )
  foobar <- structure(NA, class = "rlang_foobar")
  expect_identical_(pairlist2(1, !!!foobar, 4), pairlist(1, "foo", "bar", 4))
})

test_that("pairlist2() duplicates spliced pairlists", {
  x <- pairlist("foo", "bar")
  pairlist2(1, !!!x, 4)
  expect_identical(x, pairlist("foo", "bar"))
})

test_that("pairlist2() preserves empty arguments", {
  expect_identical(pairlist2(1, x = , , 4), pairlist(1, x = missing_arg(), missing_arg(), 4))
})

test_that("pairlist2() supports splice boxes", {
  expect_identical(pairlist2(1, splice(list("foo", "bar")), 4), pairlist(1, "foo", "bar", 4))
})

test_that("pairlist2() supports empty spliced vectors", {
  expect_null_(pairlist2(!!!NULL))
  expect_null_(pairlist2(!!!lgl()))
  expect_null_(pairlist2(!!!list()))
})
