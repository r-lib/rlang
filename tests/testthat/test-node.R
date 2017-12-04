context("node")

test_that("node_tree_clone() clones all nodes", {
  x <- pairlist(1, pairlist(2))
  clone <- node_tree_clone(x)

  # Outer vector
  expect_false(sxp_address(x) == sxp_address(clone))

  # Outer node list
  expect_true(sxp_address(node_car(x)) == sxp_address(node_car(clone)))

  cdr <- node_cdr(x)
  clone_cdr <- node_cdr(clone)
  expect_false(sxp_address(cdr) == sxp_address(clone_cdr))

  # Inner node list
  cadr <- node_car(cdr)
  clone_cadr <- node_car(clone_cdr)
  expect_false(sxp_address(cadr) == sxp_address(clone_cadr))

  # Inner vector
  caadr <- node_car(cadr)
  clone_caadr <- node_car(clone_cadr)
  expect_true(sxp_address(caadr) == sxp_address(clone_caadr))
})
