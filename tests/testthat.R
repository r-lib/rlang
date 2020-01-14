# Workaround for loadNamespace() failure on R 3.2
requireNamespace("rlang")

library("testthat")
library("rlang")

test_check("rlang")
