# can check downstream versions

    Code
      (expect_warning({
        expect_false(.rlang_downstream_check(pkg = "rlang", pkg_ver = "0.5.0", deps = bad_deps,
          info = "Consequences.", env = env(checked = FALSE)))
        NULL
      }))
    Output
      <warning/rlang_warning>
      Warning:
      The package `utils` (>= 100.10) is required as of rlang 0.5.0.

