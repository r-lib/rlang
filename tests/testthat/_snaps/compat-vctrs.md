# vec_ptype2() works

    Code
      vec_ptype2(lgl(), chr())
    Error <rlang_error>
      Can't combine types <logical> and <character>.

---

    Code
      vec_ptype2(factor("a"), lgl())
    Error <rlang_error>
      Unimplemented class <factor>.

