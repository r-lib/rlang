# vec_ptype2() implements base coercions

    Code
      vec_ptype2(lgl(), chr())
    Error <rlang_error>
      Can't combine types <logical> and <character>.

---

    Code
      vec_ptype2(factor("a"), lgl())
    Error <rlang_error>
      Unimplemented class <factor>.

# vec_ptype_common() works

    Code
      vec_ptype_common(list(lgl(), dbl(), ""))
    Error <rlang_error>
      Can't combine types <double> and <character>.

# lossy casts throw

    Code
      vec_cast(1.5, 2L)
    Error <rlang_error>
      Can't convert <double> to <integer>.

