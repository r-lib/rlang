# vec_ptype2() implements base coercions

    Code
      vec_ptype2(lgl(), chr())
    Error <simpleError>
      Can't combine types <logical> and <character>.

---

    Code
      vec_ptype2(factor("a"), lgl())
    Error <simpleError>
      Unimplemented class <factor>.

# vec_ptype_common() works

    Code
      vec_ptype_common(list(lgl(), dbl(), ""))
    Error <simpleError>
      Can't combine types <double> and <character>.

# lossy casts throw

    Code
      vec_cast(1.5, 2L)
    Error <simpleError>
      Can't convert <double> to <integer>.

